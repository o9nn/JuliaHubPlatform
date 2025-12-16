using Git
# this is substantially derivative of Documenter.jl's usage of git to run gh-pages
abstract type DeployTarget end
Base.@kwdef struct DeployConfig
    branch::String = ""
    repo::String = ""
    subfolder::String = ""
    all_ok::Bool = false
end

@enum AuthenticationMethod SSH HTTPS
authentication_method(::DeployTarget) = SSH

function authenticated_repo_url end
post_status(cfg::Union{DeployTarget,Nothing}; kwargs...) = nothing

function data_key(::DeployTarget)
    return ENV["DATA_KEY"]
end

function deploy_folder end

function currentdir()
    d = Base.source_dir()
    d === nothing ? pwd() : d
end

const NO_KEY_ENV = Dict(
    "DOCUMENTER_KEY" => nothing,
    "DOCUMENTER_KEY_PREVIEWS" => nothing,
)


function deploy(
    root = currentdir(),
    target = "data",
    dirname = "";
    repo   = error("no 'repo' keyword provided."),
    branch = "data",
    deploy_type::DeployTarget = nothing,
    archive = nothing
)
    deploy_config = deploy_folder(deploy_type;
                                    branch=branch,
                                    repo=repo,
                                    dataurl=dirname)
    if !deploy_config.all_ok
        return 
    end
                                
    deploy_branch = deploy_config.branch
    deploy_repo = deploy_config.repo
    deploy_subfolder = deploy_config.subfolder

    cd(root) do 
        sha = try 
            readchomp(`$(git()) rev-parse --short HEAD`)
        catch
            # git rev-parse will throw an error and return code 128 if it is not being
            # run in a git repository, which will make run/readchomp throw an exception.
            # We'll assume that if readchomp fails it is due to this and set the sha
            # variable accordingly.
            "(not-git-repo)"
        end
        @debug "setting up target directory."
        isdir(target) || mkpath(target)
        startswith(realpath(target), realpath(root)) || error("""
        target must be a subdirectory of root, got:
          target: $(realpath(target))
          root: $(realpath(root))
        """)
        @debug "pushing new data to remote: '$deploy_repo:$deploy_branch'."
        mktempdir() do temp
            git_push(
                temp, deploy_repo;
                branch=deploy_branch, dirname=dirname, target=target,
                sha=sha, deploy_type=deploy_type, subfolder=deploy_subfolder, forcepush=false,
                archive=archive
            )
        end
    end
end

function git_push(
    temp, repo;
    branch="gh-pages", dirname="", target="site", sha="",
    forcepush=false, deploy_type, subfolder,
    archive = false
)
dirname = isempty(dirname) ? temp : joinpath(temp, dirname)
isdir(dirname) || mkpath(dirname)

target_dir = abspath(target)

# Generate a closure with common commands for ssh and https
function git_commands(sshconfig=nothing)
    # Setup git.
    run(`$(git()) init`)
    run(`$(git()) config user.name "todo"`)
    run(`$(git()) config user.email "todo@example.com"`)
    if sshconfig !== nothing
        run(`$(git()) config core.sshCommand "ssh -F $(sshconfig)"`)
    end

    # Fetch from remote and checkout the branch.
    run(`$(git()) remote add upstream $upstream`)
    try
        run(`$(git()) fetch upstream`)
    catch e
        @error """
        Git failed to fetch $upstream
        This can be caused by a DATA_KEY variable that is not correctly set up.
        Make sure that the environment variable is properly set up as a Base64-encoded string
        of the SSH private key. You may need to re-generate the keys.
        """
        rethrow(e)
    end

    try
        run(`$(git()) checkout -b $branch upstream/$branch`)
    catch e
        @info """
        Checking out $branch failed, creating a new orphaned branch.
        This usually happens when deploying to a repository for the first time and
        the $branch branch does not exist yet. The fatal error above is expected output
        from Git in this situation.
        """
        @debug "checking out $branch failed with error: $e"
        run(`$(git()) checkout --orphan $branch`)
        run(`$(git()) commit --allow-empty -m "Initial empty commit for data"`)
    end

    # Copy docs to `subfolder` directory.
    deploy_dir = subfolder === nothing ? dirname : joinpath(dirname, subfolder)
    gitrm_copy(target_dir, deploy_dir)

    # Add, commit, and push the docs to the remote.
    run(`$(git()) add -A -- ':!.data-identity-file.tmp' ':!**/.data-identity-file.tmp'`)
    if !success(`$(git()) diff --cached --exit-code`)
        if !isnothing(archive)
            run(`$(git()) commit -m "build based on $sha"`)
            @info "Skipping push and writing repository to an archive" archive
            run(`$(git()) archive -o $(archive) HEAD`)
        elseif forcepush
            run(`$(git()) commit --amend --date=now -m "build based on $sha"`)
            run(`$(git()) push -fq upstream HEAD:$branch`)
        else
            run(`$(git()) commit -m "build based on $sha"`)
            run(`$(git()) push -q upstream HEAD:$branch`)
        end
    else
        @info "new data identical to the old -- not committing nor pushing."
    end
end
    # The upstream URL to which we push new content authenticated with token
    upstream = authenticated_repo_url(deploy_type)
    try
        cd(() -> withenv(git_commands, NO_KEY_ENV...), temp)
        post_status(deploy_type; repo=repo, type="success", subfolder=subfolder)
    catch e
        @error "Failed to push:" exception=(e, catch_backtrace())
        post_status(deploy_type; repo=repo, type="error")
        rethrow(e)
    end
end

"""
    gitrm_copy(src, dst)

Uses `git rm -r` to remove `dst` and then copies `src` to `dst`. Assumes that the working
directory is within the git repository of `dst` is when the function is called.

This is to get around [#507](https://github.com/JuliaDocs/Documenter.jl/issues/507) on
filesystems that are case-insensitive (e.g. on OS X, Windows). Without doing a `git rm`
first, `git add -A` will not detect case changes in filenames.
"""
function gitrm_copy(src, dst)
    # Remove individual entries since with versions=nothing the root
    # would be removed and we want to preserve previews
    if isdir(dst)
        for x in filter!(!in((".git", "previews")), readdir(dst))
            # --ignore-unmatch so that we wouldn't get errors if dst does not exist
            run(`$(git()) rm -rf --ignore-unmatch $(joinpath(dst, x))`)
        end
    end
    # git rm also remove parent directories
    # if they are empty so need to mkpath after
    mkpath(dst)
    # Copy individual entries rather then the full folder since with
    # versions=nothing it would replace the root including e.g. the .git folder
    for x in readdir(src)
        cp(joinpath(src, x), joinpath(dst, x); force=true)
    end
end



struct FilesystemDeployConfig <: DynamicModelTestUtils.DeployTarget
    repo_path :: String
    subfolder :: String
end
function DynamicModelTestUtils.deploy_folder(c::FilesystemDeployConfig; branch, repo, kwargs...)
    DynamicModelTestUtils.DeployConfig(; all_ok = true, subfolder = c.subfolder, branch, repo)
end
DynamicModelTestUtils.authentication_method(::FilesystemDeployConfig) = DynamicModelTestUtils.HTTPS
DynamicModelTestUtils.authenticated_repo_url(c::FilesystemDeployConfig) = c.repo_path
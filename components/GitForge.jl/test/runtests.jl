using GitForge
using GitForge: AStr, Endpoint, ForgeAPINotImplemented, ForgeAPIError
const GF = GitForge

using HTTP: HTTP
using JSON2: JSON2
using Test: @test, @testset, @test_throws

function capture(f::Function)
    so = stdout
    r, w = redirect_stdout()
    out = @async read(r, String)
    result = f()
    redirect_stdout(so)
    close(w)
    return result, fetch(out)
end

struct TestPostProcessor <: GF.PostProcessor end
GF.postprocess(::TestPostProcessor, ::HTTP.Response, ::Type) = :foo

struct TestForge <: GF.Forge end
GF.base_url(::TestForge) = "https://httpbin.org"
GF.request_headers(::TestForge, ::Function) = ["Foo" => "Bar"]
GF.request_query(::TestForge, ::Function) = Dict("foo" => "bar")
GF.request_kwargs(::TestForge, ::Function) = Dict(:verbose => 1)
GF.postprocessor(::TestForge, ::Function) = TestPostProcessor()
GF.endpoint(::TestForge, ::typeof(get_user)) = GF.Endpoint(:GET, "/get")
GF.into(::TestForge, ::typeof(get_user)) = Symbol

@testset "GitForge.jl" begin
    f = TestForge()
    (val, resp), out = capture(() -> get_user(f))

    @testset "Basics" begin
        @test val === :foo
        @test resp isa HTTP.Response
    end

    @testset "Request options" begin
        body = JSON2.read(IOBuffer(resp.body))
        @test startswith(get(body, :url, ""), "https://httpbin.org")
        @test get(get(body, :headers, Dict()), :Foo, "") == "Bar"
        @test get(get(body, :args, Dict()), :foo, "") == "bar"
        @test occursin("/get", get(body, :url, ""))
        @test !isempty(out)
    end

    @testset "Per-call request options" begin
        (_, resp), out = capture() do
            get_user(
                f;
                query=Dict("a" => "b"),
                headers=["A" => "B"],
                request_opts=(; verbose=0),
            )
        end

        @test isempty(out)

        body = JSON2.read(IOBuffer(resp.body))
        @test haskey(body.headers, :Foo)
        @test get(body.headers, :A, "") == "B"
        @test haskey(get(body, :args, Dict()), :foo)
        @test get(get(body, :args, Dict()), :a, "") == "b"
    end
end

# test whether apis are conformant

function test_api_func(api, (func, args...), auth)
    exclude(api, func, args...) && return
    try
        endpoint = GF.endpoint(api, func, args...)
        headers = Dict(GF.request_headers(api, func))
        @test headers["Content-Type"] isa AStr
        @test match(r"^Julia v.* \(GitForge v.*\)", headers["User-Agent"]) !== nothing
        if auth
            @test headers["Authorization"] isa AStr
        end
        @test GF.has_rate_limits(api, func) isa Bool
        @test !GF.has_rate_limits(api, func) || GF.on_rate_limit(api, func) isa GF.OnRateLimit
        @test endpoint isa Endpoint
        if hasmethod(GF.into, typeof.((api, func)))
            @test GF.into(api, func) isa Type
        else
            @test typeof(GF.postprocessor(api, func)) in [GF.DoNothing, GF.DoSomething, JSON]
        end
        true
    catch err
        !(err isa ForgeAPINotImplemented) && rethrow(err)
        err isa ForgeAPINotImplemented && !err.noted && println(sprint(io-> showerror(io, err)))
        err isa ForgeAPINotImplemented && !err.noted && @test true skip = true
        false
    end
end

mock_id(api) = mock_id(user_id_type(api))
mock_id(::Type{Integer}) = 1
mock_id(::Type{String}) = "fred"

function test_api(api; auth = false)
    unimplemented = 0
    @test GF.base_url(api) isa AStr
    for func in [
        get_user, (get_user, "fred"), (get_user, mock_id(api)),
        get_users,
        update_user, (update_user, mock_id(api)),
        create_user, (delete_user, mock_id(api)),
        get_user_repos, (get_user_repos, "fred"), (get_user_repos, mock_id(api)),
        (get_repo, "owner/repo"), (get_repo, "owner", "repo"), (get_repo, "owner", "subgroup", "repo"),
        create_repo, (create_repo, "org"), (create_repo, mock_id(api)), (create_repo, "namespace", "repo",
        (get_branch, "owner", "repo", "branch"),
        (get_branches, "owner", "repo"),
        (delete_branch, "owner", "repo", "branch"),
        (get_file_contents, "owner", "repo", "path"), (get_file_contents, mock_id(api), "path"),
        (get_pull_request, "owner", "repo", mock_id(api)), (get_pull_request, mock_id(api), mock_id(api)),
        (get_pull_requests, "owner", "repo"), (get_pull_requests, mock_id(api)),
        (create_pull_request, "owner", "repo"), (create_pull_request, mock_id(api)),
        (update_pull_request, "owner", "repo", 1), (update_pull_request, mock_id(api), 1),
        (subscribe_to_pull_request, mock_id(api), mock_id(api)),
        (unsubscribe_from_pull_request, mock_id(api), mock_id(api)),
        (get_commit, "owner", "repo", "ref"), (get_commit, mock_id(api), "ref"),
        (is_collaborator, "owner", "repo", "user"), (is_collaborator, "owner", "repo", mock_id(api)),
        (is_member, "org", "user"), (is_member, "org", mock_id(api)),
        groups,
        (get_tags, "owner", "repo"), (get_tags, mock_id(api)),
        (list_pull_request_comments, "owner", "repo", mock_id(api)), (list_pull_request_comments, mock_id(api), mock_id(api)),
        (get_pull_request_comment, "owner", "repo", mock_id(api)), (get_pull_request_comment, mock_id(api), mock_id(api), mock_id(api)),
        (create_pull_request_comment, "owner", "repo", mock_id(api)), (create_pull_request_comment, mock_id(api), mock_id(api)),
        (update_pull_request_comment, "owner", "repo", mock_id(api)), (update_pull_request_comment, mock_id(api), mock_id(api), mock_id(api)),
        (delete_pull_request_comment, "owner", "repo", mock_id(api)), (delete_pull_request_comment, mock_id(api), mock_id(api), mock_id(api)),
        (list_pipeline_schedules, mock_id(api)), (list_pipeline_schedules, "owner", "repo"),
        ]
        tuple = func isa Function ? (func,) : func
        impl = test_api_func(api, tuple, auth)
        !impl && (unimplemented += 1)
    end
    unimplemented > 0 && println("    $unimplemented unimplmented method$(unimplemented == 1 ? "" : "s")")
end

using GitForge.Bitbucket
using GitForge.GitHub
using GitForge.GitHub: Token, GitHubAPI
using GitForge.GitLab
using GitForge.GitLab: GitLabAPI
using UUIDs: UUID

const GH_TOKEN = Token("secret token")
const GL_TOKEN = OAuth2Token("secret token")
const USER = "a user"
const WORKSPACE = "a workspace"

user_id_type(::GitHubAPI) = Integer
user_id_type(::GitLabAPI) = Integer
user_id_type(::BitbucketAPI) = UUID

mock_id(::Type{UUID}) = UUID(0)

"""
    exclude(api, func, args...)

exclude an api function from testing
"""
exclude(_api, _func, _args...) = false
#exclude(::BitbucketAPI, ::typeof(get_file_contents), _args...) = true

@testset "Bitbucket.jl" begin
    api = BitbucketAPI(; token = GH_TOKEN, workspace = WORKSPACE)
    @test api.token == GH_TOKEN
    @test api.url == Bitbucket.DEFAULT_URL
    @test api.workspace == WORKSPACE
    test_api(api, auth = true)
    @test_throws ForgeAPIError GF.endpoint(api, get_repo, "owner")
end

@testset "GitHub.jl" begin
    api = GitHubAPI(; token = GH_TOKEN)
    @test api.token == GH_TOKEN
    @test api.url == GitHub.DEFAULT_URL
    test_api(api, auth = true)
end

@testset "GitLab.jl" begin
    api = GitLabAPI(; token = GL_TOKEN)
    @test api.token == GL_TOKEN
    @test api.url == GitLab.DEFAULT_URL
    test_api(api, auth = true)
end

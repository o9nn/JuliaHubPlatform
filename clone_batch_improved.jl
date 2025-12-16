#!/usr/bin/env julia
"""
Improved script to clone repositories in batches from repos.tsv
with better git handling to avoid SIGTERM issues during large commits.

Usage: julia clone_batch_improved.jl <batch_number> [batch_size]
Example: julia clone_batch_improved.jl 1 20

Key improvements:
1. Uses git add with batching to avoid overwhelming git
2. Commits after each batch of files to prevent timeout
3. Better error handling and progress reporting
4. Configures git to handle large repos better
"""
using DelimitedFiles

function configure_git()
    # Configure git for better handling of large repos
    try
        run(`git config core.autocrlf input`)  # Handle CRLF consistently
        run(`git config core.safecrlf false`)  # Don't warn about CRLF
        run(`git config pack.windowMemory 256m`)
        run(`git config pack.packSizeLimit 256m`)
        println("Git configured for large repository handling")
    catch e
        println("Warning: Could not configure git: $e")
    end
end

function stage_files_in_batches(dir::String, batch_size::Int=500)
    """Stage files in smaller batches to avoid git add timeout"""
    # Get all untracked and modified files
    files_output = read(`git status --porcelain`, String)
    files = filter(!isempty, split(files_output, '\n'))
    
    if isempty(files)
        println("No files to stage")
        return true
    end
    
    println("Found $(length(files)) files to stage")
    
    # Stage files in batches
    for i in 1:batch_size:length(files)
        batch_end = min(i + batch_size - 1, length(files))
        batch = files[i:batch_end]
        
        # Extract file paths (remove status prefix)
        file_paths = [strip(split(f, ' ', limit=2)[end]) for f in batch]
        file_paths = filter(!isempty, file_paths)
        
        if !isempty(file_paths)
            try
                # Use xargs-style batching for git add
                for fp in file_paths
                    # Remove quotes if present
                    clean_path = replace(fp, "\"" => "")
                    if isfile(clean_path) || isdir(clean_path)
                        run(`git add $clean_path`)
                    end
                end
                println("  Staged batch $i-$batch_end of $(length(files))")
            catch e
                println("  Warning: Error staging batch $i-$batch_end: $e")
            end
        end
    end
    
    return true
end

function commit_component(name::String)
    """Commit a single component immediately after cloning"""
    try
        # Stage only the specific component directory
        component_path = joinpath("components", name)
        if isdir(component_path)
            # Use git add with the specific path
            run(`git add $component_path`)
            
            # Commit this component
            commit_msg = "Add component: $name"
            run(`git commit -m $commit_msg --no-verify`)
            println("  ✓ Committed $name")
            return true
        end
    catch e
        # If commit fails (e.g., nothing to commit), that's okay
        if !occursin("nothing to commit", string(e))
            println("  Warning: Could not commit $name: $e")
        end
    end
    return false
end

function main(batch_num::Int, batch_size::Int=20; commit_each::Bool=true)
    repos_file = "repos.tsv"
    components_dir = "components"
    
    # Configure git first
    configure_git()
    
    # Read the repos.tsv file
    println("Reading $repos_file...")
    data = readdlm(repos_file, '\t', String, '\n'; skipstart=1)
    
    # Extract URLs and names (first two columns)
    all_repos = [(row[1], row[2]) for row in eachrow(data)]
    
    # Calculate batch range
    total_repos = length(all_repos)
    start_idx = (batch_num - 1) * batch_size + 1
    end_idx = min(batch_num * batch_size, total_repos)
    
    if start_idx > total_repos
        println("Batch $batch_num is out of range. Total repos: $total_repos")
        return false
    end
    
    repos = all_repos[start_idx:end_idx]
    
    println("Total repositories: $total_repos")
    println("Cloning batch $batch_num: repos $start_idx-$end_idx ($(length(repos)) repos)")
    println("Commit each component: $commit_each")
    println()
    
    # Create components directory if it doesn't exist
    if !isdir(components_dir)
        mkdir(components_dir)
        println("Created $components_dir directory")
    end
    
    # Clone each repository in this batch
    success_count = 0
    failed_repos = []
    
    for (i, (url, name)) in enumerate(repos)
        global_idx = start_idx + i - 1
        target_dir = joinpath(components_dir, name)
        
        # Skip if already exists
        if isdir(target_dir)
            println("[$global_idx/$total_repos] Skipping $name (already exists)")
            success_count += 1
            continue
        end
        
        println("[$global_idx/$total_repos] Cloning $name from $url...")
        
        # Clone with depth 1 for faster cloning
        try
            run(`git clone --depth 1 $url $target_dir`)
            
            # Remove .git directory to integrate as monorepo
            git_dir = joinpath(target_dir, ".git")
            if isdir(git_dir)
                rm(git_dir; recursive=true, force=true)
                println("  ✓ Removed .git directory from $name")
            end
            
            # Optionally commit each component immediately
            if commit_each
                commit_component(name)
            end
            
            success_count += 1
        catch e
            println("  ✗ Failed to clone $name: $e")
            push!(failed_repos, (name, url))
        end
        
        println()
    end
    
    # If not committing each, do a final commit for the batch
    if !commit_each && success_count > 0
        println("Staging and committing batch...")
        try
            stage_files_in_batches(components_dir)
            run(`git commit -m "Add batch $batch_num components ($start_idx-$end_idx)" --no-verify`)
            println("✓ Committed batch $batch_num")
        catch e
            println("Warning: Final commit failed: $e")
        end
    end
    
    # Summary
    println("=" ^ 60)
    println("Batch $batch_num Summary:")
    println("  Repos in batch: $(length(repos))")
    println("  Successfully cloned: $success_count")
    println("  Failed: $(length(failed_repos))")
    
    if !isempty(failed_repos)
        println()
        println("Failed repositories:")
        for (name, url) in failed_repos
            println("  - $name ($url)")
        end
    end
    
    println("=" ^ 60)
    
    return success_count == length(repos)
end

# Run the script
if abspath(PROGRAM_FILE) == @__FILE__
    if length(ARGS) < 1
        println("Usage: julia clone_batch_improved.jl <batch_number> [batch_size] [--commit-each]")
        println("Example: julia clone_batch_improved.jl 1 20")
        println("         julia clone_batch_improved.jl 1 20 --commit-each")
        exit(1)
    end
    
    batch_num = parse(Int, ARGS[1])
    batch_size = length(ARGS) >= 2 ? parse(Int, ARGS[2]) : 20
    commit_each = "--commit-each" in ARGS || "-c" in ARGS
    
    success = main(batch_num, batch_size; commit_each=commit_each)
    exit(success ? 0 : 1)
end

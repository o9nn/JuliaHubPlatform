#!/usr/bin/env julia

"""
Script to clone repositories in batches from repos.tsv
Usage: julia clone_batch.jl <batch_number> [batch_size]
Example: julia clone_batch.jl 1 20
"""

using DelimitedFiles

function main(batch_num::Int, batch_size::Int=20)
    repos_file = "repos.tsv"
    components_dir = "components"
    
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
            
            success_count += 1
        catch e
            println("  ✗ Failed to clone $name: $e")
            push!(failed_repos, (name, url))
        end
        
        println()
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
        println("Usage: julia clone_batch.jl <batch_number> [batch_size]")
        println("Example: julia clone_batch.jl 1 20")
        exit(1)
    end
    
    batch_num = parse(Int, ARGS[1])
    batch_size = length(ARGS) >= 2 ? parse(Int, ARGS[2]) : 20
    
    success = main(batch_num, batch_size)
    exit(success ? 0 : 1)
end


function load_data(action; root=currentdir(), branch="data")
    mktempdir() do temp
        cd(temp) do
            run(`$(git()) init`)
            run(`$(git()) config user.name "todo"`)
            run(`$(git()) config user.email "todo@example.com"`)
            run(`$(git()) remote add upstream $root`)
            
            run(`$(git()) fetch upstream`)

            try
                run(`$(git()) checkout -b $branch upstream/$branch`)
            catch e
                @info """
                Checking out $branch failed, creating a new orphaned branch.
                This usually happens when deploying to a repository for the first time and
                the $branch branch does not exist yet. The fatal error above is expected output
                from Git in this situation. Generate and commit some data to $branch, then try again.
                """
                throw(Exception("checking out $branch failed with error: $e"))
            end
            action(temp)
        end
    end
end

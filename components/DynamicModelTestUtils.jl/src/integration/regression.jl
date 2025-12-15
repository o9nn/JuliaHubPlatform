function default_comparison(name, sol, df; tol=1e-3, show_results=true, kwargs...)
    cmp_df = compare(sol, df; kwargs...)
    if show_results
        @show cmp_df
    end

    # this is lame but is better than the alternatives
    for (idx, var) in enumerate(cmp_df[:, :var])
        for (cidx, col) in enumerate(names(cmp_df))
            if col ∈ ["observed", "var"]
                continue
            end
            @test cmp_df[idx, cidx] < tol # "Solution $name differs from regression data using metric $col on variable $var by $(cmp_df[idx, cidx])."
        end
    end
    return cmp_df
end


function regression_test(
    runs::Dict{Symbol, <:SciMLBase.AbstractTimeseriesSolution}, 
    rootdir=currentdir(),
    data_dir="data",
    should_deploy="--deploy_data" ∈ ARGS;
    make_output_dir=false,
    clean_output_dir=false,
    deploy_only=false,
    restore=(go, root) -> load_data(go; root=root),
    cmp=(name, sol, df) -> default_comparison(name, sol, df),
    do_load=(filename) -> CSV.read(filename, DataFrame),
    discretize=(sol) -> discretize_solution(sol),
    save=(filename, df) -> CSV.write(filename, df),
    do_deploy=deploy,
    mk_filename=(symbol) -> string(symbol) * ".csv")
    should_save = false
    data_fulldir = joinpath(rootdir, data_dir)
    if !isfile(data_fulldir) && make_output_dir
        mkpath(data_fulldir)
        should_save = true
    elseif isfile(data_fulldir) && make_output_dir
        if clean_output_dir
            if !isempty(readdir(data_fulldir))
                for rmfile in readdir(data_fulldir)
                    @warn "Removing existing data file $rmfile"
                    rm(rmfile)
                end
            end
        end
        should_save = true
    end


    if !deploy_only
        restore(rootdir) do restored
            cd(restored) do
                for (name, new_sol) in runs 
                    @assert isfile(mk_filename(name))
                    data = do_load(mk_filename(name))
                    cmp(name, new_sol, data)
                end
            end
        end
    end
    if should_save
        for (name, new_sol) in runs 
            save(joinpath(data_fulldir, mk_filename(name)), discretize(new_sol))
        end
    end
    if should_save && should_deploy
        do_deploy(rootdir, data_dir,
            deploy_type = DynamicModelTestUtils.FilesystemDeployConfig(rootdir, "."),
            repo = rootdir)
    elseif !should_save && should_deploy
        mktempdir() do temp 
            cd(temp) do 
                mkdir(data_dir)
                do_deploy(temp, data_dir,
                    deploy_type = DynamicModelTestUtils.FilesystemDeployConfig(rootdir, "."),
                    repo = rootdir)

            end
        end
    end
end
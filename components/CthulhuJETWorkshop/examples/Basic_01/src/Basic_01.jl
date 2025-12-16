module Basic_01

# a tiny introduction to the tools

using Statistics: mean

#####################
#= Data structures =#

struct TrialData
    data::Vector
end

struct TrialStatistics
    mean::Float64
end

function Base.show(io::IO, stats::TrialStatistics)
    println(Core.stdout, "TrialStatistics(")
    print(Core.stdout, "  mean=")
    println(Core.stdout, stats.mean)
    print(Core.stdout, ")")
end

#######################
#= Application logic =#

function perform_N_trials(N; sample_size=100)
    return [
        TrialData(randn(sample_size))
        for i = 1:N
    ]
end

function aggregate_data(data::Vector{TrialData})
    # a simple sum
    sample_means = [
        # poorly-typed `data` field prevents `sum(...)` from being inferred
        mean(trial.data)
        for trial in data
    ]

    return TrialStatistics(
        #= mean =# mean(sample_means), # average the averages
    )
end

function @main(ARGS)

    # perform random experiments
    data = perform_N_trials(10)

    # aggregate some statistics
    stats = aggregate_data(data)

    # print data
    println(Core.stdout, stats)

    return 0
end

end # module Basic_01

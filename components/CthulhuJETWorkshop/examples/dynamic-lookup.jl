# A settings/config/database-like struct
struct Settings
    n::String
    t::Int
    w::Matrix{Float64}
end

function Settings()
    return Settings("sim-1", 45, rand(2, 3))
end


@noinline function get_setting(settings::Settings, p::String)
    if p == "name"
        return settings.n
    elseif p == "temperature"
        return settings.t
    elseif p == "weights"
        return settings.w
    else
        throw(ArgumentError(string("no parameter named ", p)))
    end
end

function print_parameter(x::String)
    print(repr(x))
end
function print_parameter(x::Int)
    print(x)
end
function print_parameter(x::Matrix)
    print(size(x), " matrix with average value ", sum(x) / prod(size(x)))
end


function main(settings)
    # Extract our data
    name = get_setting(settings, "name")
    temp = get_setting(settings, "temperature")
    weights = get_setting(settings, "weights")
    # Log some output
    print("Simulation name: ")
    print_parameter(name)
    println()
    print("Simulation temperature: ")
    print_parameter(temp)
    println()
    print("Simulation weights: ")
    print_parameter(weights)
    println()
    # Do simulation here...
end

# Setup
settings = Settings()

main(settings)

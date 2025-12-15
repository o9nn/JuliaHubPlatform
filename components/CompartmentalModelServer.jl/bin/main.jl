import Oxygen
import HTTP
using CompartmentalModelServer: greet, problem, solution, simulate
using SwaggerMarkdown: OpenAPI, @swagger

const PROB = problem()
const SOL = solution()

@swagger """
/:
  get:
    description: Returns a simple greeting.
    responses:
      '200':
        description: Successfully returned 'Hello World!'.
"""
Oxygen.@get "/" greet

@swagger """
/problem:
  get:
    description: Returns the original SIR problem's initial conditions, parameters and timespan.
    responses:
      '200':
        description: Successfully returned dictionary with keys `u0`, `p` and `tspan`.
"""
Oxygen.@get "/problem" ()->Dict(:u0 => PROB.u0, :p => PROB.p, :tspan => PROB.tspan)

@swagger """
/solution:
  get:
    description: Returns the original SIR solution.
    responses:
      '200':
        description: Successfully returned dictionary with keys `u` and `t`.
"""
Oxygen.@get "/solution" ()->SOL

@swagger """
/simulate:
  get:
    description: Returns a new SIR solution given the initial conditions, parameters and timespan passed as query parameters.
    parameters:
      - name: S
        in: query
        required: false
        description: this is the initial condition of variable `S`
        schema:
          type : number
      - name: I
        in: query
        required: false
        description: this is the initial condition of variable `I`
        schema:
          type : number
      - name: R
        in: query
        required: false
        description: this is the initial condition of variable `R`
        schema:
          type : number
      - name: beta
        in: query
        required: false
        description: this is the initial condition of parameter `beta`
        schema:
          type : number
      - name: gamma
        in: query
        required: false
        description: this is the initial condition of parameter `gamma`
        schema:
          type : number
      - name: tstart
        in: query
        required: false
        description: this is the simulation start time
        schema:
          type : number
      - name: tstop
        in: query
        required: false
        description: this is the simulation stop time
        schema:
          type : number
    responses:
      '200':
        description: Successfully returned dictionary containing the simulation inputs and new solution.
"""
Oxygen.@get "/simulate" function (req::HTTP.Request)

    # capture all query parameters
    params = Oxygen.queryparams(req)

    # try parsing numeric parameter values
    S = tryparse(Float64, get(params, "S", ""))
    I = tryparse(Float64, get(params, "I", ""))
    R = tryparse(Float64, get(params, "R", ""))
    β = tryparse(Float64, get(params, "beta", ""))
    γ = tryparse(Float64, get(params, "gamma", ""))
    tstart = tryparse(Float64, get(params, "tstart", ""))
    tstop = tryparse(Float64, get(params, "tstop", ""))

    # prepare all simulation parameter keys and values
    _input_keys = (:Snew, :Inew, :Rnew, :βnew, :γnew, :tstart, :tstop)
    _input_vals = (S, I, R, β, γ, tstart, tstop)

    # keep only given simulation parameter keys and values
    idxs = findall(!isnothing, _input_vals)
    input_keys = _input_keys[[idxs...]]
    input_vals = _input_vals[[idxs...]]
    inputs = (; zip(input_keys, input_vals)...)

    # create new model solution
    return simulate(; inputs...)
end

Oxygen.serve(; host = "0.0.0.0", port = parse(Int, get(ENV, "PORT", "8080")))

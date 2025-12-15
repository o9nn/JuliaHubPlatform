################################################################
# Julia for Engineers
# Modeling Steady-State and Dynamic Systems with Dyad
################################################################

using JuliaForEngineers

using ModelingToolkit
using OrdinaryDiffEq
using Plots

# ------------------------------------------------
# Part 1: Steady-State Modeling with Dyad --------
# ------------------------------------------------

@mtkbuild actuator = HydraulicActuator_steady()
prob = NonlinearProblem(actuator, [], [])
sol = solve(prob, abstol=1e-9)


sol[actuator.A0]
sol[actuator.u]
sol[actuator.x_dot * (actuator.A/actuator.A0)]


orifices = []
velocity_limits = 1.0:0.1:2.0
for velocity_limit in velocity_limits
    prob′ = remake(prob; p=[actuator.x_dot => velocity_limit])
    sol′ = solve(prob′; abstol=1e-9)
    push!(orifices, sol′[actuator.A0])
end

plot(velocity_limits, orifices; xlabel="velocity limit [m/s]", ylabel="orifice size [m^2]")


# ------------------------------------------------
# Part 2: Dynamic Modeling with Dyad--------------
# ------------------------------------------------

@mtkbuild actuator = HydraulicActuator_dynamic()
prob = ODEProblem(actuator, [], (0.0, 0.0001), [])
sol = solve(prob)


plot(sol; idxs=[actuator.x], ylabel="position")
plot(sol; idxs=[actuator.x_dot], ylabel="velocity")
plot(sol; idxs=[actuator.x_ddot], ylabel="acceleration")

plot(sol; idxs=[actuator.p1, actuator.p2], ylabel="pressure")

plot(sol; idxs=[actuator.c * actuator.x_dot], ylabel="force")





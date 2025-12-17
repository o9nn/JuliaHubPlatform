using Dash, PlotlyJS
using CompartmentalModel: greet, problem, solution, simulate

const PROB = problem()
const SOL = solution()

app = dash()
app_style = Dict("width" => "76%", "text-align" => "-webkit-center", "margin" => "2% 5%")
group_style = Dict("width" => "76%", "margin" => "2% 5%", "display" => "inline-block")
grouped_style = Dict("width" => "76%", "margin" => "2% 5%", "display" => "flex")
tooltip_style = Dict("always_visible" => true, "placement" => "top")

app.layout = html_div(; style=app_style) do
    html_h1("Compartmental Model Dashboard"),
    html_div("Demonstrate how to deploy a model as a dashboard application on JuliaHub."),
    dcc_graph(; id="solution-graph"),
    html_div(;
        children=[
            html_div(;
                children=[
                    html_h3("Parameters"),
                    dcc_markdown("β"),
                    dcc_slider(;
                        id="beta",
                        min=0.0,
                        max=1.0,
                        value=PROB.p[1],
                        step=nothing,
                        tooltip=tooltip_style,
                    ),
                    dcc_markdown("γ"),
                    dcc_slider(;
                        id="gamma",
                        min=0.0,
                        max=1.0,
                        value=PROB.p[2],
                        step=nothing,
                        tooltip=tooltip_style,
                    ),
                ],
                style=group_style,
            ),
            html_div(;
                children=[
                    html_h3("Initial Population"),
                    dcc_markdown("Susceptible"),
                    dcc_slider(;
                        id="susceptible",
                        min=0.0,
                        max=10_000.0,
                        value=PROB.u0[1],
                        step=nothing,
                        tooltip=tooltip_style,
                    ),
                    dcc_markdown("Infected"),
                    dcc_slider(;
                        id="infected",
                        min=0.0,
                        max=10_000.0,
                        value=PROB.u0[2],
                        step=nothing,
                        tooltip=tooltip_style,
                    ),
                    dcc_markdown("Recovered"),
                    dcc_slider(;
                        id="recovered",
                        min=0.0,
                        max=10_000.0,
                        value=PROB.u0[3],
                        step=nothing,
                        tooltip=tooltip_style,
                    ),
                ],
                style=group_style,
            ),
        ],
        style=grouped_style,
    )
end

callback!(
    app,
    Output("solution-graph", "figure"),
    Input("beta", "value"),
    Input("gamma", "value"),
    Input("susceptible", "value"),
    Input("infected", "value"),
    Input("recovered", "value"),
) do beta, gamma, susceptible, infected, recovered
    inputs = (
        βnew=beta,
        γnew=gamma,
        Snew=susceptible,
        Inew=infected,
        Rnew=recovered,
        tstart=PROB.tspan[1],
        tstop=PROB.tspan[end],
    )
    sol = simulate(; inputs...)
    p1 = scatter(; x=sol.time, y=sol.S, mode="lines+markers", name="Susceptible")
    p2 = scatter(; x=sol.time, y=sol.I, mode="lines+markers", name="Infected")
    p3 = scatter(; x=sol.time, y=sol.R, mode="lines+markers", name="Recovered")
    fig_layout = Layout(; title="Model Solution")
    fig = plot([p1, p2, p3], fig_layout)
    return fig
end

run_server(app, "0.0.0.0"; debug=true)

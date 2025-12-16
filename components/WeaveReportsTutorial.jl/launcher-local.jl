using Weave
using Dates

const xaxis = get(ENV, "xaxis", "sepal.length")
const yaxis = get(ENV, "yaxis", "petal.width")
const date = Dates.today()

const path_results = "$(@__DIR__)/results"
const report_name = "weave-iris"

weave("$(@__DIR__)/reports/$(report_name).jmd",
    out_path = "$(path_results)",
    fig_path = "$(path_results)/fig",
    doctype = "md2html",
    args = (date = date, xaxis = xaxis, yaxis = yaxis))
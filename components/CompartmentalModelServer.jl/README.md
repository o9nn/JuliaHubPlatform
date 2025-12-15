# CompartmentalModelServer.jl

Creating a model is half the battle. Often models must be deployed to production
in order to facilitate consumption by other team members or end users.To
maximize effectiveness, the model should be easily accessible without prior
knowledge of the production environment. One method which may cover several use
cases is to spin up an API server which acts as a sort of _simulation engine_
using the final model. Consumers can query the server to gain answers about a
particular simulation of interest. This package explores how to build this
server using [Oxygen.jl](https://ndortega.github.io/Oxygen.jl/stable/)
to program the API endpoints and
[StructTypes.jl](https://juliadata.github.io/StructTypes.jl/stable/)
to customize serialization/deserialization from Julia objects to JSON payloads.

## Template

This repository can be used as a template for deploying your model as a web
service. To get started, click **Use this template** then **Create a new
repository**. This will generate a new repository for you with the same
directory and file structure. See the [project structure](#project-structure)
below for more guidance.

### Model Used

Compartmental models are widely used in many areas including epidemiology. The
model used in this demonstration is the one of the simplest compartmental models
which is used to derive many others, [the SIR
model](https://en.wik_models_in_epidemiology).

### Project Structure

There are 3 main parts:

1. `src/`: source code of project where we define:
    - the model itself,
    - methods for serialization/deserialization,
    - helper functions for solving new simulations.
1. `bin/`: server entry point where we define:
    - the API endpoints.
1. `test/`: unit tests for project.

### Deployment Instructions

Follow these steps to deploy this package on JuliaHub!

1. Go to JuliaHub's [Applications](https://juliahub.com/ui/Applications) page.
1. Click **Add an application** (top-right of page).
1. Enter the URL of this repository:
    <https://github.com/JuliaComputing/CompartmentalModelServer.jl>
    - Note that additional priviledges must be added for private repositories.
1. Click **Add Application**
1. See the application under **My Applications** section of Applications page.
1. Click **Launch** to start an instance of the application.
1. Click **Connect** to connect to that new instance.

## FAQ

### Module Initialization

Initialization of the main module occurs immediately after the module is loaded.
The function body of the `__init__` method defines how the module gets
initialized. The benefit of defining `__init__` is that once the module is
loaded, calling its functions such as `simulate` is fast. The down-side is that
loading the module can take a bit longer. Since this package's main use case is
to run as a web server, it is assumed that startup time is not critical. Most
likely the server is a long running job where code loading and startup time are
only incurred once. Therefore, module Initialization is acceptable.

### Server Host

By default, most web serves (including `Oxygen.jl`) run on the normal loop back
address, localhost or equivalently `127.0.0.1`. However, on JuliaHub a web
server host should be set to `0.0.0.0`.

### Why Oxygen?

Why use `Oxygen.jl`? Why not use `HTTP.jl` or `Genie.jl` or some other package?
Each of the packages above can be used to achieve the same objective.

`HTTP.jl` would be effective, but less convenient to write the same web service.
It requires explicit registration of endpoints and does not offer functionality
such as multithreading, JSON serialization and scheduling out-of-the-box.

`Genie.jl` aims to provide a framework for building full-stack web applications
which include database integrations, high-performance backend services, and
model-view-controller frontends. Most of these capabilities are not necessary
for our use case. Other features, such as database integrations, may be easily
substituted for JuliaHub platform features, like DataSets.

In summary, `Oxygen.jl` was chosen because of its convenience and targeted
capabilities to solve our problem at hand. This light-weight package is an
excellent choice when only requiring its prioritized
[features](https://ndortega.github.io/Oxygen.jl/stable/#Features).

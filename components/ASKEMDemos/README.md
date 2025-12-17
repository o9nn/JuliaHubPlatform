# ASKEMDemos

# ModelingToolkit with SymPy
This Jupyter notebook requires Python, Jupyter, and Julia to be installed on the system. 

This notebook uses the package JuliaCall to call Julia from Python. 

JuliaCall can be added using `pip install juliacall`.

JuliaCall uses the package JuliaPkg to handle Julia dependencies from Python, use `pip install juliapkg`.

JuliaPkg uses a JSON configuration file named `juliapkg.json` to handle any Julia package dependencies. The `juliapkg.json` file for the `SymPyModelingToolkit.ipynb` notebook is in the same directory as the notebook. 
 
As long as JuliaCall and JuliaPkg are installed they will handle the Julia dependencies once you run the cell containing `juliapkg.resolve()`. The custom conversion rules to convert SymPy equations to Symbolics.jl equations are held in the `SymPyPythonCall.jl` package, so once the cell containing `jl.seval("using SymPyPythonCall")` is run the rules are in effect and the rest of the notebook should run as intended. 

For more information on the Python packages JuliaCall and JuliaPkg see [PythonCall](https://docs.juliahub.com/PythonCall/WdXsa/0.9.14/juliacall/) and [JuliaPkg](https://github.com/JuliaPy/pyjuliapkg).
# Glacial flow Pluto notebook
 If you already have Julia and Pluto notebook installed this notebook should work out of the box. To install Julia you can either download an executable from the [list](https://julialang.org/downloads/) at julialang.org, or you can use [JuliaUp](https://github.com/JuliaLang/juliaup). Once julia is installed you can run `import Pkg` and then `Pkg.add("Pluto")` in the REPL to install Pluto. Once it is installed run `using Pluto` and `Pluto.run()` to open Pluto. Then open the `GlacialFlowNotebook.jl` file from Pluto. For more information on Pluto notebooks, see their [website](https://plutojl.org/).
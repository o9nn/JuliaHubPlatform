# Hardened

Hardened.jl is a simple package to strictly check that Julia
is running with the correct flags for Hardened Compilation.
This is useful for security and safety crtical systems.

Currently this checks for:

- Bounds checking
- Global IEEE math mode
- Function overwrite

Bounds checking: overrides any `@inbounds` declarations
IEEE math mode: overrides any `@fastmath` declarations
Function overwrite: insures against resource injection and ensures testing integrity

## Example workflow

```julia
using Hardened
using PrecompileTools

# PrecompileTools workload
@setup_workload begin

    # This will check that the compilation is hardened
    Hardened.check()

    @compile_workload begin
        # ...
    end
end

```

## Covered CWEs

### CWE-125: Out-of-bounds Read

<https://cwe.mitre.org/data/definitions/125.html>

### CWE-787: Out-of-bounds Write

<https://cwe.mitre.org/data/definitions/787.html>

### CWE-1339: Insufficient Precision or Accuracy of a Real Number

<https://cwe.mitre.org/data/definitions/1339.html>

### CWE-99: Improper Control of Resource Identifiers ('Resource Injection')

<https://cwe.mitre.org/data/definitions/99.html>

## Additional Checks

This package only provides coverage against some CWEs. For additional checks, tools such as [JuliaHub](mailto:sales@juliahub.com?subject=Inquiry%20about%20JuliaHub%20for%20additional%20static%20checks&body=Hello%2C%0AI%20am%20interested%20in%20learning%20more%20about%20how%20JuliaHub%20can%20provide%20additional%20static%20checks.%20Please%20provide%20me%20with%20more%20information.%0AThank%20you!%0A) can provide greater static coverage.

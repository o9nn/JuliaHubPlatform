"""
Hardened.jl is a simple package to strictly check that Julia
is running with the correct flags for Hardened Compilation.
This is useful for security and safety crtical systems.

Currently this checks for:
    - Bounds checking
    - Global IEEE math mode

Bounds checking: overrides any `@inbounds` declarations
IEEE math mode: overrides any `@fastmath` declarations
"""
module Hardened

const BOUNDS_CHECK_ENABLED = 1
const IEEE_MATH_MODE = 2
const WARN_OVERWRITE_ENABLED = 1

"""
    check(; bounds=true, math=true, overwrite=true)

Checks if Julia is running with the required flags for hardened compilation:

- `bounds=true` checks if `--check-bounds=yes` is set
- `math=true` checks if `--math-mode=ieee` is set
- `overwrite=true` checks if `--warn-overwrite=yes` is set

Throws an error if the required flags are not set.
"""
function check(; bounds=true, math=true, overwrite=true)
    check_bounds_flag = Base.JLOptions().check_bounds != BOUNDS_CHECK_ENABLED
    fast_math_flag = Base.JLOptions().fast_math != IEEE_MATH_MODE
    warn_overwrite_flag = Base.JLOptions().warn_overwrite != WARN_OVERWRITE_ENABLED

    err_msg = ""
    if bounds && check_bounds_flag
        err_msg *= """Hardened Compilation: --check-bounds is not set to 'yes'. Run Julia with --check-bounds=yes
                          Mitigates:
                          - CWE-125: Out-of-bounds Read
                          - CWE-787: Out-of-bounds Write
                   """
    end
    if math && fast_math_flag
        err_msg *= """Hardened Compilation: --math-mode is not set to 'ieee'. Run Julia with --math-mode=ieee
                          Mitigates:
                          - CWE-1339: Insufficient Precision or Accuracy of a Real Number
                   """
    end
    if overwrite && warn_overwrite_flag
        err_msg *= """Hardened Compilation: --warn-overwrite is not set to 'yes'. Run Julia with --warn-overwrite=yes
                          Mitigates:
                          - CWE-99: Improper Control of Resource Identifiers ('Resource Injection')
                   """
    end
    if !isempty(err_msg)
        error(err_msg)
    end
end

end

# Use baremodule to shave off a few KB from the serialized `.ji` file
baremodule Openresty_jll
using Base
using Base: UUID
import JLLWrappers

JLLWrappers.@generate_main_file_header("Openresty")
JLLWrappers.@generate_main_file("Openresty", UUID("87da34d4-7b1b-5a94-8376-8cb65bf3132c"))
end  # module Openresty_jll

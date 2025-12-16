# Use baremodule to shave off a few KB from the serialized `.ji` file
baremodule Dex_jll
using Base
using Base: UUID
import JLLWrappers

JLLWrappers.@generate_main_file_header("Dex")
JLLWrappers.@generate_main_file("Dex", UUID("f1ef5e10-671a-599f-ac25-3c68827556ba"))
end  # module Dex_jll

# Use baremodule to shave off a few KB from the serialized `.ji` file
baremodule OpenPolicyAgent_jll
using Base
using Base: UUID
import JLLWrappers

JLLWrappers.@generate_main_file_header("OpenPolicyAgent")
JLLWrappers.@generate_main_file("OpenPolicyAgent", UUID("6ea5c882-2ec3-5826-84d1-aff636352c13"))
end  # module OpenPolicyAgent_jll

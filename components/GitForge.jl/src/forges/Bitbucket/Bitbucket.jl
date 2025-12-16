"""
    Bitbucket

Implements read-only access
"""
module Bitbucket

import ..GitForge: endpoint, into, postprocessor, ForgeAPIError
import ..GitForge.GitHub: ismemberorcollaborator
import Base.@kwdef, Base.Meta.quot
import StructTypes: Struct, UnorderedStruct, construct, constructfrom, StructType

using ..GitForge
using ..GitForge:
    @json,
    AStr,
    DoSomething,
    Endpoint,
    Forge,
    JSON,
    OnRateLimit,
    RateLimiter,
    HEADERS,
    ORL_THROW,
    @not_implemented
using ..GitForge.GitHub: NoToken, JWT, AbstractToken
using Dates, TimeZones, HTTP, JSON2, UUIDs, StructTypes
using Dates: Date, DateTime

export BitbucketAPI, Token, JWT

# using a const here in order to export it
const Token = GitForge.GitHub.Token
const DEFAULT_URL = "https://api.bitbucket.org/2.0"

auth_headers(::NoToken) = []
auth_headers(t::Token) = ["Authorization" => "Basic $(t.token)"]
auth_headers(t::JWT) = ["Authorization" => "Bearer $(t.token)"]

function JSON2.read(io::IO, ::Type{UUID}; kwargs...)
    str = JSON2.read(io, String)
    UUID(str[2:end-1])
end
JSON2.write(io::IO, uuid::UUID; kwargs...) = JSON2.write(io, "{$uuid}"; kwargs...)

"""
    BitbucketAPI(;
        token::AbstractToken=NoToken(),
        url::$AStr="$DEFAULT_URL",
        has_rate_limits::Bool=true,
        on_rate_limit::OnRateLimit=ORL_THROW,
    )

Create a Bitbucket API client.

## Keywords
- `token::AbstractToken=NoToken()`: Authorization token (or lack thereof).
- `url::$AStr="$DEFAULT_URL"`: Base URL of the target Bitbucket instance.
- `has_rate_limits::Bool=true`: Whether or not the Bitbucket server has rate limits.
- `on_rate_limit::OnRateLimit=ORL_THROW`: Behaviour on exceeded rate limits.
- `workspace::AbstractString=""`: slug for chosen workspace
"""
mutable struct BitbucketAPI <: Forge
    token::AbstractToken
    url::AbstractString
    hasrl::Bool
    orl::OnRateLimit
    rl_general::RateLimiter
    rl_search::RateLimiter
    workspace::AbstractString

    function BitbucketAPI(;
        token::AbstractToken=NoToken(),
        url::AStr=DEFAULT_URL,
        has_rate_limits::Bool=false,
        on_rate_limit::OnRateLimit=ORL_THROW,
        workspace::AbstractString="",
    )
        return new(token, url, has_rate_limits, on_rate_limit, RateLimiter(), RateLimiter(), workspace)
    end
end

GitForge.base_url(b::BitbucketAPI) = b.url
GitForge.request_headers(b::BitbucketAPI, ::Function) = [HEADERS; auth_headers(b.token)]
GitForge.postprocessor(::BitbucketAPI, ::Function) = JSON_Struct()
GitForge.has_rate_limits(b::BitbucketAPI, ::Function) = b.hasrl
GitForge.rate_limit_check(b::BitbucketAPI, ::Function) = GitForge.rate_limit_check(b.rl_general)
GitForge.on_rate_limit(b::BitbucketAPI, ::Function) = b.orl
GitForge.rate_limit_wait(b::BitbucketAPI, ::Function) = GitForge.rate_limit_wait(b.rl_general)
GitForge.rate_limit_period(b::BitbucketAPI, ::Function) = GitForge.rate_limit_period(b.rl_general)
GitForge.rate_limit_update!(b::BitbucketAPI, ::Function, r::HTTP.Response) =
    GitForge.rate_limit_update!(b.rl_general, r)

struct JSON_Struct <: GitForge.PostProcessor
    f::Function
end
JSON_Struct() = JSON_Struct(identity)

function GitForge.postprocess(p::JSON_Struct, r::HTTP.Response, ::Type{T}) where T
    data = JSON2.read(IOBuffer(r.body))
    converted = try
        constructfrom(T, data)
    catch err
        @error "Error converting to type $T: $data" exception=(err,catch_backtrace())
        rethrow(err)
    end
    p.f(converted)
end

constructfield(::Type{FT}, v) where {FT} = constructfrom(FT, v)

# capture type Nothing fields here so the methods below won't apply to them
constructfield(::Type{Nothing}, v) = constructfrom(Nothing, v)

constructfield(::Type{FT}, str::AbstractString) where {FT <: Union{UUID, Nothing}} = UUID(str[2:end-1])

constructfield(::Type{FT}, v::AbstractString) where {FT <: Union{Date, Nothing}} =
    Date(ZonedDateTime(replace(v, r"(\.[0-9]{3})([0-9]*)\+" => s"\1+")))

constructfield(::Type{FT}, v::AbstractString) where {FT <: Union{DateTime, Nothing}} =
    DateTime(ZonedDateTime(replace(v, r"(\.[0-9]{3})([0-9]*)\+" => s"\1+")))

macro struct_def(type::Symbol)
    quote
        StructTypes.StructType(::Type{$type}) = StructTypes.DictType()

        StructTypes.construct(::Type{$type}, x::Dict; kw...) =
            $type(;
                  [k=> constructfield(fieldtype($type, k), v)
                   for (k, v) in x if hasfield($type, k)]...,
                  _extras = Dict(kw..., [k=> v for (k, v) in x if !hasfield($type, k)]...),
                  )

        StructTypes.keyvaluepairs(obj::$type) = [
            [k=>getfield(obj, k) for k in fieldnames($type) if k != :_extras]...,
            pairs(obj._extras)...,
        ]
    end
end

macro json_struct(decl::Expr)
    type = decl.args[2]
    block = macroexpand(Bitbucket, :(@json $decl))
    push!(block.args, macroexpand(Bitbucket, :(@struct_def $type)))
    esc(block)
end

include("pagination.jl")
include("users.jl")
include("repositories.jl")
include("pull_requests.jl")
include("workspaces.jl")
include("commits.jl")
include("branches.jl")
include("tags.jl")
#include("comments.jl")

end

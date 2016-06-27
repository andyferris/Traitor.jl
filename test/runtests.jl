using Traitor
using Traitor: extract_arg_trait
using BaseTestNext

# Parsing of function argument trait constraints
# Simple trait & type symbols
@test extract_arg_trait(:(x::T::A)) == (:(x::T), :A)
@test extract_arg_trait(:(x::::A))  == (:(x),    :A)
@test extract_arg_trait(:(x::T))    == (:(x::T), nothing)
@test extract_arg_trait(:(x))       == (:x,      nothing)
# Type exprs
@test extract_arg_trait(:(x::Union{S,T}::A)) == (:(x::Union{S,T}), :A)
@test extract_arg_trait(:(x::Union{S,T}))    == (:(x::Union{S,T}), nothing)
# Trait exprs
@test extract_arg_trait(:(x::T::Union{A,B})) == (:(x::T), :(Union{A,B}))
@test extract_arg_trait(:(x::::Union{A,B}))  == (:(x),    :(Union{A,B}))
# Trait & type exprs
@test extract_arg_trait(:(x::Union{S,T}::Union{A,B})) == (:(x::Union{S,T}), :(Union{A,B}))
@test extract_arg_trait(:(x::::Union{A,B}))  == (:(x),    :(Union{A,B}))
@test extract_arg_trait(:(x::Union{S,T}))    == (:(x::Union{S,T}), nothing)
# Keyword args
@test extract_arg_trait(Expr(:kw,:(x::T::A),1)) == (Expr(:kw,:(x::T),1), :A)
@test extract_arg_trait(Expr(:kw,:(x::::A),1))  == (Expr(:kw,:x,1),      :A)
@test extract_arg_trait(Expr(:kw,:(x::T),1))    == (Expr(:kw,:(x::T),1), nothing)
@test extract_arg_trait(Expr(:kw,:x,1))         == (Expr(:kw,:x,1),      nothing)


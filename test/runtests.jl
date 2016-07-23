using Traitor
using Base.Test

abstract Size

immutable Big <: Size; end
immutable Medium <: Size; end
immutable Small <: Size; end

Size(::Union{Type{Int32},Type{Int64}}) = Small
Size(::Type{BigInt}) = Big
Size(::Type{Int128}) = Medium

@traitor function howbig(x::Any::Big)
    "Huge!"
end
@traitor function howbig(x::::Medium)
    "So-so"
end
@test howbig(Int128(1)) == "So-so"
@test howbig(BigInt(1)) == "Huge!"

@traitor function howbig(x::Integer::Small)
    "Teensy..."
end

@test howbig(1) == "Teensy..."
# These next ones throw...
# @test howbig(Int128(1)) == "So-so"
# @test howbig(BigInt(1)) == "Huge!"
# ...until we define
@traitor howbig(x::Integer::Big) = "Huge!"
@traitor howbig(x::Integer::Medium) = "So-so"

@test howbig(1) == "Teensy..."
@test howbig(Int128(1)) == "So-so"
@test howbig(BigInt(1)) == "Huge!"

@test @inferred(supertrait(Small)) == Size
@test @inferred(supertrait(Union{Small,Medium})) == Size

abstract Fooness

immutable FooA <: Fooness ; end
immutable FooB <: Fooness ; end

Fooness(::Type{Any}) = FooA
Fooness(::Type{Int16}) = FooB

@test_throws ErrorException supertrait(Union{Small,FooB})

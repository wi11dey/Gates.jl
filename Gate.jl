module Gate

using Circuit

import Base: ~, &, |, xor

mutable struct Gate{F, N} <: Circuit
    inputs::NTuple{N, Union{Gate, BoolVariable}}
    Gate{F, N}(inputs::Vararg{Union{Gate, BoolVariable}, N}) where {F, N} = new{F, N}(inputs)
end
const NotGate = Gate{~, 1}
const AndGate = Gate{&, 2}
const OrGate  = Gate{|, 2}
const XorGate = Gate{⊻, 2}

~(x::BoolExpression{Gate}) = NotGate(x)
~(x::NotGate) = x.inputs[1] # Double-negative

(&)(x::BoolExpression{Gate}, y::BoolExpression{Gate}) = AndGate(x, y)

(|)(x::BoolExpression{Gate}, y::BoolExpression{Gate}) = OrGate(x, y)

xor(x::BoolExpression, y::BoolExpression) = XorGate(x, y)

function Base.show(io::IO, ::MIME"text/vnd.graphviz", x::Gate)
    print(io, "digraph Gates {")
    print(io, "}")
end

end

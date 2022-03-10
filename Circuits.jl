module Circuits

export BoolVariable, Gate, NotGate, AndGate, OrGate, XorGate, SymbolicInteger, substitute

import Base: +, -, *, ==, <<, >>, >>>, ~, &, |, xor

struct BoolVariable
    name::Symbol
    bit::Int
end
Base.show(io::IO, x::BoolVariable) = print(io, x.name, "_", x.bit)

mutable struct Gate{F, N}
    inputs::NTuple{N, Union{Gate,BoolVariable}}

    Gate{F, N}(inputs::Vararg{Union{Gate,BoolVariable}, N}) where {F, N} = new{F, N}(inputs)
end
# TODO: fix pretty-printing
function Base.show(io::IO, x::Gate)
    exprify(x) = x
    exprify(@nospecialize x::Gate{F}) where F = Expr(:call, Symbol(F), exprify.(x.inputs)...)

    print(io, exprify(x))
end

const NotGate = Gate{~, 1}
const AndGate = Gate{&, 2}
const OrGate  = Gate{|, 2}
const XorGate = Gate{⊻, 2}

function Base.show(io::IO, ::MIME"text/vnd.graphviz", x::Gate)
    print(io, "digraph Gates {")
    print(io, "}")
end

const SymbolicBool = Union{Gate, BoolVariable}
const BoolExpression = Union{Bool, SymbolicBool}
Base.convert(::Type{BoolExpression}, x::Int) = Bool(x)

~(x::BoolExpression) = NotGate(x)
~(x::NotGate) = x.inputs[1] # Double-negative

(&)(x::BoolExpression, y::BoolExpression) = AndGate(x, y)
(&)(x::Bool, y::SymbolicBool) = x ? y : false # Short circuit
(&)(x::SymbolicBool, y::Bool) = y & x

(|)(x::BoolExpression, y::BoolExpression) = OrGate(x, y)
(|)(x::Bool, y::SymbolicBool) = x ? true : y # Short circuit
(|)(x::SymbolicBool, y::Bool) = y | x

xor(x::BoolExpression, y::BoolExpression) = XorGate(x, y)
xor(x::Bool, y::SymbolicBool) = x ? ~y : y # Short circuit
xor(x::SymbolicBool, y::Bool) = y ⊻ x

"Little-endian."
struct SymbolicInteger{S} <: Unsigned
    bits::NTuple{S, BoolExpression}
end
function SymbolicInteger{A}(x::SymbolicInteger{B}) where {A, B}
    if B > A
        throw(InexactError(:SymbolicInteger, SymbolicInteger{A}, x))
    elseif B === A
        return x
    else
        return SymbolicInteger{A}((x.bits..., ntuple(i -> false, A - B)...))
    end
end
SymbolicInteger{S}(x::Symbol) where S = SymbolicInteger{S}(ntuple(i -> BoolVariable(x, i), S))
function SymbolicInteger{S}(x::Unsigned) where S
    8sizeof(x) - leading_zeros(x) > S && throw(InexactError(:SymbolicInteger, SymbolicInteger{S}, x))

    SymbolicInteger{S}(convert(NTuple{S, Bool}, tuple(digits!(Vector{Bool}(undef, S), x, base=2)...)))
end
(::Type{SymbolicInteger})(@nospecialize x::SymbolicInteger) = x
(::Type{SymbolicInteger})(x::Unsigned) = SymbolicInteger{8sizeof(x)}(x)

function Base.show(io::IO, x::SymbolicInteger{S}) where S
    try
        print(io, typeof(x), "(0x", string(Unsigned(x), base=16, pad=cld(S, 4)), ")")
    catch
        # TODO: fix pretty-printing
        print(io, x.bits)
    end
end

Base.promote_rule(::Type{SymbolicInteger{A}}, ::Type{SymbolicInteger{B}}) where {A, B}             = SymbolicInteger{max(A, B)}
Base.promote_rule(::Type{SymbolicInteger{A}}, ::Type{B}                 ) where {A, B <: Unsigned} = SymbolicInteger{max(A, 8sizeof(B))}

Base.sizeof(::Type{SymbolicInteger{S}}) where S = S % 8 == 0 ? S ÷ 8 : S / 8
Base.sizeof(::SymbolicInteger{S}) where S = sizeof(SymbolicInteger{S})

Base.ndigits(::SymbolicInteger{S}; base::Integer=10, pad::Integer=1) where S = max(pad, base === 2 ? S : ceil(Int, S/log2(base)))

Base.zero(::Type{SymbolicInteger{S}}) where S = SymbolicInteger{S}(ntuple(i -> false, S))
Base.one( ::Type{SymbolicInteger{S}}) where S = SymbolicInteger{S}(ntuple(i -> i == 1, S))
Base.zero(x::SymbolicInteger) = zero(typeof(x))
Base.one( x::SymbolicInteger) =  one(typeof(x))

Base.leading_zeros( x::SymbolicInteger{S}) where S = S - findlast( bit -> bit !== false, x.bits)
Base.leading_ones(  x::SymbolicInteger{S}) where S = S - findlast( bit -> bit !== true,  x.bits)
Base.trailing_zeros(x::SymbolicInteger{S}) where S =     findfirst(bit -> bit !== false, x.bits) - 1
Base.trailing_ones( x::SymbolicInteger{S}) where S =     findfirst(bit -> bit !== true,  x.bits) - 1

Base.bitrotate(x::SymbolicInteger{S}, k::Integer) where S =
    k != 0 ? SymbolicInteger{S}(ntuple(i -> @inbounds(x.bits[mod1(i - k, S)]), S)) : x
<<(x::SymbolicInteger{S}, k::Int) where S =
    k != 0 ? SymbolicInteger{S}(ntuple(i -> 1 <= i - k <= S && @inbounds(x.bits[i - k]), S)) : x
>>(x::SymbolicInteger, k::Int) = x << -k
>>>(x::SymbolicInteger, k::Int) = x >> k

Base.bitreverse(x::SymbolicInteger{S}) where S = SymbolicInteger{S}(reverse(x.bits))

~(x::SymbolicInteger) = SymbolicInteger((~).(x.bits))
(&)(x::SymbolicInteger, y::SymbolicInteger) = SymbolicInteger((&).(x.bits, y.bits))
(|)(x::SymbolicInteger, y::SymbolicInteger) = SymbolicInteger((|).(x.bits, y.bits))
xor(x::SymbolicInteger, y::SymbolicInteger) = SymbolicInteger(xor.(x.bits, y.bits))

function +(x::SymbolicInteger{S}, y::SymbolicInteger{S})::SymbolicInteger{S} where S
    @inline function full_adder(a::BoolExpression, b::BoolExpression, carry_in::BoolExpression)
        a_xor_b = a ⊻ b
        return a_xor_b ⊻ carry_in, (a_xor_b & carry_in) | (a & b)
    end

    sum = fill!(Vector{BoolExpression}(undef, S), false)
    carry = false
    for i = 1:S
        @inbounds sum[i], carry = full_adder(x.bits[i], y.bits[i], carry)
    end
    return SymbolicInteger{S}(tuple(sum...))
end

*(a::SymbolicInteger{A}, b::SymbolicInteger{B}) where {A, B} =
    sum(SymbolicInteger{A + B}(ntuple(j -> 1 <= j - i + 1 <= A && @inbounds(b.bits[i]) & @inbounds(a.bits[j - i + 1]), A + B)) for i = 1:B) # TODO: can be parallelized

"Two's complement."
-(x::SymbolicInteger) = ~x + oneunit(x)

-(a::Unsigned, b::SymbolicInteger) = -a + b
-(a::SymbolicInteger, b::Unsigned) = a + -b

# Base.abs(x::SymbolicInteger) = SymbolicInteger((|).((&).(x.bits[end], (-x).bits), (&).(~(x.bits[end]), x.bits))) # SymbolicIntegers are unsigned.

==(a::SymbolicInteger{S}, b::SymbolicInteger{S}) where S = reduce(&, (~(a ⊻ b)).bits)
==(a::AbstractArray{<:SymbolicInteger}, b::AbstractArray{<:Unsigned}) = reduce(&, a .== b)
==(a::AbstractArray{<:Unsigned}, b::AbstractArray{<:SymbolicInteger}) = b == a

Base.reinterpret(::Type{SymbolicInteger{S}}, array::AbstractArray{<:SymbolicInteger}) where S = 
    reshape([SymbolicInteger{S}((bits...,))
             for bits in Iterators.partition(Iterators.flatten(getfield.(array, :bits)), S)],
            :, size(array)[2:end]...)
Base.reinterpret(type::Type{<:SymbolicInteger}, array::AbstractArray{<:Unsigned}) = reinterpret(type, SymbolicInteger.(array))
function Base.reinterpret(::Type{U}, array::AbstractArray{T} where {S, SymbolicInteger{S} <: T <: Unsigned}) where {U <: Unsigned}
    symbolic = reinterpret(SymbolicInteger{8sizeof(U)}, array)
    try
        return Vector{U}(symbolic)
    catch
        return symbolic
    end
end

function (::Type{T})(x::SymbolicInteger{S}) where {S, T <: Integer}
    S > 8sizeof(T) && throw(InexactError(Symbol(T), T, x))

    return reinterpret(T, BitVector(x.bits).chunks)[1]
end
Base.BigInt(x::SymbolicInteger) = parse(BigInt, "0b" * join(UInt8.(x.bits)))
function Base.Unsigned(x::SymbolicInteger{S}) where S
    width = max(8, nextpow(2, S))
    if width > 128
        return BigInt(x)
    else
        return getfield(Base, Symbol("UInt", width))(x)
    end
end
Base.Signed(x::SymbolicInteger) = x.bits[end] ? signed(Unsigned(x)) : -signed(Unsigned(-x))
Base.Integer(x::SymbolicInteger) = Unsigned(x)

struct SymbolicBitVector <: AbstractVector{BoolExpression}
    bits::Vector{BoolExpression}
end
Base.size(vec::SymbolicBitVector) = size(vec.bits)
Base.IndexStyle(::Type{SymbolicBitVector}) = IndexLinear()
Base.getindex( vec::SymbolicBitVector,        i::Int) = vec.bits[i]
Base.setindex!(vec::SymbolicBitVector, value, i::Int) = vec.bits[i] = value
Base.vcat(vecs::Union{SymbolicBitVector,BitVector}...) = SymbolicBitVector(invoke(vcat, Tuple{Vararg{AbstractVector}}, vecs...))
Base.resize!(vec::SymbolicBitVector, n::Int) = resize!(vec.bits, n)

function Base.getproperty(vec::SymbolicBitVector, prop::Symbol)
    if prop == :chunks
        return [SymbolicInteger{64}((bits...,))
                for bits in Iterators.partition(Iterators.flatten((vec.bits, zeros(Bool, -rem(length(vec.bits), 64, RoundUp)))), 64)]
    end

    return getfield(vec, prop)
end

function Base.digits!(given::BitVector, n::SymbolicInteger; base::Int=2)
    if -1 >= base >= 1
        throw(DomainError(base, "base must be ≥ 2 or ≤ -2"))
    end

    if abs(base) !== 2
        throw(ArgumentError("type Bool too small for base $base"))
    end

    return SymbolicBitVector([n.bits[1:min(end, length(given))]..., zeros(Bool, max(0, length(given) - length(n.bits)))...])
end

function substitute(expression::BoolExpression,
                    memoized::AbstractDict{Gate, BoolExpression}=WeakKeyDict{Gate, BoolExpression}();
                    substitutions...)
    @inline get_bit(x::Integer, bit) = ((x >>> (bit - 1)) & 0b1) != 0b0
    @inline get_bit(x::SymbolicInteger, bit) = x.bits[bit]

    walk(x::Bool) = x
    function walk(x::BoolVariable)
        substitution = get(substitutions, x.name, missing)
        return ismissing(substitution) ? x : get_bit(substitution, x.bit)
    end
    function walk(gate::Gate{F}) where F
        result = get(memoized, gate, missing)
        if ismissing(result)
            result = memoized[gate] = F(walk.(gate.inputs)...)
        end
        return result
    end

    return walk(expression)
end
substitute(expression::SymbolicInteger,
           memoized::AbstractDict{Gate, BoolExpression}=WeakKeyDict{Gate, BoolExpression}();
           substitutions...) =
               SymbolicInteger(substitute.(expression.bits, Ref(memoized); substitutions...))

end

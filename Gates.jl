module Gates

export BoolVariable, Gate, NotGate, AndGate, OrGate, XorGate, BoolExpression, @ints, SymbolicInteger, substitute

import Base: +, -, *, ==, <<, >>, >>>, ~, &, |, xor

"Basic symbolic unit, representing a specific bit of a named variable."
struct BoolVariable
    name::Symbol
    bit::Int
end
Base.show(io::IO, ::MIME"text/latex", x::BoolVariable) = print(io, x.name, "_", x.bit)
const subscripts = collect("₀₁₂₃₄₅₆₇₈₉")
Base.show(io::IO, ::MIME"text/plain", x::BoolVariable) = print(io, x.name, subscripts[reverse!(digits(x.bit)) .+ 1]...)
Base.isless(x::BoolVariable, y::BoolVariable) = x.name == y.name ? isless(x.bit, y.bit) : isless(x.name, y.name)

"A logic gate which takes N inputs and produces 1 output, using the function F."
mutable struct Gate{F, N} # Mutability is only to avoid copying.
    const inputs::NTuple{N, Union{Gate,BoolVariable}}

    Gate{F, N}(inputs::Vararg{Union{Gate,BoolVariable}, N}) where {F, N} = new{F, N}(inputs)
end

"Convenience definitions for common gate types."
const NotGate = Gate{~, 1}
const AndGate = Gate{&, 2}
const OrGate  = Gate{|, 2}
const XorGate = Gate{⊻, 2}

function Base.show(io::IO, mime::MIME"text/plain", x::Gate)
    first = true
    precedence = typemax(Int)
    function output(operator::Symbol, x::Gate)
        was_first = first
        first = false
        old_precedence = precedence
        precedence = Base.operator_precedence(operator)
        if Base.isunaryoperator(operator)
            print(io, operator)
            output(x.inputs[1])
            return
        end
        if precedence < old_precedence && !was_first
            print(io, "(")
        end
        output(x.inputs[1])
        if precedence < 12
            print(io, " ")
        end
        print(io, operator)
        if precedence < 12
            print(io, " ")
        end
        output(x.inputs[2])
        if precedence < old_precedence && !was_first
            print(io, ")")
        end
        precedence = old_precedence
    end
    output(x::NotGate) = output(:¬, x)
    output(x::AndGate) = output(:∧, x)
    output(x::OrGate ) = output(:∨, x)
    output(x::XorGate) = output(:⊻, x)
    output(x::Union{Bool, BoolVariable}) = show(io, mime, x)

    output(x)
end

"Convert a gate to a GraphViz DOT diagram of the circuit the gate represents."
function Base.show(io::IO, ::MIME"text/vnd.graphviz", x::Gate)
    println(io, "digraph {")
    println(io, "    rankdir=\"LR\";")
    println(io, "}")
end

"Any boolean value that is not yet known."
const SymbolicBool = Union{Gate, BoolVariable}
"An expression that is guaranteed to evaluate to a boolean. May literally be true or false as well."
const BoolExpression = Union{Bool, SymbolicBool}
Base.convert(::Type{BoolExpression}, x::Int) = Bool(x)

# Definitions of logic operators on BoolExpressions as gates.
# The difference between using the operator versus the Gate constructor directly is that the operator may not always return a gate, in case one or both inputs are already known (short-circuiting).

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

"Little-endian collection of BoolExpressions that together represent the bits of an unsigned integer of width S."
struct SymbolicInteger{S} <: Unsigned
    bits::NTuple{S, BoolExpression}
end
function SymbolicInteger{A}(x::SymbolicInteger{B}) where {A, B}
    if B > A
        throw(InexactError(:SymbolicInteger, SymbolicInteger{A}, x))
    elseif B === A
        return x
    else
        return SymbolicInteger{A}((x.bits..., ntuple(i -> false, Val(A - B))...))
    end
end
SymbolicInteger{S}(x::Symbol) where S = SymbolicInteger{S}(ntuple(i -> BoolVariable(x, i), Val(S)))
function SymbolicInteger{S}(x::Unsigned) where S
    8sizeof(x) - leading_zeros(x) > S && throw(InexactError(:SymbolicInteger, SymbolicInteger{S}, x))

    SymbolicInteger{S}(convert(NTuple{S, Bool}, tuple(digits!(Vector{Bool}(undef, S), x, base=2)...)))
end
(::Type{SymbolicInteger})(@nospecialize x::SymbolicInteger) = x
(::Type{SymbolicInteger})(x::Unsigned) = SymbolicInteger{8sizeof(x)}(x)

"Alternate syntax to access the i-th bit of x, since getindex is already used by all Numbers."
(x::SymbolicInteger)(i::Int) = x.bits[i]

function Base.show(io::IO, ::MIME"text/plain", x::SymbolicInteger{S}) where S
    try
        print(io, typeof(x), "(0x", string(Unsigned(x), base=16, pad=cld(S, 4)), ")")
    catch
        matrix = collect(x.bits)
        pre = " "
        if get(io, :compact, false)
            matrix = reshape(matrix, 1, :)
            pre = ""
        else
            println(io, S, "-bit SymbolicInteger:")
        end
        Base.print_matrix(io, matrix, pre)
    end
end
Base.show(io::IO, x::SymbolicInteger) = show(IOContext(io, :compact => true), MIME("text/plain"), x)

macro ints(declarations...)
    function declare(declaration)
        if !Meta.isexpr(declaration, :(::))
            error("usage: @symints x::32 y::5 to define x = SymbolicInteger{32}(:x); y = SymbolicInteger{5}(:y)")
        end
        name, width = declaration.args
        :($name = SymbolicInteger{$width}($(Meta.quot(name)));)
    end
    quote
        $(declare.(declarations)...)
        nothing
    end |> esc
end

Base.BroadcastStyle(::Type{<:SymbolicInteger}) = Broadcast.Style{SymbolicInteger}()
Base.BroadcastStyle( ::Broadcast.AbstractArrayStyle{0}, i::Broadcast.Style{SymbolicInteger}) = i
Base.BroadcastStyle(a::Broadcast.AbstractArrayStyle   ,  ::Broadcast.Style{SymbolicInteger}) = a
function Base.copy(bc::Broadcast.Broadcasted{Broadcast.Style{SymbolicInteger}})
    bcf = Broadcast.flatten(bc)
    bits(x::SymbolicInteger) = x.bits
    bits(x::Ref) = x
    SymbolicInteger(convert(Tuple{Vararg{BoolExpression}}, bcf.f.(bits.(bcf.args)...)))
end

# When dealing with multiple SymbolicIntegers, promote to the largest width to avoid losing any information.
Base.promote_rule(::Type{SymbolicInteger{A}}, ::Type{SymbolicInteger{B}}) where {A, B}             = SymbolicInteger{max(A, B)}
Base.promote_rule(::Type{SymbolicInteger{A}}, ::Type{B}                 ) where {A, B <: Unsigned} = SymbolicInteger{max(A, 8sizeof(B))}

Base.sizeof(::Type{SymbolicInteger{S}}) where S = S % 8 == 0 ? S ÷ 8 : S / 8
Base.sizeof(::SymbolicInteger{S}) where S = sizeof(SymbolicInteger{S})

Base.ndigits(::SymbolicInteger{S}; base::Integer=10, pad::Integer=1) where S = max(pad, base === 2 ? S : ceil(Int, S/log2(base)))

Base.zero(::Type{SymbolicInteger{S}}) where S = SymbolicInteger{S}(ntuple(i -> false,  Val(S)))
Base.one( ::Type{SymbolicInteger{S}}) where S = SymbolicInteger{S}(ntuple(i -> i == 1, Val(S)))
Base.zero(x::SymbolicInteger) = zero(typeof(x))
Base.one( x::SymbolicInteger) =  one(typeof(x))

Base.leading_zeros( x::SymbolicInteger{S}) where S = S - findlast( bit -> bit !== false, x.bits)
Base.leading_ones(  x::SymbolicInteger{S}) where S = S - findlast( bit -> bit !== true,  x.bits)
Base.trailing_zeros(x::SymbolicInteger{S}) where S =     findfirst(bit -> bit !== false, x.bits) - 1
Base.trailing_ones( x::SymbolicInteger{S}) where S =     findfirst(bit -> bit !== true,  x.bits) - 1

Base.bitrotate(x::SymbolicInteger{S}, k::Integer) where S =
    k != 0 ? SymbolicInteger{S}(ntuple(i -> @inbounds(x.bits[mod1(i - k, S)]), Val(S))) : x
<<(x::SymbolicInteger{S}, k::Int) where S =
    k != 0 ? SymbolicInteger{S}(ntuple(i -> 1 <= i - k <= S && @inbounds(x.bits[i - k]), Val(S))) : x
>>(x::SymbolicInteger, k::Int) = x << -k
>>>(x::SymbolicInteger, k::Int) = x >> k

Base.bitreverse(x::SymbolicInteger{S}) where S = SymbolicInteger{S}(reverse(x.bits))

~(x::SymbolicInteger) = .~x
(&)(x::SymbolicInteger, y::SymbolicInteger) = x .& y
(|)(x::SymbolicInteger, y::SymbolicInteger) = x .| y
xor(x::SymbolicInteger, y::SymbolicInteger) = x .⊻ y

function +(x::SymbolicInteger{S}, y::SymbolicInteger{S}) where S
    @inline function full_adder(a::BoolExpression, b::BoolExpression, carry_in::BoolExpression)
        a_xor_b = a ⊻ b
        return a_xor_b ⊻ carry_in, (a_xor_b & carry_in) | (a & b)
    end

    sum = Vector{BoolExpression}(undef, S)
    carry = false
    for i = 1:S
        @inbounds sum[i], carry = full_adder(x.bits[i], y.bits[i], carry)
    end
    return SymbolicInteger{S}(tuple(sum...))
end

*(a::SymbolicInteger{A}, b::SymbolicInteger{B}) where {A, B} =
    sum(SymbolicInteger{A + B}(ntuple(j -> 1 <= j - i + 1 <= A && @inbounds(b.bits[i]) & @inbounds(a.bits[j - i + 1]), Val(A + B))) for i = 1:B) # TODO: can be parallelized

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

substitutions(bits::Pair{BoolVariable}...; names...) =
    Dict((BoolVariable(name, bit) => substitution
          for (name, value) in names
              for (bit, substitution) in enumerate(SymbolicInteger(value).bits))...,
         bits...)
function substitute(expression::BoolExpression,
                    memoized::AbstractDict{Gate, BoolExpression},
                    bits...;
                    names...)
    subs = substitutions(bits...; names...)

    walk(x::Bool) = x
    walk(x::BoolVariable) = get(subs, x, x)
    walk(gate::Gate{F}) where F = get!(memoized, gate) do
        F(walk.(gate.inputs)...)
    end

    return walk(expression)
end
substitute(expression::SymbolicInteger, memoized::AbstractDict{Gate, BoolExpression}, bits...; names...) =
    substitute.(expression, Ref(memoized), bits...; names...)
substitute(expression, bits::Pair{BoolVariable}...; names...) =
    substitute(expression, WeakKeyDict{Gate, BoolExpression}(), bits...; names...)

# include("bristol.jl")

end

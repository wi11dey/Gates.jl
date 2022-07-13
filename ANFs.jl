module ANFs

export ANF

import Base: +, *

    using ..Gates

const Term = Set{BoolVariable}

struct ANF
    terms::Set{Term}
end
ANF(anf::ANF)        = ANF(Set(Set.(anf.terms)))
ANF(x::Val{true })   = ANF(Set((Term(),)))
ANF(x::Val{false})   = ANF(Set{Term}())
ANF(x::Bool)         = ANF(Val(x))
ANF(x::Int)          = ANF(Bool(x))
ANF(x::BoolVariable) = ANF(Set((Term((x,)),)))

+(a::ANF, b::ANF) = ANF(setdiff!(a.terms ∪ b.terms, a.terms ∩ b.terms))
function *(a::ANF, b::ANF)
    product = Set{Term}()
    for i in a.terms, j in b.terms
        term = i ∪ j
        (term in product ? delete! : push!)(product, term)
    end
    ANF(product)
end

function ANF(x::Gate)
    memoized = WeakKeyDict{Gate, ANF}()
    walk(x::Union{Bool, BoolVariable}) = ANF(x)
    walk(gate::Gate) = get!(memoized, gate) do
        transform(::NotGate, a   ) = ANF(Val(true)) + a
        transform(::AndGate, a, b) = a*b
        transform(::OrGate , a, b) = a + b + a*b
        transform(::XorGate, a, b) = a + b

        transform(gate, walk.(gate.inputs)...)
    end

    walk(x)
end

Gates.BoolExpression(anf::ANF) = mapfoldl(⊻, sort!(collect(anf.terms), by=length), init=false) do term
    foldl(&, sort!(collect(term)), init=true)
end
Base.convert(::Type{BoolExpression}, anf::ANF) = BoolExpression(anf)

function _show(io::IO, mime::MIME, anf::ANF)
    compact = get(io, :compact, false)
    if !compact
        println(io, length(anf.terms), "-term ANF:")
        print(io, " ")
    end
    terms = sort!(collect(anf.terms), by=length)
    if isempty(terms)
        print(io, "0")
    end
    degree = -1
    for term in terms
        newdegree = length(term)
        if degree >= 0
            if newdegree == degree || compact
                print(io, " + ")
            else
                println(io, " +")
                print(io, " ")
            end
        end
        if isempty(term)
            print(io, "1")
        end
        for var in sort!(collect(term))
            show(io, mime, var)
        end
        degree = newdegree
    end
end
Base.show(io::IO, mime::MIME,             anf::ANF) = _show(io, mime, anf)
Base.show(io::IO, mime::MIME"text/plain", anf::ANF) = _show(io, mime, anf)
function Base.show(io::IO, anf::ANF)
    print(io, "ANF(")
    _show(IOContext(io, :compact => true), MIME("text/plain"), anf)
    print(io, ")")
end

function Gates.substitute(anf::ANF, bits::Pair{BoolVariable}...; names...)
    subs = Gates.substitutions(bits...; names...)
    reduce(+, mapreduce.(var -> ANF(get(subs, var, var)), *, anf.terms, init=ANF(Val(true))), init=ANF(Val(false)))
end

end

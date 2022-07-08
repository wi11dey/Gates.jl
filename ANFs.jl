const Term = Set{BoolVariable}

mutable struct ANF
    terms::Set{Term}
end

function ANF(x::Gate)
    # TODO: can be parallelized
    const \ = setdiff
    function ⊕(a::Set{Term}, b::Set{Term})
        like = a ∩ b
        (a \ like) ∪ (b \ like)
    end
    function *(a::Set{Term}, b::Set{Term})
        product = Set{Term}()
        for i in a, j in b
            term = i ∪ j
            (term in product ? remove! : push!)(product, term)
        end
        product
    end

    memoized = WeakKeyDict{Gate, Set{Term}}()
    walk(x::Val{true}) = Set(Set())
    walk(x::Val{false}) = Set()
    walk(x::Bool) = walk(Val(x))
    walk(x::BoolVariable) = Set(Set((x,)))
    walk(gate::NotGate) = get!(memoized, gate) do
        walk(true) ⊕ walk(gate.inputs[0])
    end
    walk(gate::OrGate) = get!(memoized, gate) do
        a, b = walk.(gate.inputs)
        a ⊕ b ⊕ a * b
    end
    walk(gate::AndGate) = get!(memoized, gate) do
        a, b = walk.(gate.inputs)
        a * b
    end
    walk(gate::XorGate) = get!(memoized, gate) do
        a, b = walk.(gate.inputs)
        a ⊕ b
    end

    ANF(walk(gate))
end

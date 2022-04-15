module CNFs

export CNF

using ..Circuits

mutable struct CNF
    clauses::Matrix{Int}
    inputs::Dict{Symbol,Dict{Int,Int}}
    size::Int
end
CNF(cnf::CNF) = CNF(Matrix(cnf.clauses), Dict(cnf.inputs), cnf.size)

"Tseytin transformation into 3CNF."
function CNF(x::Gate)
    flat_clauses = Int[]
    wires = IdDict{SymbolicBool,Int}()
    inputs = Dict{Symbol,Dict{Int,Int}}()

    walk(x::BoolVariable) = get!(wires, x) do
        get!(inputs, x.name, Dict{Int,Int}())[x.bit] = length(wires) + 1
    end
    walk(gate::Gate) = get!(wires, gate) do
        walked = walk.(gate.inputs)
        c = length(wires) + 1
        tseytin(::NotGate, a)    = push!(flat_clauses,
                                         -a, -c, 0,
                                         a,   c, 0)
        tseytin(::AndGate, a, b) = push!(flat_clauses,
                                         -a, -b, c,
                                         a,  -c, 0,
                                         b,  -c, 0)
        tseytin(::OrGate, a, b)  = push!(flat_clauses,
                                         a,  b, -c,
                                         -a, c,  0,
                                         -b, c,  0)
        tseytin(::XorGate, a, b) = push!(flat_clauses,
                                         -a, -b, -c,
                                         a,   b, -c,
                                         a,  -b,  c,
                                         -a,  b,  c)
        tseytin(gate, walked...)
    end

    push!(flat_clauses, walk(x), 0, 0)

    return CNF(reshape(flat_clauses, 3, :), inputs, length(wires))
end

function map!(f, cnf::CNF)
    map!(v -> copysign(f(abs(v)), v), cnf.clauses)
    map!(input -> map!(f, values(input)), values(cnf.inputs))
    return cnf
end

"Modify input CNF to remove variables that are not used in any clause."
function compress!(cnf::CNF)
    vars = abs.(vec(cnf.clauses))
    sort!(vars)
    pushfirst!(vars, 0)
    unique!(vars)
    popfirst!(vars)
    remapping = Dict(zip(vars, eachindex(vars)))
    map!(v -> get(remapping, v, zero(v)), cnf)
    map!(values(cnf.inputs)) do input
        filter!(pair -> !iszero(pair.second), input)
    end

    cnf.size = length(vars)

    return cnf
end

"""
Removes repeated variables in the same clause.
"""
function deduplicate!(cnf::CNF)
    clauses = eachcol(cnf.clauses)
    remaining = trues(length(clauses))
    for (i, clause) in enumerate(clauses),
        (j, var) in enumerate(clause)
        rest = @view clause[j + 1:end]
        if -var in rest
            remaining[i] = false
            continue
        end
        replace!(rest, var => 0)
    end

    cnf.clauses = cnf.clauses[:, remaining]

    return cnf
end

"2SAT simplification."
function simplify(cnf::CNF)
end

using Random

function Random.shuffle!(rng::AbstractRNG, cnf::CNF)
    perm = randperm(rng, cnf.size)
    map!(v -> perm[v], cnf)
    shuffle!.(rng, eachcol(cnf.clauses))
    cnf.clauses .= cnf.clauses[:, randperm(rng, size(cnf.clauses, 2))]
    return cnf
end

end

module CNFs

export CNF

using ..Circuits

struct CNF
    clauses::Matrix{Int}
    inputs::Dict{Symbol,AbstractDict{Int,Int}}
    size::Int
    last_input_variable::Int
end
CNF(cnf::CNF) = CNF(Matrix(cnf.clauses), Dict(cnf.inputs), cnf.size, cnf.last_input_variable)

"Tseytin transformation into 3CNF."
function CNF(x::Gate)
    flat_clauses = Vector{Union{Pair{BoolVariable,Bool},Nothing}}()

    tseytin(::NotGate, c, a)    = (push!(flat_clauses,
                                         a => false, c => false, nothing,
                                         a => true,  c => true,  nothing);
                                   c)
    tseytin(::AndGate, c, a, b) = (push!(flat_clauses,
                                         a => false, b => false, c => true,
                                         a => true,  c => false, nothing,
                                         b => true,  c => false, nothing);
                                   c)
    tseytin(::OrGate, c, a, b)  = (push!(flat_clauses,
                                         a => true,  b => true,  c => false,
                                         a => false, c => true,  nothing,
                                         b => false, c => true,  nothing);
                                   c)
    tseytin(::XorGate, c, a, b) = (push!(flat_clauses,
                                         a => false, b => false, c => false,
                                         a => true,  b => true,  c => false,
                                         a => true,  b => false, c => true,
                                         a => false, b => true,  c => true);
                                   c)

    sizes = Dict{Symbol,Int}()
    gates = WeakKeyDict{Gate,BoolVariable}()
    @gensym gatesym

    walk(x::BoolVariable) = (sizes[x.name] = max(get(sizes, x.name, 1), x.bit); x)
    function walk(gate::Gate)
        result = get(gates, gate, missing)
        if ismissing(result)
            walked = walk.(gate.inputs)
            result = gates[gate] = tseytin(gate, BoolVariable(gatesym, length(gates) + 1), walked...)
        end
        return result
    end

    push!(flat_clauses, walk(x) => true, nothing, nothing)

    inputs = Dict{Symbol,AbstractDict{Int,Int}}()
    i = 1
    for (input, size) in sizes
        inputs[input] = pairs(i:i + size - 1)
        i += size
    end
    inputs[gatesym] = pairs(i:i + length(gates) - 1)
    clauses = map(v -> isnothing(v) ? 0 : (v.second ? 1 : -1)*inputs[v.first.name][v.first.bit], reshape(flat_clauses, 3, :))
    delete!(inputs, gatesym)

    return CNF(clauses, inputs, i + length(gates) - 1, i - 1)
end

"""
Removes repeated variables in the same clause.

Modifies input CNF to remove repeated literals, and additionally returns a new CNF with no repeated literals and no trivially true clauses (with A ∨ ¬A).
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
        replace!(rest, var=>0)
    end

    return CNF(cnf.clauses[:, remaining],
               cnf.inputs,
               cnf.size,
               cnf.last_input_variable)
end

"Modify input CNF to remove variables that are not used in any clause, and return a "
function compress!(cnf::CNF)
    vars = abs.(vec(cnf.clauses))
    pushfirst!(vars, 0)
    sort!(vars)
    unique!(vars)
    popfirst!(vars)
    remap = Dict(Iterators.flatten(zip(vars, eachindex(vars)),
                                   Iterators.map(.-, zip(vars, eachindex(vars)))))
    replace!(cnf.clauses, remap...)

    map!(values(cnf.inputs)) do input
        filter!(pair -> pair.second !== 0, map!(v -> get(remap, v, 0), values(convert(Dict, input))))
    end

    return CNF(cnf.clauses,
               cnf.inputs,
               length(vars),
               cnf.last_input_variable)
end

using Random

function Random.shuffle!(rng::AbstractRNG, cnf::CNF)::CNF
    shuffle!.(rng, eachcol(cnf.clauses))
    cnf.clauses .= cnf.clauses[:, randperm(rng, size(cnf.clauses, 2))]
    return cnf
end

end

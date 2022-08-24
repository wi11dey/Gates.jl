module CNFs

export CNF

using ..Gates

mutable struct CNF
    clauses::Matrix{Int}
    inputs::Dict{Symbol,Dict{Int,Int}}
    size::Int
end
function Bool(cnf::CNF)
    length(cnf.clauses) == 0 && return true
    any(all.(iszero, eachcol(cnf.clauses))) && return false
    throw(InexactError(:Bool, Bool, cnf))
end

"Tseytin transformation into 3CNF."
function CNF(x::Gate)
    flat_clauses = Int[]
    wires = IdDict{Gates.SymbolicBool,Int}()
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
        c
    end

    push!(flat_clauses, walk(x), 0, 0)

    return CNF(reshape(flat_clauses, 3, :), inputs, length(wires))
end

function Base.show(io::IO, ::MIME"text/plain", cnf::CNF)
    println(io, size(cnf.clauses, 2), "-clause, ", cnf.size, "-literal CNF:")
    buffer = IOBuffer()
    Base.print_matrix(IOContext(buffer), cnf.clauses', "", " ∨ ")
    seek(buffer, 0)
    join(io, (" ($clause)" for clause in eachline(buffer)), " ∧\n")
    close(buffer)
end

function Base.map!(f, cnf::CNF)
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

"Removes repeated variables in the same clause."
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

    if length(cnf.clauses) == 0
        empty!(cnf.inputs)
        cnf.size = 0
    end

    return cnf
end

"Simplification procedure I generalized from the 2SAT satisfiability algorithm. Guaranteed to solve instand"
function simplify!(cnf::CNF)
    deduplicate!(cnf)

    locations = Dict{Int,Vector{CartesianIndex}}()
    for (location, v) in pairs(IndexCartesian(), cnf.clauses)
        v != 0 || continue
        push!(get!(locations, v, CartesianIndex[]), location)
    end

    remaining = trues(length(clauses))
    assignments = Dict{Int,Bool}()
    Base.@propagate_inbounds function dfs(clause::Int)
        v = 0
        for w in @view cnf.clauses[:, clause]
            w != 0 || continue
            v == 0 || return true
            v = w
        end
        v != 0 || return false
        get!(assignments, abs(v), v > 0) == (v > 0) || return false
        remaining[getindex.(locations[v], 1)] .= false
        for i in locations[-v]
            col = i[1]
            remaining[col] || continue
            cnf.clauses[i] = 0
            dfs(col) || return false
        end
        return true
    end
    @inbounds for i = axes(cnf.clauses, 2)
        remaining[i] || continue
        if !dfs(i)
            cnf.clauses = zeros(Int, 1, 1)
            empty!(cnf.inputs)
            cnf.size = 0
            return cnf, Dict{BoolVariable,Bool}()
        end
    end
    cnf.clauses = cnf.clauses[:, remaining]

    partial = Dict{BoolVariable,Bool}()
    for (name, bits) in cnf.inputs,
        (bit, v) in bits
        assignment = get(assignments, v, missing)
        !ismissing(assignment) || continue
        partial[BoolVariable(name, bit)] = assignment
    end

    compress!(cnf)

    return cnf, partial
end

using Random

function Random.shuffle!(rng::AbstractRNG, cnf::CNF)
    perm = randperm(rng, cnf.size)
    map!(v -> perm[v], cnf)
    shuffle!.(rng, eachcol(cnf.clauses))
    cnf.clauses .= cnf.clauses[:, randperm(rng, size(cnf.clauses, 2))]
    return cnf
end

import PicoSAT

for f = (:solve, :itersolve)
    @eval PicoSAT.$f(cnf::CNF; args...) = PicoSAT.$f(Iterators.map(clause -> Iterators.filter(!=(0), clause),
                                                                   eachcol(cnf.clauses));
                                                     vars=cnf.size,
                                                     args...)
end

end

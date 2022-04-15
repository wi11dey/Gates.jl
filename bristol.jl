export readbristol

# (Old) Bristol Format: https://homes.esat.kuleuven.be/~nsmart/MPC/old-circuits.html
# Hand-optimized Bristol Format: http://stevengoldfeder.com/projects/circuits/sha2circuit.html
# New Bristol Fashion: https://homes.esat.kuleuven.be/~nsmart/MPC/

struct BristolParseError <: Exception
    line::Int
    msg::String
end

function showerror(io::IO, ex::BristolParseError)
    print(io, "BristolParseError: line $ex.line: $ex.msg")
end

function readbristol(io::IO, inputs::Vector{<:SymbolicInteger})
    const bristolmap = Dict{String, Type{<:Gate}}("AND" => &, "XOR" => ⊻, "INV" => ~)

    lines = Iterators.Stateful(eachline(io))
    @inline parseerror(msg::String) = throw(BristolParseError(lines.taken, msg))

    # Skip gate counts:
    try
        popfirst!(lines)
    catch e
        if e isa EOFError
            parseerror("input ended before header ended")
        else
            rethrow(e)
        end
    end

    # Slurp header:
    try
        header = broadcast.(parse, Int, split.(Iterators.takewhile(line -> occursin(r"^(?:\d+\s*)+$", line), lines)))
    catch e
        if e isa ArgumentError
            parseerror("incorrect header format")
        else
            rethrow(e)
        end
    end

    # Check header:
    @inline width(i::Int) = i <= length(inputs) ? length(inputs[i].bits) : 0
    if length(header) == 1
        @info "Reading old-style Bristol format circuit"
        length(header[]) == 3 || parseerror("incorrect Bristol format header format")
        width.(1:2) == header[][1:2] || parseerror("expected inputs of length $(width.[1:2]), but got inputs of length $(header[][1:2])")
        outwires = header[][3:3]
    elseif length(header) == 2
        @info "Reading new-style Bristol fashion circuit"
        length(inputs) == header[1][1] || parseerror("expected $(length(inputs)) inputs, but input count is $(header[1][1])")
        width.(eachindex(inputs)) == header[1][2:end] || parseerror("expected inputs of length $(width(eachindex(inputs))), but got inputs of length $(header[1][2:end])")
        outwires = header[2][2:end]
        length(outwires) == header[2][1] || parseerror("output count does not match number of outputs")
    elseif length(header) == 0
        parseerror("no header")
    else
        parseerror("too many lines in header")
    end

    # Parse circuit:
    wires = Dict{Int, BoolVariable}(zip(Iterators.countfrom(0), Iterators.flatten(getfield.(inputs, :bits))))
    try
        for line in Iterators.dropwhile(isempty, lines)
            tokens = split(line)
            parameters = parse.(Int, tokens[begin:end - 1])
            gate = uppercase(tokens[end])

            if gate == "MAND"
                setindex!.(Ref(wires),
                           parameters[end - parameters[2] + 1:end],
                           Base.splat(AndGate).(Iterators.partition(getindex.(Ref(wires), parameters[3:3 + parameters[1] - 1]),
                                                                    ninputs(AndGate)))...)
                continue
            end

            parameters[2] == 1 || parseerror("expected 1 output from $gate gate, got $(parameters[2])")

            if gate == "EQ"
                wires[parameters[end]] = Bool(parameters[3])
                continue
            end

            if gate == "EQW"
                wires[parameters[end]] = wires[parameters[3]]
                continue
            end

            f = get(bristolmap, gate) do
                parseerror("unknown gate $gate")
            end
            wires[parameters[end]] = f(getindex.(Ref(wires), parameters[3:3 + parameters[1] - 1])...)
        end
    catch e
        if e isa KeyError
            parseerror("no wire $(e.key) defined yet")
        elseif e isa InexactError && e.T <: Bool
            parseerror("expected input to be either 0 or 1, but got $(e.val)")
        elseif e isa BoundsError
            parseerror("too few parameters")
        elseif e isa DimensionMismatch
            parseerror("mismatched number of inputs and outputs")
        elseif e isa MethodError
            parseerror("wrong number of inputs")
        else
            rethrow(e)
        end
    end

    # Output:
    i = length(wires)
    outputs = SymbolicInteger[]
    for bits in Iterators.reverse(outwires)
        outputs.pushfirst!(SymbolicInteger{bits}(ntuple(j -> get(() -> parseerror("not enough wires for outputs"), wires, i - bits + j), bits)))
        i -= bits
    end
    return outputs
end

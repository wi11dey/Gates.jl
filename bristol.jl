export readbristol

# (Old) Bristol Format: https://homes.esat.kuleuven.be/~nsmart/MPC/old-circuits.html
# Hand-optimized Bristol Format: http://stevengoldfeder.com/projects/circuits/sha2circuit.html
# New Bristol Fashion: https://homes.esat.kuleuven.be/~nsmart/MPC/

# TODO parse error type
const bristolmap = Dict{String, Type{<:Gate}}("AND" => AndGate, "XOR" => XorGate, "INV" => NotGate)

# TODO error handling
function readbristol(io::IO, inputs::Vector{<:SymbolicInteger})
    ninputs(::Type{<:Gate{F, N}}) where {F, N} = N

    lines = Iterators.Stateful(eachline(io))
    popfirst!(lines)

    # Slurp header:
    header = broadcast.(parse, Int, split.(Iterators.takewhile(line -> occursin(r"^(?:\d+\s*)+$", line), lines)))

    # Check header:
    if length(header) == 1
        # Bristol format:
        if length(inputs) == 1
            header[][1] == length(inputs[1].bits) && header[][2] == 0 || throw(ArgumentError("a"))
        elseif length(inputs) == 2
            length.(getfield.(inputs, :bits)) == header[][1:2] || throw(ArgumentError("a"))
        end
        outwires = header[][3:3]
    elseif length(header) == 2
        # Bristol fashion:
        length.(getfield.(inputs, :bits)) == header[1][2:end] || throw(ArgumentError("a"))
        outwires = header[2][2:end]
    else
        throw(ArgumentError("incorrect header format"))
    end

    # Parse circuit:
    wires = Dict{Int, BoolVariable}(zip(Iterators.countfrom(0), Iterators.flatten(getfield.(inputs, :bits))))
    for line in Iterators.dropwhile(isempty, lines)
        tokens = split(line)
        parameters = parse.(Int, tokens[begin:end - 1])
        gate = uppercase(tokens[end])
        if gate == "MAND"
            setindex!.(Ref(wires),
                       parameters[end - parameters[2] + 1:end],
                       Base.splat(AndGate).(Iterators.partition(parameters[3:3 + parameters[1] - 1],
                                                                ninputs(AndGate)))...)
        elseif gate == "EQ"
            wires[parameters[end]] = Bool(parameters[3])
        elseif gate == "EQW"
            wires[parameters[end]] = wires[parameters[3]]
        else
            parameters[2] == 1 || throw(ArgumentError("expected 1 output from $gate gate, received $parameters[2]"))
            gatetype = bristolmap[gate]
            in = ninputs(gatetype)
            parameters[1] == in || throw(ArgumentError("expected $in inputs from $gate gate, received $parameters[1]"))
            wires[parameters[end]] = gatetype(getindex.(Ref(wires), parameters[3:3 + in - 1])...)
        end
    end

    # Output:
    i = length(wires)
    outputs = SymbolicInteger[]
    for S in Iterators.reverse(outwires)
        outputs.pushfirst!(SymbolicInteger{S}((getindex.(Ref(wires), i - S:i)...,)))
        i -= S
    end
    return outputs
end

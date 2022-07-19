# TODO: Port https://github.com/IainNZ/GraphLayout.jl to modern Julia as a NetworkLayout.jl implementation, then use that for layout
# XXX: Consider xdot output?

module GatePlots

using ..Gates
using RecipesBase
import GraphRecipes: GraphPlot, graphplot
using LaTeXStrings

const Point64 = NTuple{2, Float64}

arc!(path::Vector{Point64}, (x, y)::Point64, radius::Float64, radianspi::AbstractRange) =
    append!(path, reim.((x + y*im) .+ radius .* cispi.(radianspi)))

struct GateShape{F} <: Function end
GateShape(::Gate{F}) where F = GateShape{~}()

function (::GateShape{~})(x, y, w, h)
    r = 0.1min(w, h)
    halfw = w/2
    halfh = h/2
    left, right = x - halfw, x + halfw
    result = [
        (left, y - halfh),
        (left, y + halfh),
        (right - 2r, y)
    ]
    arc!(result, (right - r, y), r, LinRange(1, -1, 10))
    push!(result, result[begin])
    result
end
function (::GateShape{&})(x, y, w, h)
end
# How to draw: https://spinningnumbers.org/a/logic-gates.html
function (::GateShape{|})()
end
function (::GateShape{⊻})()
end

@recipe function f(x::Gate; outnode=nothing)
    wires = IdDict{Gates.SymbolicBool,Int}()

    source = Int[]
    destiny = Int[]
    names = AbstractString[]
    shapes = Union{GateShape, Symbol}[]
    strokealpha = Float64[]

    walk(x::BoolVariable) = get!(wires, x) do
        wire = length(wires) + 1

        push!(names, latexstring(sprint(show, MIME("text/latex"), x)))
        push!(shapes, :circle)
        push!(strokealpha, 0)

        wire
    end
    function walk(gate::Gate)
        wire = get!(wires, gate) do
            push!(names, "  ")
            push!(shapes, GateShape(gate))
            push!(strokealpha, 1)

            length(wires) + 1
        end

        append!(source, fill(wire, axes(gate.inputs)))
        append!(destiny, walk.(gate.inputs))

        wire
    end

    if !isnothing(outnode)
        wires[gensym()] = 1

        push!(names, outnode)
        push!(shapes, :rect)
        push!(strokealpha, 0)
        push!(source,  1)
        push!(destiny, 2)
    end

    walk(x)

    names := names
    nodeshape := shapes
    markerstrokealpha := strokealpha
    method --> :buchheim
    root --> :right
    nodecolor --> :white
    GraphPlot((source, destiny))
end

graphplot(x::Gate) = plot(x)

end

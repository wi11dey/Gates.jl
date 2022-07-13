module GatePlots

using GraphRecipes
using Luxor
using ..Gates

const Point64 = NTuple{2, Float64}

function arc!(path::Vector{Point64}, center::Point64, radius::Float64, start::Int, stop::Int, resolution=10)
end

struct GateShape{F} <: Function end
GateShape(::Gate{F}) where F = GateShape{F}()

function (::GateShape{~})(x, y, width, height)
    r = (width + height)/2*0.1
    result = [
        (x - width/2, y - height/2),
        (x - width/2, y + height/2),
        (x + width/2 - 2r, y)
    ]
    arc!(result, (x + width/2 - r, y), r, -180, 180)
    push!(result, (x - width/2, y - height/2))
    result
end
function (::GateShape{&})()
    result
end
# How to draw: https://spinningnumbers.org/a/logic-gates.html
function (::GateShape{|})()
end
function (::GateShape{⊻})()
end

@recipe function f(gate::Gate)
    source = Int[]
    destiny = Int[]
    nodeshapes = GateShape[]

    nodeshape --> nodeshapes
    method --> :buchheim
    root --> :right
    fillcolor --> :white
    linecolor --> :black
    GraphPlot((source, destiny))
end

end

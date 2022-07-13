module GateTrees

using AbstractTrees
using ..Gates

AbstractTrees.children(gate::Gate) = gate.inputs
AbstractTrees.nodevalue(gate::Gate{F}) where F = F
AbstractTrees.childrentype(gate::Gate) = typeof(gate.inputs)
AbstractTrees.childrentype(::Type{Gate{F, N}}) where {F, N} = NTuple{N, Gate.SymbolicBool}
AbstractTrees.ChildIndexing(::Type{<:Gate}) = IndexedChildren()

end

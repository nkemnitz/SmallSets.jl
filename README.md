*Currently only works for bitstype*

SmallSets mimics the behaviour of a regular Set, but relies on a [https://github.com/JuliaArrays/StaticArrays.jl](StaticArrays.jl) `MVector` (T is bitstype) or plain `Vector` (not a bitstype) that will be used as long as the set consists of <= N elements. Once additional elements are added, a regular `Set` is used. 

SmallSet might offer better performance if you are dealing with a huge amount of Sets, of which most will only contain N elements or below.


```Pkg.clone("https://github.com/nkemnitz/SmallSets.jl")```

```julia
using BenchmarkTools, Compat # for benchmarking
using SmallSets



```
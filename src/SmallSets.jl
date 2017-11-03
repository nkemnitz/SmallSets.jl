module SmallSets

using StaticArrays

import Base: show_vector, join_eltype, issubset,
             _default_eltype, grow_to!, eltype,
             similar, show, isempty, length, in,
             push!, pop!,
             delete!, sizehint!, empty!, rehash!,
             start, done, next,
             union, union!, intersect, setdiff!,
             ==, <, <=

export SmallSet, issmall

mutable struct SmallSet{N, T} <: Base.AbstractSet{T} where N <: Integer
  vsize::Int
  vector::MVector{N, T} # TODO Union Vector/MVector slows down everything...
  set::Union{Set{T}, Void}

  SmallSet{N, T}() where {N, T} = begin
    if N > 32
      warn("Consider using Set when using N > 32.")
    end
    new(0, MVector{N, T}(), nothing)
  end
end

SmallSet{N}() where N <: Integer = SmallSet{N, Any}()
SmallSet{N}(itr) where {N} = SmallSet{N, eltype(itr)}(itr)
SmallSet{N, T}(itr) where {N, T} = union!(SmallSet{N, T}(), itr)
function SmallSet{N}(g::Base.Generator) where {N}
  T = _default_eltype(typeof(g))
  (isleaftype(T) || T === Union{}) || return grow_to!(SmallSet{N, T}(), g)
  return SmallSet{N, T}(g)
end

# private
join_sizes() = 0
join_sizes(v1::SmallSet{N, T}, vs::SmallSet...) where {N, T} = max(N, join_sizes(vs...))

# public
issmall(s::SmallSet) = typeof(s.set) === Void
eltype(::Type{SmallSet{N, T}}) where {N, T} = T
similar(s::SmallSet{N, T}) where {N, T} = SmallSet{N, T}()
similar(s::SmallSet{N, T}, NT::Type) where {N, T} = SmallSet{N, NT}()

function show(io::IO, s::SmallSet{N, T}) where {N, T}
  print(io, "SmallSet")
  if isempty(s)
    print(io, "{", N, ",", T, "}()")
    return
  end
  print(io,"(")
  show_vector(io, issmall(s) ? s.vector[1 : s.vsize] : s.set, "[","]") # FIXME (currently creates copy)
  print(io,")")
end

isempty(s::SmallSet) = s.vsize === 0
length(s::SmallSet{N, T}) where {N, T} = issmall(s) ? s.vsize : length(s.set::Set{T})

function in(x, s::SmallSet{N, T}) where {N, T}
  if issmall(s)
    for i = 1 : s.vsize
      if x == s.vector[i]
        return true
      end
    end
    return false
  else
    return x in s.set::Set{T}
  end
end

function push!(s::SmallSet{N, T}, x) where {N, T}
  if !(x in s)
    if s.vsize < N
      s.vsize += 1
      s.vector[s.vsize] = x
      return s
    end

    if issmall(s)
      s.set = Set{T}(s.vector)
    end

    push!(s.set::Set{T}, x)
  end
  return s
end

#TODO: Switch back to small? When?
function pop!(s::SmallSet{N, T}, x) where {N, T}
  if issmall(s)
    i = findfirst(s.vector, x)
    if i === 0
      throw(KeyError(x))
    else
      s.vector[i] = s.vector[s.vsize]
      s.vsize -= 1
    end
  else
    pop!(s.set::Set{T}, x)
  end
  return x
end
pop!(s::SmallSet, x, deflt) = x in s ? pop!(s, x) : deflt
pop!(s::SmallSet{N, T}) where {N, T} = issmall(s) ? pop!(s, s.vector[1]) : pop!(s, s.set::Set{T})
delete!(s::SmallSet, x) = (pop!(s, x, 0); s)

copy(s::SmallSet) = union!(similar(s), s)

sizehint!(s::SmallSet, newsz) = issmall(s) ? s : sizehint!(s.set) # FIXME?
empty!(s::SmallSet) = (s.set = nothing; s.vsize = 0; s)
rehash!(s::SmallSet) = issmall(s) ? s : rehash!(s.set)

start(s::SmallSet)       = issmall(s) ? start(s.vector) : start(s.set)
done(s::SmallSet, state) = issmall(s) ? done(s.vector, state) : done(s.set, state)
next(s::SmallSet, i)     = issmall(s) ? next(s.vector, i) : next(s.set, i)

union(s::SmallSet) = copy(s)
function union(s::SmallSet, sets::SmallSet...)
  u = SmallSet{join_sizes(s, sets...), join_eltype(s, sets...)}()
  union!(u, s)
  for t in sets
    union!(u, t)
  end
  return u
end
const ∪ = union
union!(s::SmallSet, xs) = (for x = xs; push!(s, x); end; s)
union!(s::SmallSet, xs::AbstractArray) = (sizehint!(s, length(xs)); for x = xs; push!(s, x); end; s) # FIXME

intersect(s::SmallSet) = copy(s)
#function intersect(s::Set, sets::Set...)
const ∩ = intersect
#function setdiff(a::Set, b::Set)
setdiff!(s::SmallSet, xs) = (for x=xs; delete!(s,x); end; s)

==(l::SmallSet, r::SmallSet) = (length(l) == length(r)) && (l <= r)
<( l::SmallSet, r::SmallSet) = (length(l) < length(r)) && (l <= r)
<=(l::SmallSet, r::SmallSet) = issubset(l, r)

const ⊆ = issubset
⊊(l::SmallSet, r::SmallSet) = <(l, r)
⊈(l::SmallSet, r::SmallSet) = !⊆(l, r)
⊇(l, r) = issubset(r, l)
⊉(l::SmallSet, r::SmallSet) = !⊇(l, r)
⊋(l::SmallSet, r::SmallSet) = <(r, l)

end

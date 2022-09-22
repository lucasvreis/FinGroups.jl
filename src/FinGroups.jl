module FinGroups

using Primes, Mods
import REPL.REPLCompletions: latex_symbols
import Base: (^)

# -----------------------------------

abstract type FinGroup end

abstract type SimpleGroup <: FinGroup end

export Cyclic, ℤ

struct Cyclic{p,k} <: SimpleGroup
    x::Mod
    Cyclic{p,k}(n) where {p,k} = new{p,k}(Mod{p^k}(n))
end

function ℤ(n)
    fac = factor(n)
    length(fac) ≠ 1 && return @error "Not Implemented"
    p, k  = first(fac)
    Cyclic{p,k}
end

ℤ(n,x) = ℤ(n)(x)

Base.show(io::IO, ::Type{Cyclic{p,k}}) where {p,k} = print(io, "ℤ($p^$k)")
Base.show(io::IO, g::Cyclic{p,k}) where {p,k} = print(io, "$(g.x.val) + $(p^k)ℤ")

# -----------------------------------

export DirPow, DirProd, ×

struct DirPow{A<:FinGroup,n} <: FinGroup
    val::NTuple{n,A}
    DirPow{A,n}(xs...) where {A,n} = new{A,n}(xs)
end

struct DirProd{T<:Tuple{Vararg{FinGroup}}} <: FinGroup
    t::T
end

Base.show(io::IO, ::Type{DirPow{A,n}}) where {A,n} = print(io, "$A^$n")
Base.show(io::IO, ::Type{DirProd{T}}) where {T} = join(io, (string(G) for G ∈ fieldtypes(T)), " × ")

A::Type{<:FinGroup} ^ n::Int = DirPow{A,n}
A::Type{<:FinGroup} × B::Type{<:FinGroup} = DirProd{Tuple{A,B}}


# -----------------------------------------------------

export classify_p_group, classify, divisors

function classify_p_group(p, k)
    k == 1 && return Cyclic{p,1}
    k == 2 && return (Cyclic{p,2}, Cyclic{p,1}^2)
    @error "Sorry, I don't know how to classify p-groups of exponent > 2"
end

function divisors(fac::Dict)
    (prod(fs) for fs in Iterators.product(((p^i for i in 0:k) for (p,k) in fac)...))
end

function classify(N::Int)
    fac = factor(Dict, N)
    if length(fac) == 1
        p, k = first(fac)

        @info "Order is power of prime, so G is a p-group"

        return classify_p_group(p, k)
    end

    n = Dict{Int,Vector{Int}}()
    for (p,k) in fac
        n[p] = Int[]
        delete!(fac, p)
        for d in divisors(fac)
            d % p == 1 && append!(n[p], d)
        end
        fac[p] = k
    end

    @info "Possibilities for p-Sylow multiplicities are:"
    println(join(("  $p" for p in n), "\n"))
end

end # module

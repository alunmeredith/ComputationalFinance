LatticeEurCall <- function(S0, K, r, T, sigma, N) {
    
    deltaT <- T/N
    
    # Evaluate expressions for u, d, p
    u = exp(sigma * sqrt(deltaT))
    d = 1/u
    p = (exp(r * deltaT) - d) / (u - d)
    
    
    # Generate option values at time T (last column of lattice)
    lattice = matrix(NA, N+1, N+1)
    for (i in 0:N){
        lattice[i+1, N+1] = max(0, S0 * (u^i) * (d^(N-i)) - K)
    }
    
    # Step backwards dt and generate option values of lattice at that point as weighted average of child nodes. 
    for (j in (N-1):0) {
        for (i in 0:j) {
            lattice[i + 1, j + 1] = exp(-r * deltaT) *
                (p * lattice[i+2, j+2] + (1-p) * lattice[i+1, j+2])
        }
    }
    
    return(list(price = lattice[1,1], lattice = lattice))
}
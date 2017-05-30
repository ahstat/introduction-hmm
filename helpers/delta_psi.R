delta_psi = function(Y, p = 0.4, q = 0.7, pi=c(0,0,1)) {
  # Transition matrices
  M = hidden_transition_matrix(p)
  N = hidden_to_observation_matrix(q)
  
  # delta and psi to fill
  m = length(Y)
  delta = matrix(ncol=3, nrow=m) # max
  psi = matrix(ncol=3, nrow=m) # argmax
  
  # delta_0 and psi_0
  k=1
  delta[k,]=pi * N[, Y[k]+1]
  psi[k,]=rep(0, ncol(psi))
  
  # delta_k and psi_k for k > 0
  for(k in 2:m) {
    delta[k,] = apply(delta[k-1,] * M, 2, max) * N[, Y[k]+1]
    psi[k,] = apply(delta[k-1,] * M, 2, which.max)
  }
  
  return(list(delta = delta, psi = psi))
}

# Compute the sequence with the highest likelihood using backtracking
best_sequence = function(delta, psi) {
  m = nrow(delta)
  
  xmax=rep(NA, m)
  xmax[m] = which.max(delta[m,])
  
  for(k in (m-1):1) {
    xmax[k]=psi[k+1,xmax[k+1]]
  }
  
  # Return the sequence with values as letters
  return(int_to_letter(xmax))
}
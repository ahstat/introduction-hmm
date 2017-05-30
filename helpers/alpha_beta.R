# Compute alpha, given p, q, and observations y_{1:k} where:
# alpha[k, i] := P(Y_{1:k} = y_{1:k}; X_k = i | p, q)
#
# pi is the initial distribution, here set as (0,0,1)
#
# This function is not suitable for long sequences
# (because alpha will converge to 0).
alpha_func = function(Y, p = 0.4, q = 0.7, pi = c(0,0,1)) {
  # Transition matrices
  M = hidden_transition_matrix(p)
  N = hidden_to_observation_matrix(q)
  
  # alpha to fill
  m = length(Y)
  alpha = matrix(ncol=3, nrow=m)
  
  # alpha_0
  k=1
  alpha[k,]=pi*N[, Y[k]+1]
  
  # alpha_k for k > 0
  for(k in 2:m) {
    alpha[k,]=(alpha[k-1,] %*% M) * N[, Y[k]+1]
  }
  
  return(alpha)
}

# Compute beta, given p, q, and observations y_{1:k} where:
# beta[k, i] := P(Y_{(k+1):m} = y_{(k+1):m} | X_k = i, p, q)
beta_func = function(Y, p = 0.4, q = 0.7) {
  # Transition matrices
  M = hidden_transition_matrix(p)
  N = hidden_to_observation_matrix(q)
  
  # beta to fill
  m = length(Y)
  beta = matrix(ncol=3, nrow=m)
  
  # beta_{m-1}
  k=m
  beta[m,]=rep(1,length(beta[1,]))
  
  # beta_k for k < m-1
  for(k in (m-1):1) {
    beta[k,]=  (M %*% N[,Y[k+1]+1]) * beta[k+1,]
  }
  
  return(beta)
}
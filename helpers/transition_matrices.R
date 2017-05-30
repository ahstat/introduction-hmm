# Define the matrix of transition M for hidden states
hidden_transition_matrix = function(p = 0.4) {
  return(matrix(c(1-p, p, 0, 0, 1-p, p, p, 0, 1-p), ncol=3, byrow=T))
}

# Define the matrix of transition N from hidden states to observations
hidden_to_observation_matrix = function(q = 0.7) {
  return(matrix(c(1, 0, 1-q, q, 0, 1), ncol=2, byrow=T))
}

# Define the likelihood of a sequence X and observations Y, given p and q
likelihood_of = function(X, Y, p, q) {
  # Transition matrices
  M = hidden_transition_matrix(p)
  N = hidden_to_observation_matrix(q)
  
  x = letter_to_int(X)
  
  logP_X = 0
  for(i in 1:(m-1)) {
    logP_X = logP_X + log(N[x[i],Y[i]+1])
    logP_X = logP_X + log(M[x[i],x[i+1]])
  }
  logP_X = logP_X + log(N[x[m],Y[m]+1])
  return(logP_X)
}
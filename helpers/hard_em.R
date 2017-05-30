# Given initial parameters (p_init, q_init), we update parameters by doing
# hard EM (see beamer page 33).
# After iterating, we obtain estimation of p and q (p_out and q_out).
obtain_p_q = function(Y, p_init, q_init, k_max = 100,
                      p_seq = seq(from=0.01, to=0.99, by=0.01),
                      q_seq = seq(from=0.01, to=0.99, by=0.01)) {
  p_vect = p_init
  q_vect = q_init
  
  for(k in 1:k_max) {
    # Break the loop when convergence happened
    if(k > 1) {
      if(p_vect[k] == p_vect[k-1] & q_vect[k] == q_vect[k-1]) {
        break()
      }
    }
    
    # Get the best sequence knowing (p_k, q_k)
    viterbi_output = delta_psi(Y, p_vect[k], q_vect[k])
    delta = viterbi_output$delta
    psi = viterbi_output$psi
    Xmax = best_sequence(delta, psi)
    
    # Get the log-likelihood of the resulting sequence for all (p, q)
    logP = matrix(NA, ncol=length(q_seq), nrow=length(p_seq))
    for(i in 1:length(p_seq)) {
      for(j in 1:length(q_seq)) {
        p = p_seq[i]
        q = q_seq[j]
        logP[i,j] = likelihood_of(Xmax, Y, p, q)
      }
    }
    
    # Select new (p_{k+1}, q_{k+1}) with the highest log-likelihood
    idx_max = which(logP == max(logP), arr.ind = TRUE)
    p_vect[k+1] = p_seq[idx_max[1]]
    q_vect[k+1] = q_seq[idx_max[2]]
  }
  
  p_out = tail(p_vect, 1)
  q_out = tail(q_vect, 1)
  
  return(c(p = p_out, q = q_out))
}
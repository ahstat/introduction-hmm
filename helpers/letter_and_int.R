letter_to_int = function(X) {
  m = length(X)
  x = rep(NA, m)
  x[which(X == "A")] = 1
  x[which(X == "B")] = 2
  x[which(X == "C")] = 3
  return(x)
}

int_to_letter = function(x) {
  m = length(x)
  X = rep(NA, m)
  X[which(x == 1)] = "A"
  X[which(x == 2)] = "B"
  X[which(x == 3)] = "C"
  return(X)
}
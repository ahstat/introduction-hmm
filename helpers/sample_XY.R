# Compute a sample of (X, Y) with rate p (for X), rate q (for Y) and a size
# of m. The seed can be selected with the 'seed' argument.
# When seed = "beamer", the sample used for the presentation is retrieved.
sampleXY = function(p = 0.4, q = 0.7, m = 100, seed = 1234) {
  if(seed == "beamer") {
    if(p == 0.4 & q == 0.7 & m == 100) {
      return(sample_beamer())
    } else {
      stop(paste('seed == "beamer" is only here to retrieve the sample shown',
                 'in the beamer presentation.'))
    }
  }
  
  set.seed(seed)
  X = sample_X(p, m)
  Y = sample_Y(q, X)
  return(list(X=X, Y=Y))
}

# Get the next letter of the alphabet
next_letter = function(letter) {
  if(letter == "A") {
    return("B")
  } else if(letter == "B") {
    return("C")
  } else if(letter == "C") {
    return("A")
  }
}

# Sample X with rate p and size m
sample_X = function(p, m = 100) {
  X = rep(NA, m)
  X[1] = "C" # because pi is selected as (0, 0, 1)
  aleaX = rbinom(m, 1, p)
  for(i in 1:(m-1)) {
    X[i+1] = ifelse(aleaX[i] == 1, next_letter(X[i]), X[i])
  }
  return(X)
}

# Sample Y with rate q, knowing X
sample_Y = function(q, X) {
  Y = rep(NA, length(X))
  aleaY = rbinom(length(X), 1, q)
  Y[which(X == "A")] = 0
  Y[which(X == "C")] = 1
  
  idxB = which(X == "B")
  Y[idxB] = ifelse(aleaY[idxB] == 1, 1, 0)
  return(Y)
}

# Sample (X, Y) used in the presentation
sample_beamer = function() {
  X=c("C", "C", "C", "A", "B", "B", "B", "C", "A", "B",
      "B", "B", "B", "B", "C", "C", "A", "A", "A", "B",
      "C", "C", "C", "A", "A", "B", "B", "C", "C", "C",
      "C", "C", "A", "A", "A", "A", "B", "B", "B", "B",
      "B", "C", "C", "C", "C", "A", "A", "A", "A", "A",
      "A", "A", "B", "C", "A", "B", "B", "B", "B", "B",
      "B", "B", "B", "B", "B", "C", "C", "A", "A", "B",
      "C", "A", "A", "B", "B", "B", "C", "A", "A", "B",
      "B", "C", "C", "A", "B", "B", "B", "B", "B", "B",
      "C", "C", "C", "A", "A", "A", "A", "A", "A", "B")
  
  Y=c(1, 1, 1, 0, 1, 0, 0, 1, 0, 0,
      1, 1, 0, 0, 1, 1, 0, 0, 0, 1,
      1, 1, 1, 0, 0, 1, 1, 1, 1, 1,
      1, 1, 0, 0, 0, 0, 0, 0, 0, 1,
      1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
      0, 0, 1, 1, 0, 0, 0, 0, 1, 0,
      1, 0, 1, 1, 0, 1, 1, 0, 0, 1,
      1, 0, 0, 1, 0, 0, 1, 0, 0, 0,
      1, 1, 1, 0, 0, 1, 0, 1, 0, 0,
      1, 1, 1, 0, 0, 0, 0, 0, 0, 1)
  
  return(list(X=X, Y=Y))
}
rm(list = ls())
setwd("E:/gitperso/introduction-hmm/")
#setwd("E:/to/your/directory/")
source("helpers/sample_XY.R")
source("helpers/transition_matrices.R")
source("helpers/alpha_beta.R") # forward and backward decomposition
source("helpers/letter_and_int.R") # conversion between A, B, C and 1, 2, 3
source("helpers/delta_psi.R") # Viterbi algorithm (max and argmax)
source("helpers/hard_em.R")

width = 600
height = 600
dir.create("outputs", showWarnings = FALSE)

# The following libraries and source are only for the graphical representation
# of the problem shown in the presentation.
# install.packages("igraph")
# install.packages("plotrix")
library(igraph) #for arrows
library(plotrix) #for circles
source("helpers/plot_buildings.R")

##################
# Sample X and Y #
##################
# Initial distribution is assumed to be pi = c(0,0,1) for all this code.
XY = sampleXY(p = 0.4, q = 0.7, m = 100, seed = "beamer")
X = XY$X
Y = XY$Y

#########################
# Forward decomposition #
#########################
# alpha[k, i] := P(Y_{1:k} = y_{1:k}; X_k = i | p, q)

##
# Computation for the true parameters
##
p = 0.4
q = 0.7
m = length(Y)
alpha = alpha_func(Y, p, q)

# Result for beamer presentation page 23:
alpha[1,] # alpha_0
alpha[2,] # alpha_1
alpha[3,] # alpha_2
alpha[4,] # alpha_3
alpha[5,] # alpha_4
alpha[6,] # alpha_5
alpha[7,] # alpha_6
alpha[8,] # alpha_7
alpha[9,] # alpha_8
alpha[nrow(alpha),] # alpha_{m-1}
sum(alpha[m,]) # p(y_{0:(m-1)}) knowing p and q

##
# Computations for a fixed q, or for a fixed p
##
# The line in red is position of the true parameter
main = paste("Probability of the sequence of observations knowing q=",
             q, " as a function of p", sep = "")
p_seq = seq(from=0, to=1, by=0.01)
proba_obs = sapply(p_seq, function(p) {sum(alpha_func(Y, p, q)[m,])})
plot(p_seq, proba_obs, type = "l",
     xlab = "p", ylab = "Probability",
     main = main)
abline(v = p, col = "red")

main = paste("Probability of the sequence of observations knowing p=",
             p, " as a function of q", sep = "")
q_seq = seq(from=0, to=1, by=0.01)
proba_obs = sapply(q_seq, function(q) {sum(alpha_func(Y, p, q)[m,])})
plot(q_seq, proba_obs, type = "l",
     xlab = "q", ylab = "Probability",
     main = main)
abline(v = q, col = "red")

#####################
# Viterbi algorithm #
#####################
# Algorithm to find the best sequence knowing p and q,
# where "best" means with the highest likelihood.
p=0.4
q=0.7
m = length(Y)
viterbi_output = delta_psi(Y, p, q)
delta = viterbi_output$delta
psi = viterbi_output$psi

##
# Log-likelihood for the best sequence
##
logP_star = log(max(delta[m,]))

##
# Best sequence (using backtracking)
##
Xmax = best_sequence(delta, psi)

# Result for beamer presentation page 28:
print(X)
print(Xmax)

##
# Log-likelihoods
##
# Log likelihood for the true sequence
logP_Xinit = likelihood_of(X, Y, p, q)
print(logP_Xinit)

# Log likelihood for the sequence with the highest likelihood
# (recomputation using M and N)
logP_Xbest = likelihood_of(Xmax, Y, p, q)
print(logP_Xbest)

# Log-likelihood for the best sequence (ok, we have same as logP_Xbest)
print(logP_star)

########################
# Estimation of (p, q) # (Method 1: Using forward decomposition)
########################
# Using the forward decomposition, compute P(y_{0:(m-1)} | p, q)
# for a grid of parameters (p, q)
# P is the likelihood of observations given p, q
p_seq = seq(from=0.01, to=0.99, by=0.01)
q_seq = p_seq
P = matrix(NA, ncol=length(q_seq), nrow=length(p_seq))

for(i in 1:length(p_seq)) {
  p = p_seq[i]
  P[i,] = sapply(q_seq, function(q) {sum(alpha_func(Y, p, q)[m,])})
}
logP = log(P)

idx_max = which(logP == max(logP), arr.ind = TRUE)
p_max = p_seq[idx_max[1]]
q_max = q_seq[idx_max[2]]

##
# Output of the likelihood
##
png("outputs/01-Likelihood.png", width, height)

graph=persp(P, theta=50, phi=25,
            xlab="p - param. Markov", 
            ylab="q - param. Markov to obs.", 
            zlab="likelihood")

# Position for the best likelihood
mypoints = trans3d(x=p_max,y=q_max,z=P[idx_max], pmat = graph)
points(mypoints,col="blue", pch=16)

# Position for the likelihood corresponding to the true sequence
mypoints = trans3d(x=0.4,y=0.7,z=P[40,70], pmat = graph)
points(mypoints,col="green", pch=16)

dev.off()

##
# Output of the contour of the log-likelihood
##
png("outputs/02-log-likelihood_contour.png", width, height)

contour(logP, nlevels=100, xlab="p", ylab="q")

# Position for the best likelihood
points(x=p_max, y=q_max, col="blue", pch=16)

# Position for the likelihood corresponding to the true sequence
points(x=0.4, y=0.7, col="green", pch=16)

dev.off()

########################
# Estimation of (p, q) # (Method 2: hard EM)
########################
# Given initial parameters theta_0 = (p_0, q_0),
# we can update parameters by doing hard EM (see beamer page 33).
# After iterating, we obtain an estimation of p and q.
#
# We can obtain p and q given all initial values p_0 and q_0 on a grid.

##
# Code to compute estimated (p, q) given different initialization (p_0, q_0)
##
p_seq = seq(from=0.01, to=0.99, by=0.2)
q_seq = p_seq

p_given_pq_init = matrix(NA, nrow = length(p_seq), ncol = length(q_seq))
q_given_pq_init = matrix(NA, nrow = length(p_seq), ncol = length(q_seq))

for(i in 1:length(p_seq)) {
  p_init = p_seq[i]
  for(j in 1:length(q_seq)) {
    q_init = q_seq[j]
    print(paste("(", p_init, ", ", q_init, ")", sep = ""))
    pq_current = obtain_p_q(Y, p_init, q_init)
    p_given_pq_init[i,j] = pq_current[1]
    q_given_pq_init[i,j] = pq_current[2]
  }
}

plot(p_given_pq_init, q_given_pq_init)

##
# Plotting
##
# We have obtain the following values (for some initial conditions)
pA=c(0.06,0.31,0.46,0.51,0.51,0.51)
qA=c(0.46,0.36,0.71,0.21,0.56,0.71)
weight=c(392,13,4,4642,95,4655)/9801

png("outputs/03-log-likelihood_contour_and_EM_hard.png", width, height)

contour(logP, nlevels=100, xlab="p", ylab="q")
points(x=0.4, y=0.7, col="green", pch=16)
points(x=p_max, y=q_max, col="blue", pch=16)

points(x=pA[1], y=qA[1], col="red", pch=".")
points(x=pA[2], y=qA[2], col="red", pch=".")
points(x=pA[3], y=qA[3], col="red", pch=".")
points(x=pA[4], y=qA[4], col="red", pch=16)
points(x=pA[5], y=qA[5], col="red", pch=".")
points(x=pA[6], y=qA[6], col="red", pch=16)

dev.off()

#####################
# Plot of buildings #
#####################
# Plot for pages 6, 8, 11 and 12 of the beamer presentation

png("outputs/04-buildings.png", width, height) 
plot(c(2),xlim=c(-0.8,0.7), ylim=c(-0.9,0.6), axes=FALSE, xlab="", ylab="")
plot_rectangles()
dev.off()

png("outputs/05-buildings_and_arrows.png", width, height) 
plot(c(2),xlim=c(-0.8,0.7), ylim=c(-0.9,0.6), axes=FALSE, xlab="", ylab="")
iArrows=igraph:::igraph.Arrows
plot_rectangles()
plot_arrows()
dev.off()

png("outputs/06-buildings_and_circle.png", width, height) 
plot(c(2),xlim=c(-0.8,0.7), ylim=c(-0.9,0.6), axes=FALSE, xlab="", ylab="")
draw.circle(mean(xC),mean(yC), 0.59, col="lightgrey", border="lightgrey")
plot_rectangles()
text(-0.7, -0.7, "0", cex = 2)
text(-0.25, 0.15, "1", cex = 2)
dev.off()
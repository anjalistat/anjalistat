# simulate discrete Markov chains
# setup transition matrix 
P <- TPM@transitionMatrix

run.mc.sim <- function( P,   # probability transition matrix
                        num.iters=20, 
                        num.chains=500 )
{
  
  # number of possible states
  num.states <- nrow(P)
  
  # states X_t for all chains
  states     <- matrix(NA, ncol=num.chains, nrow=num.iters)
  
  # probability vectors pi^n through time
  all_probs  <- matrix(NA, nrow=num.iters, ncol=num.states)
  
  # forces chains to start in state 1
  pi_0      <- c(1, rep(0, num.states-1))
  
  # initialize variables for first state 
  P_n           <- P
  all_probs[1,] <- pi_0
  states[1,]    <- 1
  
  for(t in 2:num.iters) {
    
    # pi^n for this iteration
    pi_n           <- pi_0 %*% P_n
    all_probs[t,]  <- pi_n
    
    for(chain_num in seq_len(num.chains)) {
      # probability vector to simulating next state 
      p                     <- P[ states[t-1,chain_num], ]
      states[t,chain_num]   <- which(rmultinom(1, 1, p) == 1)
    }
    
    # update probability transition matrix
    P_n           <- P_n %*% P
  }
  
  return(all_probs)
}

################################################################

sim1 <- run.mc.sim(P)
sim1
all.probs <- sim1
#matplot(all.probs, type='l', col=1:7, lty=1, ylab='probability', xlab='n')
#legend(locator(1), c("-1 to -0.5", "-0.5  to 0.0",
#               "0.0 to 0.5", "0.5 to 1","1 to 1.5",
#               "1.5 to 2","more than 2"), lty=1:2,xpd=TRUE, col=1:7,pch=20,cex=0.69)


matplot(all.probs, type='l', col=1:11, lty=1,main='Limiting probabilities', ylab='probability', xlab='n-steps')
legend("topright", c("-1 to -0.2","-0.2 to -0.1","-0.1 to 0", "0  to 0.25",
                     "0.25 to 0.4", "0.4 to 0.55","0.55 to 0.7","0.7 to 0.85","0.85 to 1",
                     "1 to 1.15","more than 1"), lty=1:11,xpd=TRUE,col=1:7,pch=20,cex=0.6)

write.csv(sim1$all.probs,"limitingprob.csv")


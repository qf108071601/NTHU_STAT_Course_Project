accept.reject = function(n) { 
  n.accepts = 0
  result.sample = rep(NA, n)
  while (n.accepts < n) {
    y = rexp(n = 1,rate = 1)
    u1 = sample(Uni$U,1)       # step 2
    u2 = sample(Uni$U,1)
    if (u1 <= exp((-(y-1)/2))) { # step 3 (accept)
      if (u2<=0.5) {
        y = -y
      }
      n.accepts = n.accepts+1
      result.sample[n.accepts] = y
    }
  }
  result.sample
}


vals <- accept.reject(n = 10000) 

# Checking if it went well
hist(vals, breaks=30, freq=FALSE, main="Sample vs true Density")
mean(vals);var(vals)

#' Expectation Step of the EM Algorithm
#'
#' Calculate the posterior probabilities (soft labels) that each component
#' has to each data point.
#'
#' @param sd.vector Vector containing the standard deviations of each component
#' @param sd.vector Vector containing the mean of each component
#' @param alpha.vector Vector containing the mixing weights  of each component
#' @return Named list containing the loglik and posterior.df
#' 
e_step <- function(x, mu.vector, sd.vector, alpha.vector) {
  comp1.prod <- dnorm(x, mu.vector[1], sd.vector[1]) * alpha.vector[1]
  comp2.prod <- dnorm(x, mu.vector[2], sd.vector[2]) * alpha.vector[2]
  
  sum.of.comps <- comp1.prod + comp2.prod
  comp1.post <- comp1.prod / sum.of.comps
  comp2.post <- comp2.prod / sum.of.comps
  
  comp1.post.lik = alpha.vector[1]*comp1.post
  comp2.post.lik = alpha.vector[2]*comp2.post
  sum.of.post.lik <- comp1.post.lik + comp2.post.lik
  
  
  sum.of.comps.ln <- log(sum.of.comps, base = exp(1))
  sum.of.post.lik.ln <- log(sum.of.post.lik, base = exp(1))
  
  sum.of.comps.ln.sum <- sum(sum.of.comps.ln)+sum(sum.of.post.lik.ln)
  
  list("loglik" = sum.of.comps.ln.sum,
       "prob" = comp1.post,
       "posterior.df" = cbind(comp1.post, comp2.post))
}

#' Maximization Step of the EM Algorithm
#'
#' Update the Component Parameters
#'
#' @param x Input data.
#' @param posterior.df Posterior probability data.frame.
#' @return Named list containing the mean (mu), variance (var), and mixing
#'   weights (alpha) for each component.
m_step <- function(x, posterior.df) {
  comp1.n <- sum(posterior.df[, 1])
  comp2.n <- sum(posterior.df[, 2])
  
  comp1.mu <- 1/comp1.n * sum(posterior.df[, 1] * x)
  
  comp2.mu <- 1/comp2.n * sum(posterior.df[, 2] * x)
  
  comp1.var <- sum(posterior.df[, 1] * (x - comp1.mu)^2) * 1/comp1.n
  comp2.var <- sum(posterior.df[, 2] * (x - comp2.mu)^2) * 1/comp2.n
  
  comp1.alpha <- comp1.n / length(x)
  comp2.alpha <- comp2.n / length(x)
  
  list("mu" = c(comp1.mu, comp2.mu),
       "var" = c(comp1.var, comp2.var),
       "alpha" = c(comp1.alpha, comp2.alpha))
}
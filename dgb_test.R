library(kernlab)

source("lambda_ocsvm.R")

# Safe Screening of OCSVM
# INPUT: D = {x_i}(サンプル数は"m") lambda_ref, alpha_ref(参照解), cond1, cond2
# OUTPUT: L, R, Z(正常，異常，データセットの添字集合)

screen_notSV <- function(x, m, l_l, u_l) {
  # Initialize
  L = c()
  R = c()
  Z = 1:m
  
  # Process
  for (i in 1:m) {
    if (u_l[i] < 1) {
      L <- append(L, i)
      Z <- Z[Z != i]
    } else if (l_l[i] > 1) {
      R <- append(R, i)
      Z <- Z[Z != i]
    }
  }
  
  # OUTPUT
  return(list(L, R, Z))
}

calc_wphi <- function(lambda_ref, alpha_ref, K) {
  wphi <- (t(alpha_ref) %*% K)/lambda_ref
  return(wphi)
}

max_0 <- function(x) {
  return(max(0, 1 - x))
}

calc_primal <- function(lambda_ref, alpha_ref, K) {
  primal <- sum(unlist(lapply(1/lambda_ref * t(alpha_ref) %*% K, max_0)))
            + 1/(2*lambda_ref) * t(alpha_ref) %*% K %*% alpha_ref
  
  return(primal)
}

calc_r <- function(primal, dual, lambda_ref) {
  radius <- sqrt(2*(primal - dual)/lambda_ref)
  
  return(radius)
}

calc_l_l <- function(wphi, radius) {
  return(wphi - radius)
}

calc_u_l <- function(wphi, radius) {
  return(wphi + radius)
}

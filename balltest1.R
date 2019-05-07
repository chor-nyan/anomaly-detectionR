library(kernlab)

source("svm_cvxr.R")

# Safe Screening of OCSVM
# INPUT: D = {x_i}(サンプル数は"m") C_ref, xi_ref(参照解), cond1, cond2
# OUTPUT: L, R, Z(正常，異常，データセットの添字集合)

screen_notSV <- function(x, m, l_C, u_C) {
  # Initialize
  L = c()
  R = c()
  Z = 1:m
  
  # Process
  for (i in 1:m) {
    if (u_C[i] < 1) {
      L <- append(L, i)
      Z <- Z[Z != i]
    } else if (l_C[i] > 1) {
      R <- append(R, i)
      Z <- Z[Z != i]
    }
  }
  
  # OUTPUT
  return(list(L, R, Z))
}

calc_phim <- function(C, C_ref, alpha_C_ref, K) {
  return((C + C_ref)/(2*C_ref) * (t(alpha_C_ref) %*% K))
}
calc_r <- function(C, C_ref, alpha_C_ref, K) {
  return((C - C_ref)/(2*C_ref) * sqrt(t(alpha_C_ref) %*% K %*% alpha_C_ref))
}

calc_l_C <- function(C, C_ref, alpha_C_ref, K) {
  l_C <- calc_phim(C, C_ref, alpha_C_ref, K) 
         - calc_r(C, C_ref, alpha_C_ref, K)
  
  return(l_C)
}
calc_u_C <- function(C, C_ref, alpha_C_ref, K) {
  u_C <- calc_phim(C, C_ref, alpha_C_ref, K) 
  + calc_r(C, C_ref, alpha_C_ref, K)
  
  return(u_C)
}

# calc_l_C <- function(C, C_ref, alpha_C_ref, K) {
#   l_C <- (C + C_ref)/(2*C_ref) * (t(alpha_C_ref) %*% K)
#   - (C - C_ref)/(2*C_ref) * sqrt(t(alpha_C_ref) %*% K %*% alpha_C_ref)
#   
#   return(l_C)
# }
# 
# calc_u_C <- function(C, C_ref, alpha_C_ref, K) {
#   u_C <- (C + C_ref)/(2*C_ref) * (t(alpha_C_ref) %*% K)
#   + (C - C_ref)/(2*C_ref) * sqrt(t(alpha_C_ref) %*% K %*% alpha_C_ref)
#   
#   return(u_C)
# }


library(CVXR)

l_ocsvm <- function(lambda, K) {
  # 変数定義
  alpha <- Variable(m)
  
  # 目的関数
  objective <- 1/(2*lambda) * (quad_form(alpha, K)) - sum(alpha)
  constraints <- list(0 <= alpha, alpha <= 1)
  prob <- Problem(Minimize(objective), constraints)
  solution <- solve(prob)
  
  dual <- solution$value
  alpha <- solution$getValue(alpha)
  sv_flag <- alpha > 0.00001
  alpha[!sv_flag] <-  0
  dis_hyperplane <- t(alpha) %*% K
  
  anomaly_flag <- dis_hyperplane < 1
  anomal <- subset(x, anomaly_flag)
  normal <- subset(x, !anomaly_flag)
  
  # 可視化
  plot(0, 0, type = "n", xlim = c(-3, 5), ylim = c(-3, 3), xlab = "x", ylab = "y")
  points(normal, col = "darkgrey", pch = 1)
  points(anomal, col = "orange", pch = 1)
  
  max_0 <- function(x) {
    return(max(0, 1 - x))
  }
  
  # loss <- sum(max_0(as.vector(dis_hyperplane)))
  
  return(list(alpha, dual))
}

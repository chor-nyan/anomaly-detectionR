library(CVXR)

C_ocsvm <- function(C, K) {
  # 変数定義
  alpha <- Variable(m)
  
  # 目的関数
  objective <- (1/2) * (quad_form(alpha, K)) - sum(alpha)
  constraints <- list(0 <= alpha, alpha <= C)
  prob <- Problem(Minimize(objective), constraints)
  solution <- solve(prob)
  
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
  
  loss <- sum(max_0(as.vector(dis_hyperplane)))
  
  return(list(alpha, loss))
}

# shrink <- subset(x, sv_flag)
# m = dim(shrink)[1]
# # 変数定義
# C <- 0.05
# alpha <- Variable(m)
# K <- kernelMatrix(rbf, shrink)
# # 目的関数
# objective <- (1/2) * (quad_form(alpha, K)) - sum(alpha)
# constraints <- list(0 <= alpha, alpha <= C)
# prob <- Problem(Minimize(objective), constraints)
# solution <- solve(prob)
# 
# alpha <- solution$getValue(alpha)
# sv_flag <- alpha > 0.00001
# alpha[!sv_flag] <-  0
# dis_hyperplane <- t(alpha) %*% K
# # hyperplane <- function(index, alpha, K) {
# #   return(t(alpha) %*% K)
# # }
# anomaly_flag <- dis_hyperplane < 1
# anomal <- subset(shrink, anomaly_flag)
# normal <- subset(shrink, !anomaly_flag)
# plot(0, 0, type = "n", xlim = c(-3, 5), ylim = c(-3, 3), xlab = "x", ylab = "y")
# points(normal, col = "darkgrey", pch = 1)
# points(anomal, col = "orange", pch = 1)

# f(1, solution$getValue(alpha), K)

# # データ生成
# set.seed(10)
# n <- 2
# m <- 50
# 
# X <- matrix(rnorm(m*n), nrow = m, ncol = n)
# y <- c(rep(-1, m/2), rep(1, m/2))
# X[y == 1,] = X[y == 1,] + 1

# # 変数定義
# cost <- 10
# beta0 <- Variable()
# beta <- Variable(n)
# slack <- Variable(m)
# 
# # 目的関数の定義と最適化
# objective <- (1/2) * sum_squares(vstack(beta, beta0)) + cost * sum(slack)
# constraints <- list(y * (X %*% beta + beta0) >= 1 - slack, slack >= 0)
# prob3.3 <- Problem(Minimize(objective), constraints)
# solution3.3 <- solve(prob3.3)
# solution3.3$status
# solution3.3$getValue(beta)

# # Variables minimized over
# x <- Variable(1)
# y <- Variable(1)
# 
# # Problem definition
# objective <- Minimize(x^2 + y^2)
# constraints <- list(x >= 0, 2*x + y == 1)
# prob2.1 <- Problem(objective, constraints)
# 
# # Problem solution
# solution2.1 <- solve(prob2.1)
# solution2.1$status
# solution2.1$value

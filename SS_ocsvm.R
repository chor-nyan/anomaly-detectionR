library(kernlab)
library(CVXR)

source("svm_cvxr.R")
source("balltest1.R")

# データおよびカーネル定義
set.seed(0)
n = 2  # 2次元
m = 1000
x1 <- rbind(matrix(rnorm(950*2), 950, 2))
x2 <- rbind(matrix(rnorm(50*2), 50, 2)) + 3 
x <- rbind(x1, x2)
# rbf <- rbfdot(sigma = 0.1)
sigma <- sigest(x, scaled = F)[2]
rbf <- rbfdot(sigma = sigma)
K <- kernelMatrix(rbf, x)
C <- 0.06
C_ref <- 0.05

temp <- C_ocsvm(C_ref, K)
alpha_C_ref <- temp[[1]]
xi <- temp[[2]]

l_C <- calc_l_C(C, C_ref, alpha_C_ref, K)
u_C <- calc_u_C(C, C_ref, alpha_C_ref, K)

L_R_Z <- screen_notSV(x, m, l_C, u_C)
L <- L_R_Z[[1]]
R <- L_R_Z[[2]]
Z <- L_R_Z[[3]]

plot(0, 0, type = "n", xlim = c(-3, 5), ylim = c(-3, 3), xlab = "x", ylab = "y")
points(x[R,], col = "darkgrey", pch = 1)
points(x[L,], col = "orange", pch = 1)

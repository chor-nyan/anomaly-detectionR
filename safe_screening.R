library(tidyverse)
library(kernlab)
library(MASS)

# 学習データとパラメータ
set.seed(101)
x <- rbind(matrix(rnorm(1000), ncol = 2), matrix(rnorm(1000, mean = 4), ncol = 2))
x <- scale(x)
sigma <- sigest(x, scaled = F)[2]
rbf <- rbfdot(sigma = sigma)

    
# 学習
ocsvm <- ksvm(x, type = "one-svc", kernel = rbf, nu = 0.1)

# 参照する双対変数αの格納
alpha_ref <- rep(0, 1000)
alpha_ref[ocsvm@alphaindex] <- ocsvm@alpha

# 図示
colorcode <- rep(0, nrow(x))
colorcode[ocsvm@alphaindex] <- 1
plot(x, pch = 21, bg = colorcode)  # bgはマークを塗りつぶす色の指定 
px <- seq(-3, 3, 0.03)
py <- seq(-3, 3, 0.03)
pgrid <- expand.grid(px, py)
pred <- predict(ocsvm, pgrid)
contour(px, py, array(as.numeric(pred) - 1, dim = c(length(px), length(py))),
        xlim = c(-3, 3), ylim = c(-3, 3),
        col = "red", lwd = 2, add = T)

# mu1 <- c(-0.5, -0.5)
# mu2 <- c(0.5, 0.5)
# si <- diag(1.5 ^ 2, 1.5 ^ 2)
#   
# x1 <- mvrnorm(500, mu1, si)
# y1 <- rep(1, 500)
# x2 <- mvrnorm(500, mu2, si)
# y2 <- rep(-1, 500)
# install.packages(c("shinyjs", "mime"))
# df <- tibble(
#   x = c(x1, x2),
#   y = c(y1, y2)
# )
# 
# x <- c(x1, x2)
# plot(x1, y1)

# df <- tribble(
#   ~x, ~y,
#   3, 1,
#   4, 0
# )

x <- c(1, 2, 3, 4)
y <- 1

c = 2
c_ref = 1
w_ref = c(1:10)
xi_ref = 1

# not-kernelization

# Ball Test1
safe_screening <- function(x, y, c, c_ref, w_ref, xi_ref) {
  m1 <- ((c + c_ref)/2*c_ref) * w_ref
  r1 <-  ((c - c_ref)/2*c_ref) * norm(w_ref, type = "2")
  l_c <- (y*x) %*% m1 - r1*norm(y*x, type = "2")  
  u_c <- (y*x) %*% m1 + r1*norm(y*x, type = "2")
  
  if(l_c > 1) {return(0)
    } else return(c)
}



library(kernlab)
library(mlbench)
library(e1071)
# dat <- mlbench.spirals(400, cycles = 1.2, sd = 0.07)
# x <- dat$x
# y <- dat$classes
# rbfsvm <- ksvm(x, y, type = "C-svc", kernel = "rbfdot")
# par(ps = 16, lwd = 2)
# plot(x, col = predict(rbfsvm, x))
# 
# testx <- matrix(rnorm(100),,2)
# predict(rbfsvm, testx)

dat <- mlbench.2dnormals(200, cl = 2, sd = 1)
plot(dat)
x <- dat$x
y <- dat$classes
# y <- rep(1, 200)

d.svm <- svm(x, y)

nusvm <- ksvm(x, y, type = "nu-svc", kernel = "rbfdot")
c <- 1/(nusvm@b * 200)
Csvm <- ksvm(x, y, type = "C-svc", kernel = "rbfdot", C = c)

C <- 10
Csvm <- ksvm(x, y, type = "C-svc", kernel = "rbfdot", C = C)
alpha <- unlist(Csvm@alpha)
nu <- sum(alpha)/(C*200)
nusvm <- ksvm(x, y, type = "nu-svc", kernel = "rbfdot", nu = nu)
Csvm
nusvm

library(svmpath)
library(kernlab)
library(mlbench)
dat <- mlbench.2dnormals(200, 2, sd = 1)
x <- dat$x
y <- sign(as.double(dat$classes) - 1.5)
sigma <- sigest(x)[2]
fit <- svmpath(x, y, kernel.function = radial.kernel, param.kernel = sigma)

par(ps = 20, mai = c(1., 1., 0.7, 0.7))
idx <- rev(order(colSums(abs((apply(fit$alpha, 1, diff))))))[1:5]

diff(1:10, 2)
plot(1/fit$lambda, fit$alpha[idx[1],],
     log = 'x', type = 'l', lwd = 2, xlab = "C", ylab = "alpha")
for (i in 2:length(idx)) {
  lines(1/fit$lambda, fit$alpha[idx[i],], col = i, lwd = 2)
  
}
# data(svmpath)
# attach(balanced.overlap)
# 
# fit <- svmpath(x,y)
# coef(fit)
# plot(fit$xvar="lambda")
# # fit <- svmpath(x,y,trace=TRUE,plot=FALSE)
# # plot(fit,step=2)
# 
# detach(balanced.overlap)

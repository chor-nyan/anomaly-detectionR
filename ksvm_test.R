library(kernlab)

set.seed(0)
x1 <- c(rnorm(300, mean=0, sd=1), runif(10, min=-5, max=5)) 
x2 <- c(rnorm(300, mean=0, sd=1), runif(10, min=-5, max=5))

plot(x1, x2)

df <- data.frame(type=1, x1, x2) # type:分類クラス（全て1を代入）

x <- rbind(matrix(rnorm(1000), ncol = 2), matrix(rnorm(1000, mean = 4), ncol = 2))
y <- rep(1, 1000)
model <- ksvm(x, y, type = "C-svc", kernel = "rbfdot",
              C = 5, kpar = list(sigma = 0.5))

library(mlbench)
data <- mlbench.circle(n = 100)
x <- data$x
y <- rep(1, 100)
y <- (as.numeric(data$classes) - 1) * 2 - 1

f2 <- ksvm(x, y, type = "C-svc", kernel = "rbfdot", C = 0.5)

predict(f2, x)
plot(f2)

# data(iris)
# rbf <- rbfdot(sigma = 0.1)
# 
# irismodel <- ksvm(Species ~ ., data = iris, type = "C-bsvc", 
#                   kernel = rbf, C = 10, prob.model = TRUE)
# 
# ft <- fitted(irismodel)
# 
# predict(irismodel, iris[,-5], type="probabilities")

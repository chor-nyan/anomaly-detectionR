set.seed(101)
tmp1 <- rnorm(1000)
set.seed(102)
tmp2 <- rnorm(1000, 0, 1)
d <- data.frame(x = tmp1, y = tmp2)
plot(d, pch = 19, cex = 0.5, xlim = c(-4, 4), ylim = c(-4, 4))

install.packages("e1071")
library(e1071)
tuned <- tune.svm(x = d, y = rep(TRUE, nrow(d)), type = 'one-classification', 
                  kernel = 'radial', gamma = 1e-3:1, nu = 1e-2:1)
tuned$best.parameters

d.svm <- svm(x = d, y = NULL, type = "one-classification", kernel = "radial",
             gamma = tuned$best.parameters$gamma * 100, nu = tuned$best.parameters$nu)

px <- seq(-4, 4, 0.05)
py <- seq(-4, 4, 0.05)
pgrid <- expand.grid(px, py)
names(pgrid) <- names(d)
pred <- predict(d.svm, newdata = pgrid)
pred.num <- as.integer(pred)
plot(d, pch = 19, cex = 0.5, xlim = c(-4, 4), ylim = c(-4, 4), xlab = "", ylab = "")
par(new = T)
contour(px, py, array(pred.num, dim = c(length(px), length(py))), 
        levels = 0.5, labels = "",  xlim = c(-4, 4), ylim = c(-4, 4), lwd = 5, col = "red")
  
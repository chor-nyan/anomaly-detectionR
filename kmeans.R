library(cluster)
set.seed(101)
x <- rbind(matrix(rnorm(1000), ncol = 2), matrix(rnorm(1000, mean = 4), ncol = 2))
x <- scale(x)
plot(x)
dat.km <- kmeans(x, 2)
clusplot(x, dat.km$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
    
# 学習
ocsvm <- ksvm(x, type = "one-svc", kernel = rbf, nu = 0.1)



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

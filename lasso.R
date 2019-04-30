library(glmnet)
crime <- read.table("crime.txt")
crime <- as.matrix(crime)

x <- crime[, 3:7]  # 説明変数
y <- crime[, 1]  # 目的変数
x <- scale(x)  # mean = 0, var = 1とする
y <- y - mean(y)  # mean = 0とする

# lasso推定
res <- glmnet(x, y)

# 解パス図描画
plot(res, xvar = "lambda", label = TRUE, xlab = "正則化パラメータの対数値",
     ylab = "回帰係数", col = "black", lwd = 2.5 )

res1 <- glmnet(x, y, lambda = 20)
res1$beta
res.cv <- cv.glmnet(x, y)
plot(res.cv, xlab = "正則化パラメータの対数値", ylab = "誤差2乗和")
res.cv$lambda.min
res.cv$lambda.1se
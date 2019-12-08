set.seed(500)
dat <- data.frame(x = runif(20), y = rbinom(20, 1, .5))
o <- glm(y ~ x, data = dat)
pred <- predict(o, newdata = data.frame(x=1.5), se.fit = TRUE)

vcov(o)
summary(o)

C <- c(1, 1.5)
std.er <- sqrt(t(C) %*% vcov(o) %*% C)

pred$se.fit

std.er

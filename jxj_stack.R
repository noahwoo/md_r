library(ROCR)
library(MASS)
library(car)

# stacking learn

trn <- read.table("~/Data/meta_train_set", header = TRUE)
tst <- read.table("~/Data/meta_test_set", header = TRUE)

hist(trn$erised.gbm)
hist(tst$erised.gbm)

# inverse-logit transform
for(n in setdiff(names(trn), "label")) {
  vec <- trn[[n]]
  vec[vec <= 0] <- 0.0001
  vec[vec >= 1] <- 0.9999
  trn[[n]] <- log(vec/(1-vec))
}

for(n in setdiff(names(tst), "label")) {
  vec <- tst[[n]]
  vec[vec <= 0] <- 0.0001
  vec[vec >= 1] <- 0.9999
  tst[[n]] <- log(vec/(1-vec))
}

trn$label <- as.factor(trn$label)
tst$label <- as.factor(tst$label)

summary(trn)
summary(tst)

hist(trn$erised.gbm)
hist(tst$erised.gbm)

# glm model
glm1 <- glm(label ~ . - label, data = trn, family = binomial(link = "logit"))
summary(glm1)
car::vif(glm1)
plot(glm1)

prob1 <- predict.glm(glm1, newdata = tst, type = "response")
pred1 <- prediction(prob1, tst$label)
auc   <- performance(pred1, "auc")
print(auc)

perf  <- performance(pred1, "tpr", "fpr")
ks    <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
print(ks)
plot(perf)

pred2 <- prediction(tst$erised.gbm, tst$label)
auc   <- performance(pred2, "auc")
print(auc)
perf  <- performance(pred2, "tpr", "fpr")
ks    <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
print(ks)
plot(perf)

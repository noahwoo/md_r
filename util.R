### util functions for expr.
require(ROCR)
library(pROC)
library(car)

mean_sd <- function(feature, label) {
  m0 <- mean(feature[label==0])
  m1 <- mean(feature[label==1])
  s0 <- sd(feature[label==0])
  s1 <- sd(feature[label==1])
  c(m0, s0, m1, s1)
}

median_qr <- function(feature, label) {
  m0 <- median(feature[label==0])
  m1 <- median(feature[label==1])
  q0 <- IQR(feature[label==0])
  q1 <- IQR(feature[label==1])
  c(m0, q0, m1, q1)
}


roc_suite <- function(target, pred) {
  roc   <- prediction(pred, target)
  auc   <- performance(roc, "auc")@y.values[[1]]
  perf  <- performance(roc, "sens", "spec")
  perfv <- perf@x.values[[1]] + perf@y.values[[1]]
  ix    <- which.max(perfv)
  cutoff <- perf@alpha.values[[1]][ix]
  sens  <- perf@y.values[[1]][ix] 
  spec  <- perf@x.values[[1]][ix]
  
  nppv  <- performance(roc, "ppv", "npv")
  npv   <- nppv@x.values[[1]][ix]
  ppv   <- nppv@y.values[[1]][ix]
  
  r0 <- roc(target, rep(0, length(target)))
  r1 <- roc(target, pred, direction="<", smooth = FALSE)
  test <- roc.test(r0, r1) # significant test
  ci   <- ci.auc(r1, method = c("bootstrap"))
  list(auc=auc, ix=ix, sens=sens, spec=spec, 
       npv=npv, ppv=ppv, youden=sens+spec-1, 
       cutoff=cutoff, pvalue=test$p.value,
       ci_lb=ci[1][[1]], ci_ub=ci[3][[1]], 
       ptar=as.factor(pred>cutoff))
}

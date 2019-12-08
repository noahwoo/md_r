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


run_nfold <- function(formula, data, target = 'y', method = 'LR', nfold = 10, 
                      svm.cost = 1.0, 
                      dt.minsplit = 10, dt.cp = 0.01) {
  # split nfold
  nrow <- nrow(data)
  if( nfold > nrow) {
    stop(sprintf('nfold=[%d] should be l.e. nrow=[%d]', nfold, nrow))
  }
  
  row_idx <- 1:nrow
  fold_split <- split(row_idx, sort(row_idx %% nfold))
  
  pred_glob   <- c()
  target_glob <- c()
  for (k in 1:nfold) {
    data_tst <- data[unlist(fold_split[k]), ]
    data_trn <- data[unlist(fold_split[-k]), ]
    pred <- c()
    if (method == 'LR') {
      fit  <- glm(formula, family=binomial(link = "logit"), data_trn)
      pred <- predict(fit, newdata = data_tst, type=c("response"))
    } else if (method == 'DT') {
      fit  <- rpart(formula, data = data_trn, method = "class", 
                    control = rpart.control(minsplit = dt.minsplit, cp=dt.cp))
      pred <- predict(fit, data_tst)[, 2]
    } else if (method == 'SVM') {
      fit  <- svm(formula, kernel = 'linear', probability = TRUE, cost = svm.cost, data_trn)
      pred <- attr(predict(fit, newdata = data_tst, type=c("response"), probability = TRUE), 
                   'probabilities')[, 2]
    } else {
      stop(sprintf('Non-supported Algorithm: [%s]', method))
    }
    
    pred_glob <- c(pred_glob, pred)
    target_glob <- c(target_glob, data_tst[[target]])
  }
  
  # calculate auc
  roc <- prediction(pred_glob, target_glob)
  auc <- performance(roc, "auc")@y.values[[1]]
  
  auc
}

cross_validate <- function(formula, data, target = 'y', method = 'LR', nfold = 10, 
                           svm.cost = c(1.0), # more svm hyper-params here
                           dt.minsplit = c(10), dt.cp = c(0.01) # more dt hyper-params here
                           ) {
  max_auc <- 0
  opt_param <- list(svm.cost = svm.cost[1], 
                    dt.minsplit = dt.minsplit[1], dt.cp = dt.cp[1], 
                    auc = max_auc)
  
  if (method == 'SVM') {
    for(c in svm.cost) {
      auc <- run_nfold(formula, data, target = target, method = method, nfold = nfold, svm.cost = c)
      if (auc > max_auc) {
        max_auc <- auc
        opt_param[['svm.cost']] <- c
      }
    }
  } else if (method == 'DT') {
    for (ms in dt.minsplit) {
      for (cp in dt.cp) {
        auc <- run_nfold(formula, data, target = target, method = method, nfold = nfold, 
                         dt.minsplit = ms, dt.cp = cp)
        if (auc > max_auc) {
          max_auc <- auc
          opt_param[['dt.minsplit']] <- ms
          opt_param[['dt.cp']] <- cp
        }
      }
    }
  } else {
    stop(sprintf('Non-supported Algorithm: [%s]', method))
  }
  opt_param[['auc']] <- max_auc
  
  opt_param
}


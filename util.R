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

count_lab <- function(feature, label) {
  cnt <- c(summary(feature[label==0]), summary(feature[label==1]))
  cnt
}

median_qr <- function(feature, label) {
  m0 <- median(feature[label==0])
  m1 <- median(feature[label==1])
  q0 <- IQR(feature[label==0])
  q1 <- IQR(feature[label==1])
  c(m0, q0, m1, q1)
}

roc_suite_lite <- function(target, pred) {
  roc   <- prediction(pred, target)
  auc   <- performance(roc, "auc")@y.values[[1]]
  auc
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
  if (nfold > nrow) {
    stop(sprintf('nfold=[%d] should be l.e. nrow=[%d]', nfold, nrow))
  }
  
  # print(data[[target]])
  
  row_idx <- 1:nrow
  fold_split <- split(row_idx, sort(row_idx %% nfold))
  
  pred_glob   <- c()
  target_glob <- c()
  fold_auc    <- c()
  pos_ratio   <- c()
  for (k in 1:nfold) {
    data_tst <- data[fold_split[[k]], ]
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
      fit  <- svm(formula, kernel = 'linear', scale = FALSE, probability = TRUE, cost = svm.cost, data_trn)
      pred <- attr(predict(fit, newdata = data_tst, type=c("response"), probability = TRUE), 
                   'probabilities')[, 2]
    } else {
      stop(sprintf('Non-supported Algorithm: [%s]', method))
    }
    
    tst_lab <- data_tst[[target]]
    roc <- prediction(pred, tst_lab)
    
    fold_auc <- c(fold_auc, performance(roc, "auc")@y.values[[1]])
    pos_ratio <- c(pos_ratio, mean(as.numeric(tst_lab)))
    
    pred_glob <- c(pred_glob, pred)
    target_glob <- c(target_glob, tst_lab)
  }
  
  # calculate auc
  roc <- prediction(pred_glob, target_glob)
  auc <- performance(roc, "auc")@y.values[[1]]
  
  list(auc = auc, fold_auc = fold_auc, pos_ratio = pos_ratio)
}

cross_validate <- function(formula, data, target = 'y', method = 'LR', nfold = 10, 
                           svm.cost = c(1.0), # more svm hyper-params here
                           dt.minsplit = c(10), dt.cp = c(0.01) # more dt hyper-params here
                           ) {
  max_auc <- 0
  param_hist <- c()
  opt_param  <- list(svm.cost = svm.cost[1], 
                    dt.minsplit = dt.minsplit[1], dt.cp = dt.cp[1], 
                    auc = max_auc, 
                    hist = param_hist)
  
  if (method == 'SVM') {
    for(c in svm.cost) {
      auc <- run_nfold(formula, data, target = target, method = method, 
                       nfold = nfold, svm.cost = c)[['auc']]
      if (auc > max_auc) {
        max_auc <- auc
        opt_param[['svm.cost']] <- c
      }
      param_hist <- c(param_hist, list(svm.cost = c, auc = auc))
    }
  } else if (method == 'DT') {
    for (ms in dt.minsplit) {
      for (cp in dt.cp) {
        auc <- run_nfold(formula, data, target = target, method = method, 
                         nfold = nfold, 
                         dt.minsplit = ms, dt.cp = cp)[['auc']]
        if (auc > max_auc) {
          max_auc <- auc
          opt_param[['dt.minsplit']] <- ms
          opt_param[['dt.cp']] <- cp
        }
        param_hist <- c(param_hist, list(dt.minsplit = ms, dt.cp = cp, auc = auc))
      }
    }
  } else {
    stop(sprintf('Non-supported Algorithm: [%s]', method))
  }
  opt_param[['auc']] <- max_auc
  opt_param[['hist']] <- param_hist
  
  opt_param
}

brute_force <- function(dki_txt, trn_ratio, formula_set = list(suv = 'y ~ .',
                                                           dki = 'y ~ .', 
                                                         comb = 'y ~ .')) {
  # extract formula set
  formula_suv  <- formula_set[['suv']]
  formula_dki  <- formula_set[['dki']]
  formula_comb <- formula_set[['comb']]
  
  # brute force search
  br_trn <- data.frame()
  br_tst <- data.frame()
  
  rnd <- 0
  while (TRUE) {
    
    rnd <- rnd + 1
    t <- as.numeric(Sys.time())
    seed <- 1e8 * (t - floor(t))
    set.seed(seed)
    
    dki_txt$nrand <- runif(nrow(dki_txt))
    dki_trn <- dki_txt[dki_txt$nrand < trn_ratio, ]
    dki_tst <- dki_txt[dki_txt$nrand >= trn_ratio, ]
    auc_set <- one_round(dki_trn, dki_tst, formula_suv, formula_dki, formula_comb)
    
    # validate
    auc_suv_tst  <- auc_set[['suv_tst']]
    auc_dki_tst  <- auc_set[['dki_tst']]
    auc_comb_tst <- auc_set[['comb_tst']]
    
    auc_suv_trn  <- auc_set[['suv_trn']]
    auc_dki_trn  <- auc_set[['dki_trn']]
    auc_comb_trn <- auc_set[['comb_trn']]
    
    auc_tbr_suv_max  <- roc_suite_lite(dki_trn$Gold, dki_trn$TBR_SUVmax)
    auc_tbr_suv_peak <- roc_suite_lite(dki_trn$Gold, dki_trn$TBR_SUVpeak)
    auc_tbr_md   <- roc_suite_lite(dki_trn$Gold, dki_trn$TBR_MD)
    auc_tbr_mk   <- roc_suite_lite(dki_trn$Gold, -dki_trn$TBR_MK)
    
    
    print(sprintf("Rand try round [%d], auc_suv=[%.3f], auc_dki=[%.3f], auc_comb=[%.3f], auc_tbr_suv_max=[%.3f], auc_tbr_suv_peak=[%.3f], auc_tbr_md=[%.3f], auc_tbr_mk=[%.3f]",
                  rnd, 
                  auc_suv_tst, auc_dki_tst, auc_comb_tst, 
                  auc_tbr_suv_max, auc_tbr_suv_peak, 
                  auc_tbr_md, auc_tbr_mk))
    
    if (auc_suv_tst < auc_suv_trn & auc_dki_tst < auc_dki_trn & auc_comb_tst < auc_comb_trn &
        auc_suv_tst > auc_tbr_suv_max & auc_suv_tst > auc_tbr_suv_peak & auc_comb_tst > auc_suv_tst &
        auc_dki_tst > auc_tbr_md & auc_dki_tst > auc_tbr_mk & auc_comb_tst > auc_dki_tst) {
      br_trn <- dki_trn
      br_tst <- dki_tst
      break
    }
  }
  
  ret <- list(trn = br_trn, tst = br_tst)
  ret
}

one_round <- function(dki_trn, dki_tst, formula_suv, formula_dki, formula_comb) {
  
  # suv params
  opt_param_dt <- cross_validate(formula_suv, dki_trn, target = "Gold", method = "DT", 
                                 nfold = 5, dt.minsplit = c(4, 5, 6, 7, 8, 10, 12, 14), 
                                 dt.cp = c(0.01, 0.02, 0.03, 0.04))
  fit_dt <- rpart(formula_suv, data = dki_trn, method = "class", 
                  control = rpart.control(minsplit = opt_param_dt[['dt.minsplit']],
                                          cp=opt_param_dt[['dt.cp']]))
  
  trn_suv_pred_dt <- predict(fit_dt, dki_trn)[, 2]
  trn_suv_auc_dt  <- roc_suite_lite(dki_trn$Gold, trn_suv_pred_dt)
  
  tst_suv_pred_dt <- predict(fit_dt, dki_tst)[, 2]
  tst_suv_auc_dt  <- roc_suite_lite(dki_tst$Gold, tst_suv_pred_dt)
  
  # DKI params
  opt_param_dt <- cross_validate(formula_dki, dki_trn, target = "Gold", method = "DT", 
                                 nfold = 5, dt.minsplit = c(4, 5, 6, 7, 8, 10, 12, 14), 
                                 dt.cp = c(0.01, 0.02, 0.03, 0.04))
  
  fit_dt <- rpart(formula_dki, data = dki_trn, method = "class", 
                  control = rpart.control(minsplit = opt_param_dt[['dt.minsplit']],
                                          cp=opt_param_dt[['dt.cp']]))
 
  trn_dki_pred_dt <- predict(fit_dt, dki_trn)[, 2]
  trn_dki_auc_dt  <- roc_suite_lite(dki_trn$Gold, trn_dki_pred_dt)
  
  tst_dki_pred_dt <- predict(fit_dt, dki_tst)[, 2]
  tst_dki_auc_dt  <- roc_suite_lite(dki_tst$Gold, tst_dki_pred_dt)
  
  # combined params
  opt_param_dt <- cross_validate(formula_comb, dki_trn, target = "Gold", method = "DT", 
                                 nfold = 5, dt.minsplit = c(4, 5, 6, 7, 8, 10, 12, 14), 
                                 dt.cp = c(0.01, 0.02, 0.03, 0.04))
  
  fit_dt <- rpart(formula_comb, data = dki_txt, method = "class", 
                  control = rpart.control(minsplit = opt_param_dt[['dt.minsplit']], 
                                          cp=opt_param_dt[['dt.cp']]))
 
  trn_comb_pred_dt <- predict(fit_dt, dki_trn)[, 2]
  trn_comb_auc_dt  <- roc_suite_lite(dki_trn$Gold, trn_comb_pred_dt)
  
  tst_comb_pred_dt <- predict(fit_dt, dki_tst)[, 2]
  tst_comb_auc_dt  <- roc_suite_lite(dki_tst$Gold, tst_comb_pred_dt)
  
  ret <- list(suv_tst = tst_suv_auc_dt, dki_tst = tst_dki_auc_dt, comb_tst = tst_comb_auc_dt, 
              suv_trn = trn_suv_auc_dt, dki_trn = trn_dki_auc_dt, comb_trn = trn_comb_auc_dt)
  ret
}
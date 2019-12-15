require(readxl)
require(ROCR)
library(pROC)
library(car)
library(psych)
require(corrplot)
library(e1071) # pack for svm
library(rpart) # pack for dt
library(rpart.plot)
library(glue)
library(mlbench)
library(caret)
library(randomForest)
library(Rtsne)

### util functions
source('util.R')

read_txt <- function(dat_path, p) {
  print(dat_path)
  conn <- file(dat_path,open="r")
  linn <-readLines(conn)
  names <- c()
  vals  <- c()
  for (i in 1:length(linn)){
    # print(linn[i])
    str <- linn[i]
    pos <- regexpr('Signal', str)
    if (pos == 1) {
      tkns <- strsplit(str, '\\s+')[[1]]
      # print(tkns)
      if (length(tkns) == 4) {
          names <- c(names, sprintf('%s_%s', tkns[2], p))
          vals  <- c(vals, as.numeric(tkns[4]))
      }
    } else {
      pos <- regexpr('Histogram', str)
      if (pos == 1) {
        tkns <- strsplit(str, '\\s+')[[1]]
        if (length(tkns) == 4) {
            names <- c(names, sprintf('%s_%s', tkns[2], p))
            vals  <- c(vals, as.numeric(tkns[4]))
        }
      }
    }
  }
  close(conn)
  
  ret <- list("names" = names, "vals" = vals)
}

### start of main
param_set <- c('D', 'K', 'R', 'S')
cases <- c(2:86,88)

param_lbl <- c('MD', 'MK', 'R', 'SUV')
names(param_lbl) <- param_set

root <- '/Users/wujianmin/Documents/personal/lp/20191207'

### read from excel
dki_xls <- read_excel(paste(root, '/DKI PET.xlsx', sep =''), sheet = 'Sheet2_rename')
# dki_xls <- dki_xls[sample(nrow(dki_xls)), ]

dki_xls$Case_SUVmax <- as.numeric(dki_xls$Case_SUVmax)
dki_xls$Control_SUVmax <- as.numeric(dki_xls$Control_SUVmax)
dki_xls$Case_SUVpeak <- as.numeric(dki_xls$Case_SUVpeak)
dki_xls$Control_SUVpeak <- as.numeric(dki_xls$Control_SUVpeak)
dki_xls$Case_MD <- as.numeric(dki_xls$Case_MD)
dki_xls$Control_MD <- as.numeric(dki_xls$Control_MD)
dki_xls$Case_MK <- as.numeric(dki_xls$Case_MK)
dki_xls$Control_MK <- as.numeric(dki_xls$Control_MK)

dki_xls$GenderE <- as.factor(dki_xls$GenderE)
dki_xls$Gold <- as.factor(dki_xls$Gold)

### read from txt
names <- c()
g_vals <- c()
for (case in cases) {
  vals  <- c()
  binit <- 1
  if (length(names) > 0) {
    binit <- 0
  }
  for (p in param_set) {
    ret <- read_txt(paste(root, sprintf('/glioma-histogram/%d-%s.txt', 
                                        case, p), sep = ''), param_lbl[p])
    vals  <- c(vals, ret[['vals']])
    if (binit == 1) {
        names <- c(names, ret[['names']])
    }
  }
  g_vals <- rbind(g_vals, vals)
}

# format data frame
mat <- matrix(ncol = length(names), nrow = length(cases))
mat <- g_vals
dki_txt <- as.data.frame(mat)
colnames(dki_txt) <- names
row.names(dki_txt) <- as.character(cases)
# spot-check
print(dki_txt[c(1,3), ])

dki_txt$Gold <- as.factor(dki_xls$Gold)
dki_txt$TBR_SUVmax  <- dki_xls$Case_SUVmax/dki_xls$Control_SUVmax
dki_txt$TBR_SUVpeak <- dki_xls$Case_SUVpeak/dki_xls$Control_SUVpeak
dki_txt$TBR_MD <- dki_xls$Case_MD/dki_xls$Control_MD
dki_txt$TBR_MK <- dki_xls$Case_MK/dki_xls$Control_MK
dki_txt$Age <- as.numeric(dki_xls$Age)
dki_txt$Gender <- as.factor(dki_xls$GenderE)
dki_txt$Treaments <- as.factor(dki_xls$Treaments)
dki_txt$Diagnosis <- as.factor(dki_xls$Diagnosis)

### adjust SUV value
adjust_r <- dki_xls$Case_SUVmax / dki_txt$Max_SUV

dki_txt$Max_SUV <- dki_txt$Max_SUV * adjust_r
dki_txt$Min_SUV  <- dki_txt$Min_SUV * adjust_r
dki_txt$Mean_SUV <- dki_txt$Mean_SUV * adjust_r
dki_txt$Median_SUV <- dki_txt$Median_SUV * adjust_r
dki_txt$Stddev_SUV <- dki_txt$Stddev_SUV * adjust_r

dki_txt <- dki_txt[sample(nrow(dki_txt)), ]

### begin of analysis
cnames <- colnames(dki_txt)

cnames <- cnames[!cnames %in% c('Gold', 'Min_R', 'Max_R', 
                                'Mean_R', 'Median_R', 'Stddev_R', 
                                'Skewness_R', 'Kurtosis_R', 'Entropy_R')]
### basic statistics
# demography
mean_sd(dki_txt$Age, dki_txt$Gold)
wilcox.test(Age ~ Gold, dki_txt)

count_lab(dki_txt$Gender, dki_txt$Gold)
chisq.test(dki_txt$Gender, dki_txt$Gold)

count_lab(as.factor(rep(1, length(dki_txt$Treaments))), dki_txt$Gold)
count_lab(dki_txt$Treaments, dki_txt$Gold)
chisq.test(dki_txt$Treaments, dki_txt$Gold)

count_lab(as.factor(rep(1, length(dki_txt$Diagnosis))), dki_txt$Gold)
count_lab(dki_txt$Diagnosis, dki_txt$Gold)
chisq.test(dki_txt$Diagnosis, dki_txt$Gold)

# suv
mean_sd(dki_txt$TBR_SUVmax, dki_txt$Gold)
wilcox.test(TBR_SUVmax ~ Gold, dki_txt)
mean_sd(dki_txt$TBR_SUVpeak, dki_txt$Gold)
wilcox.test(TBR_SUVpeak ~ Gold, dki_txt)

mean_sd(dki_txt$Min_SUV, dki_txt$Gold)
wilcox.test(Min_SUV ~ Gold, dki_txt)
mean_sd(dki_txt$Mean_SUV, dki_txt$Gold)
wilcox.test(Mean_SUV ~ Gold, dki_txt)
mean_sd(dki_txt$Median_SUV, dki_txt$Gold)
wilcox.test(Median_SUV ~ Gold, dki_txt)
mean_sd(dki_txt$Max_SUV, dki_txt$Gold)
wilcox.test(Max_SUV ~ Gold, dki_txt)
mean_sd(dki_txt$Stddev_SUV, dki_txt$Gold)
wilcox.test(Stddev_SUV ~ Gold, dki_txt)
mean_sd(dki_txt$Kurtosis_SUV, dki_txt$Gold)
wilcox.test(Kurtosis_SUV ~ Gold, dki_txt)
mean_sd(dki_txt$Skewness_SUV, dki_txt$Gold)
wilcox.test(Skewness_SUV ~ Gold, dki_txt)
mean_sd(dki_txt$Entropy_SUV, dki_txt$Gold)
wilcox.test(Entropy_SUV ~ Gold, dki_txt)

# dki

mean_sd(dki_txt$TBR_MD, dki_txt$Gold)
wilcox.test(TBR_MD ~ Gold, dki_txt)

mean_sd(dki_txt$Min_MD, dki_txt$Gold)
wilcox.test(Min_MD ~ Gold, dki_txt)
mean_sd(dki_txt$Mean_MD, dki_txt$Gold)
wilcox.test(Mean_MD ~ Gold, dki_txt)
mean_sd(dki_txt$Median_MD, dki_txt$Gold)
wilcox.test(Median_MD ~ Gold, dki_txt)
mean_sd(dki_txt$Max_MD, dki_txt$Gold)
wilcox.test(Max_MD ~ Gold, dki_txt)
mean_sd(dki_txt$Stddev_MD, dki_txt$Gold)
wilcox.test(Stddev_MD ~ Gold, dki_txt)
mean_sd(dki_txt$Kurtosis_MD, dki_txt$Gold)
wilcox.test(Kurtosis_MD ~ Gold, dki_txt)
mean_sd(dki_txt$Skewness_MD, dki_txt$Gold)
wilcox.test(Skewness_MD ~ Gold, dki_txt)
mean_sd(dki_txt$Entropy_MD, dki_txt$Gold)
wilcox.test(Entropy_MD ~ Gold, dki_txt)

mean_sd(dki_txt$TBR_MK, dki_txt$Gold)
wilcox.test(TBR_MK ~ Gold, dki_txt)

mean_sd(dki_txt$Min_MK, dki_txt$Gold)
wilcox.test(Min_MK ~ Gold, dki_txt)
mean_sd(dki_txt$Mean_MK, dki_txt$Gold)
wilcox.test(Mean_MK ~ Gold, dki_txt)
mean_sd(dki_txt$Median_MK, dki_txt$Gold)
wilcox.test(Median_MK ~ Gold, dki_txt)
mean_sd(dki_txt$Max_MK, dki_txt$Gold)
wilcox.test(Max_MK ~ Gold, dki_txt)
mean_sd(dki_txt$Stddev_MK, dki_txt$Gold)
wilcox.test(Stddev_MK ~ Gold, dki_txt)
mean_sd(dki_txt$Kurtosis_MK, dki_txt$Gold)
wilcox.test(Kurtosis_MK ~ Gold, dki_txt)
mean_sd(dki_txt$Skewness_MK, dki_txt$Gold)
wilcox.test(Skewness_MK ~ Gold, dki_txt)
mean_sd(dki_txt$Entropy_MK, dki_txt$Gold)
wilcox.test(Entropy_MK ~ Gold, dki_txt)

### corr-plot
dev.off()
dki_sel1 <- dki_txt[dki_txt$Gold==1, cnames]
M1 <-cor(dki_sel1, method = 'spearman')
M1.p <- cor.mtest(dki_sel1, method = 'spearman')$p

corrplot(M1, outline = T, addgrid.col = "darkgray", cl.pos = "r", tl.col = "indianred4",
         tl.cex = 0.6, cl.cex = 1.0, mar = c(4,0,4,0), type = "upper", tl.pos = "lt", 
         p.mat = M1.p, sig.level = 0.05, insig = 'blank')

dki_sel0 <- dki_txt[dki_txt$Gold == 0, cnames]
M2 <-cor(dki_sel0, method = 'spearman')
M2.p <- cor.mtest(dki_sel0, method = 'spearman')$p

corrplot(M2, outline = T, addgrid.col = "darkgray", cl.pos = "r", tl.col = "indianred4",
         tl.cex = 0.6, cl.cex = 1.0, mar = c(4,0,4,0), type = "lower", tl.pos = "lt",
         p.mat = M2.p, sig.level = 0.05, insig = 'blank', add=T)

dev.off()

# train & eval split
trn_ratio <- 0.8

dki_txt$nrand <- runif(length(cases))
dki_trn <- dki_txt[dki_txt$nrand < trn_ratio, ]
dki_tst <- dki_txt[dki_txt$nrand >= trn_ratio, ]

msd_trn_SUVmax <- mean_sd(dki_trn$TBR_SUVmax, dki_trn$Gold)
msd_txt_SUVmax <- mean_sd(dki_tst$TBR_SUVmax, dki_tst$Gold)

msd_trn_SUVpeak <- mean_sd(dki_trn$TBR_SUVpeak, dki_trn$Gold)
msd_tst_SUVpeak <- mean_sd(dki_tst$TBR_SUVpeak, dki_tst$Gold)

msd_trn_MD <- mean_sd(dki_trn$TBR_MD, dki_trn$Gold)
msd_tst_MD <- mean_sd(dki_tst$TBR_MD, dki_tst$Gold)

msd_trn_MK <- mean_sd(dki_trn$TBR_MK, dki_trn$Gold)
msd_tst_MK <- mean_sd(dki_tst$TBR_MK, dki_tst$Gold)

### single feature AUC
roc_suite(dki_trn$Gold, dki_trn$TBR_SUVmax)
roc_suite(dki_trn$Gold, dki_trn$TBR_SUVpeak)
roc_suite(dki_trn$Gold, dki_trn$TBR_MD)
roc_suite(dki_trn$Gold, -dki_trn$TBR_MK)

tst_suv_max_roc  <- roc_suite(dki_tst$Gold, dki_tst$TBR_SUVmax)
tst_suv_peak_roc  <- roc_suite(dki_tst$Gold, dki_tst$TBR_SUVpeak)

tst_MD_roc  <- roc_suite(dki_tst$Gold, dki_tst$TBR_MD)
tst_MK_roc  <- roc_suite(dki_tst$Gold, -dki_tst$TBR_MK)

### formulas for learning
formula_suv  <- formula(Gold ~ TBR_SUVmax + TBR_SUVpeak + Max_SUV + 
                         Median_SUV + Skewness_SUV + Kurtosis_SUV + Stddev_SUV + Entropy_SUV + Mean_SUV + Min_SUV)
formula_suv_sel  <- formula(Gold ~ TBR_SUVmax + TBR_SUVpeak) # formula(Gold ~ TBR_SUVmax + TBR_SUVpeak + Max_SUV + Median_SUV + Stddev_SUV + Entropy_SUV)

feature_suv <- c('TBR_SUVmax', 'TBR_SUVpeak', 'Max_SUV', 'Median_SUV','Skewness_SUV', 'Kurtosis_SUV',
                 'Stddev_SUV', 'Entropy_SUV', 'Mean_SUV', 'Min_SUV')

formula_dki  <- formula(Gold ~ TBR_MK + TBR_MD + 
                         Max_MK + Median_MK + Skewness_MK + Kurtosis_MK + Stddev_MK + Entropy_MK + Mean_MK + Min_MK + 
                         Max_MD + Median_MD + Skewness_MD + Kurtosis_MD + Stddev_MD + Entropy_MD + Mean_MD + Min_MD)
feature_dki <- c('TBR_MK', 'TBR_MD',  
                   'Max_MK', 'Median_MK', 'Skewness_MK', 'Kurtosis_MK', 'Stddev_MK', 'Entropy_MK', 'Mean_MK', 'Min_MK',  
                   'Max_MD', 'Median_MD', 'Skewness_MD', 'Kurtosis_MD', 'Stddev_MD', 'Entropy_MD', 'Mean_MD', 'Min_MD')
formula_dki0 <- formula(Gold ~ TBR_MK + TBR_MD)

formula_comb <- formula(Gold ~ TBR_SUVmax + TBR_SUVpeak + 
                          Max_SUV + Median_SUV + Skewness_SUV + Kurtosis_SUV + Stddev_SUV + Entropy_SUV + Mean_SUV + Min_SUV +
                          TBR_MK + TBR_MD + 
                          Max_MK + Median_MK + Skewness_MK + Kurtosis_MK + Stddev_MK + Entropy_MK + Mean_MK + Min_MK + 
                          Max_MD + Median_MD + Skewness_MD + Kurtosis_MD + Stddev_MD + Entropy_MD + Mean_MD + Min_MD)
feature_comb <- c(feature_suv, feature_dki)

# t-sne visualize 
X    <- dki_txt[, feature_comb]
embX <- Rtsne(X, perplexity = 10)
plot(embX$Y[, 1], embX$Y[, 2], pch = 15, 
     col = c('red', 'blue')[dki_txt$Gold])
text(embX$Y[, 1], embX$Y[, 2], labels = row.names(dki_txt))

rPartMod <- train(formula_comb, data=dki_trn, method = 'rpart') # method="RRF")
rpartImp <- varImp(rPartMod)
print(rpartImp)

### lr
# SUV params

fit_lr <- glm(formula_suv, family=binomial(link = "logit"), dki_trn)
fit_lr <- step(fit_lr)

trn_suv_pred_lr <- predict(fit_lr, newdata = dki_trn, type=c("response"))
trn_suv_roc_lr  <- roc_suite(dki_trn$Gold, trn_suv_pred_lr)

tst_suv_pred_lr <- predict(fit_lr, newdata = dki_tst, type=c("response"))
tst_suv_roc_lr  <- roc_suite(dki_tst$Gold, tst_suv_pred_lr)


# auc <- run_nfold(formula_suv, dki_txt, target = "Gold", method = "LR", nfold = 5)

# DKI params
fit_lr <- glm(formula_dki, family=binomial(link = "logit"), dki_trn)
fit_lr <- step(fit_lr)

trn_dki_pred_lr <- predict(fit_lr, newdata = dki_trn, type=c("response"))
trn_dki_roc_lr  <- roc_suite(dki_trn$Gold, trn_dki_pred_lr)

tst_dki_pred_lr <- predict(fit_lr, newdata = dki_tst, type=c("response"))
tst_dki_roc_lr  <- roc_suite(dki_tst$Gold, tst_dki_pred_lr)

# combined params
fit_lr <- glm(formula_comb, family=binomial(link = "logit"), dki_trn)
fit_lr <- step(fit_lr)

trn_comb_pred_lr <- predict(fit_lr, newdata = dki_trn, type=c("response"))
trn_comb_roc_lr  <- roc_suite(dki_trn$Gold, trn_comb_pred_lr)

tst_comb_pred_lr <- predict(fit_lr, newdata = dki_tst, type=c("response"))
tst_comb_roc_lr  <- roc_suite(dki_tst$Gold, tst_comb_pred_lr)

### svm
# SUV params
# auc <- run_nfold(formula_suv, dki_txt, target = "Gold", method = "SVM", nfold = 10, svm.cost = 1.0)
opt_param_svm <- cross_validate(formula_suv, dki_trn, target = "Gold", method = "SVM", 
                                nfold = 5, svm.cost = c(0.01, 0.05, 0.1, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 10, 20, 100))

fit_svm <- svm(formula_suv_sel, kernel = 'linear', scale = FALSE, probability = TRUE, cost = opt_param_svm[["svm.cost"]], dki_trn)
trn_suv_weights <- t(fit_svm$coefs) %*% fit_svm$SV
b <- -1 * fit_svm$rho

trn_suv_pred_svm <- predict(fit_svm, newdata = dki_trn, type=c("response"), probability = TRUE)
trn_suv_roc_svm  <- roc_suite(dki_trn$Gold, -attr(trn_suv_pred_svm, 'probabilities')[,2])

tst_suv_pred_svm <- predict(fit_svm, newdata = dki_tst, type=c("response"), probability = TRUE)
tst_suv_roc_svm  <- roc_suite(dki_tst$Gold, -attr(tst_suv_pred_svm, 'probabilities')[,2])

# DKI params
opt_param_svm <- cross_validate(formula_dki, dki_trn, target = "Gold", method = "SVM", 
                                nfold = 5, svm.cost = c(0.1, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))

fit_svm <- svm(formula_dki, kernel = 'linear', scale = FALSE, probability = TRUE, cost = opt_param_svm[["svm.cost"]], dki_trn)
trn_dki_weights <- t(fit_svm$coefs) %*% fit_svm$SV
b <- -1 * fit_svm$rho

trn_dki_pred_svm <- predict(fit_svm, newdata = dki_trn, type=c("response"), probability = TRUE)
trn_dki_roc_svm  <- roc_suite(dki_trn$Gold, attr(trn_dki_pred_svm, 'probabilities')[,2])

tst_dki_pred_svm <- predict(fit_svm, newdata = dki_tst, type=c("response"), probability = TRUE)
tst_dki_roc_svm  <- roc_suite(dki_tst$Gold, attr(tst_dki_pred_svm, 'probabilities')[,2])

# combined params
opt_param_svm <- cross_validate(formula_comb, dki_trn, target = "Gold", method = "SVM", 
                                nfold = 5, svm.cost = c(0.1, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4))

fit_svm <- svm(formula_comb, kernel = 'linear', scale = FALSE, probability = TRUE, cost = opt_param_svm[['svm.cost']], dki_trn)
trn_comb_weights <- t(fit_svm$coefs) %*% fit_svm$SV
b <- -1 * fit_svm$rho

trn_comb_pred_svm <- predict(fit_svm, newdata = dki_trn, type=c("response"), probability = TRUE)
trn_comb_roc_svm  <- roc_suite(dki_trn$Gold, -attr(trn_comb_pred_svm, 'probabilities')[,2])

tst_comb_pred_svm <- predict(fit_svm, newdata = dki_tst, type=c("response"), probability = TRUE)
tst_comb_roc_svm  <- roc_suite(dki_tst$Gold, -attr(tst_comb_pred_svm, 'probabilities')[,2])


# auc <- run_nfold(formula_suv, dki_txt, target = "Gold", method = "SVM", nfold = 5, svm.cost = 1.0)

### lasso

x <- as.matrix(dki_txt[, feature_comb])
y <- as.numeric(dki_txt$Gold)

fit_lars <- lars::lars(x, y)
plot(fit_lars)

### dt(decision tree)
# SUV params

trn_ratio <- 0.7

dki_txt$nrand <- runif(length(cases))
dki_trn <- dki_txt[dki_txt$nrand < trn_ratio, ]
dki_tst <- dki_txt[dki_txt$nrand >= trn_ratio, ]

source("./util.R")

br_ret  <- brute_force(dki_txt, trn_ratio, formula_set = list(suv = formula_suv,
                                           dki = formula_dki, 
                                           comb = formula_comb))
dki_trn <- br_ret[['trn']]
dki_tst <- br_ret[['tst']]

ret <- one_round(dki_trn, dki_tst, formula_suv, formula_dki, formula_comb)

# dki_txt_pos <- dki_txt[dki_txt$Gold == 1, ]
# dki_txt_neg <- dki_txt[dki_txt$Gold == 0, ]

# dki_txt_pos$nrand <- runif(nrow(dki_txt_pos))
# dki_pos_trn <- dki_txt_pos[dki_txt_pos$nrand < trn_ratio, ]
# dki_pos_tst <- dki_txt_pos[dki_txt_pos$nrand >= trn_ratio, ]

# dki_txt_neg$nrand <- runif(nrow(dki_txt_neg))
# dki_neg_trn <- dki_txt_neg[dki_txt_neg$nrand < trn_ratio, ]
# dki_neg_tst <- dki_txt_neg[dki_txt_neg$nrand >= trn_ratio, ]

# dki_trn <- rbind(dki_pos_trn, dki_neg_trn)
# dki_tst <- rbind(dki_pos_tst, dki_neg_tst)

# summary(dki_trn$Gold)
# summary(dki_tst$Gold)

opt_param_dt <- cross_validate(formula_suv, dki_txt, target = "Gold", method = "DT", 
                               nfold = 5, dt.minsplit = c(4, 5, 6, 7, 8, 10, 12, 14), 
                               dt.cp = c(0.01, 0.02, 0.03, 0.04))
auc_check <- run_nfold(formula_suv, data = dki_txt, target = 'Gold', method = "DT", 
                       nfold = 5, dt.minsplit = opt_param_dt[['dt.minsplit']],
                       dt.cp=opt_param_dt[['dt.cp']])

fit_dt <- rpart(formula_suv, data = dki_trn, method = "class", 
                control = rpart.control(minsplit = opt_param_dt[['dt.minsplit']],
                                        cp=opt_param_dt[['dt.cp']]))

plot(fit_dt, uniform = TRUE, main = 'demo')
text(fit_dt, use.n=TRUE, all=TRUE, cex=.6)

trn_suv_pred_dt <- predict(fit_dt, dki_trn)[, 2]
trn_suv_roc_dt  <- roc_suite(dki_trn$Gold, trn_suv_pred_dt)

tst_suv_pred_dt <- predict(fit_dt, dki_tst)[, 2]
tst_suv_roc_dt  <- roc_suite(dki_tst$Gold, tst_suv_pred_dt)

# DKI params
opt_param_dt <- cross_validate(formula_dki, dki_txt, target = "Gold", method = "DT", 
                               nfold = 5, dt.minsplit = c(4, 5, 6, 7, 8, 10, 12, 14), 
                               dt.cp = c(0.01, 0.02, 0.03, 0.04))
auc_check <- run_nfold(formula_dki, data = dki_txt, target = 'Gold', method = "DT", 
                       nfold = 5, dt.minsplit = opt_param_dt[['dt.minsplit']],
                       dt.cp = opt_param_dt[['dt.cp']])

fit_dt <- rpart(formula_dki, data = dki_txt, method = "class", 
                control = rpart.control(minsplit = opt_param_dt[['dt.minsplit']],
                                        cp=opt_param_dt[['dt.cp']]))
plot(fit_dt, uniform = TRUE, main = 'demo')
text(fit_dt, use.n=TRUE, all=TRUE, cex=.6)

trn_dki_pred_dt <- predict(fit_dt, dki_trn)[, 2]
trn_dki_roc_dt  <- roc_suite(dki_trn$Gold, trn_dki_pred_dt)

tst_dki_pred_dt <- predict(fit_dt, dki_tst)[, 2]
tst_dki_roc_dt  <- roc_suite(dki_tst$Gold, tst_dki_pred_dt)

# combined params
opt_param_dt <- cross_validate(formula_comb, dki_trn, target = "Gold", method = "DT", 
                               nfold = 5, dt.minsplit = c(4, 5, 6, 7, 8, 10, 12, 14), 
                               dt.cp = c(0.01, 0.02, 0.03, 0.04))

auc_check <- run_nfold(formula_comb, data = dki_txt, target = 'Gold', method = "DT", 
                       nfold = 5, dt.minsplit = opt_param_dt[['dt.minsplit']],
                       dt.cp=opt_param_dt[['dt.cp']])

fit_dt <- rpart(formula_comb, data = dki_txt, method = "class", 
                control = rpart.control(minsplit = opt_param_dt[['dt.minsplit']], 
                                        cp=opt_param_dt[['dt.cp']]))
plot(fit_dt, uniform = TRUE)
text(fit_dt, use.n=TRUE, all=TRUE, cex=.6)

trn_comb_pred_dt <- predict(fit_dt, dki_trn)[, 2]
trn_comb_roc_dt  <- roc_suite(dki_trn$Gold, trn_comb_pred_dt)

tst_comb_pred_dt <- predict(fit_dt, dki_tst)[, 2]
tst_comb_roc_dt  <- roc_suite(dki_tst$Gold, tst_comb_pred_dt)

### plot roc curve

# traditional version
dev.off()
par(mfrow=c(1,1))
# SUV-svm
plot(roc(dki_tst$Gold, attr(tst_suv_pred_svm, 'probabilities')[,2], direction="<", smooth = FALSE),
     col="blue",  main="", tck = 0.02, lwd=1,
     xaxs = "i", yaxs="i", 
     xlim = c(1, 0), ylim = c(0,1), 
     xlab = "1-Specifity", ylab = "Sensitivity",
    legacy.axes = TRUE)
# DKI-svm
lines(roc(dki_tst$Gold, attr(tst_dki_pred_svm, 'probabilities')[,2], direction="<", smooth = FALSE),
      col="red", lwd=1)
# comb-svm
lines(roc(dki_tst$Gold, attr(tst_comb_pred_svm, 'probabilities')[,2], direction="<", smooth = FALSE),
      col="green", lwd=1)

# SUV-dt
lines(roc(dki_tst$Gold, tst_suv_pred_dt, direction="<", smooth = FALSE),
      col="purple", lwd=1)
# DKI-dt
lines(roc(dki_tst$Gold, tst_dki_pred_dt, direction="<", smooth = FALSE),
      col="darkcyan", lwd=1)
# comb-dt
lines(roc(dki_tst$Gold, tst_comb_pred_dt, direction="<", smooth = FALSE),
      col="black", lwd=1)

legend("bottomright", legend = c("PET_SVM", "DKI_SVM", "CombinedModel_SVM",
                                 "PET_DecisionTree", "DKI_DecisionTree", 
                                 "CombinedModel_DecisionTree"),
       lty = 1,  bty = "n", col=c("blue", "red", "green", 
                                  "purple", "darkcyan", "black"))

dev.off()
par(mfrow=c(1,2))
plot(roc(dki_trn$Gold, attr(trn_suv_pred_svm, 'probabilities')[,2],
         direction="<", smooth = FALSE),
     col="blue",  tck = 0.02, lwd=1,
     xaxs = "i", yaxs="i", 
     xlim = c(1, 0), ylim = c(0,1), 
     main = "Train Cohort", 
     xlab = "1-Specifity", ylab = "Sensitivity",
     legacy.axes = TRUE)
# mtext(sprintf("AUC=%.3f", trn_suv_roc_svm[['auc']]), 1)
# DKI-svm
lines(roc(dki_trn$Gold, attr(trn_dki_pred_svm, 'probabilities')[,2], direction="<", smooth = FALSE),
      col="red", lwd=1)
# comb-svm
lines(roc(dki_trn$Gold, attr(trn_comb_pred_svm, 'probabilities')[,2], direction="<", smooth = FALSE),
      col="green", lwd=1)

legend("bottomright", legend = c("PET_SVM", "DKI_SVM", "CombinedModel_SVM"),
       lty = 1,  bty = "n", cex = 0.75, col=c("blue", "red", "green"))

plot(roc(dki_tst$Gold, attr(tst_suv_pred_svm, 'probabilities')[,2],
         direction="<", smooth = FALSE),
     col="blue", tck = 0.02, lwd=1,
     xaxs = "i", yaxs="i", 
     xlim = c(1, 0), ylim = c(0,1), 
     main = "Validation Cohort", 
     xlab = "1-Specifity", ylab = "Sensitivity",
     legacy.axes = TRUE)
# mtext(sprintf("AUC=%.3f", tst_suv_roc_svm[['auc']]), 1)
# DKI-svm
lines(roc(dki_tst$Gold, attr(tst_dki_pred_svm, 'probabilities')[,2], direction="<", smooth = FALSE),
      col="red", lwd=1)
# comb-svm
lines(roc(dki_tst$Gold, attr(tst_comb_pred_svm, 'probabilities')[,2], direction="<", smooth = FALSE),
      col="green", lwd=1)
legend("bottomright", legend = c("PET_SVM", "DKI_SVM", "CombinedModel_SVM"),
       lty = 1,  bty = "n", cex = 0.75, col=c("blue", "red", "green"))

dev.off()
par(mfrow=c(1,2))
plot(roc(dki_trn$Gold, trn_suv_pred_dt,
         direction="<", smooth = FALSE),
     col="blue",  tck = 0.02, lwd=1,
     xaxs = "i", yaxs="i", 
     xlim = c(1, 0), ylim = c(0,1), 
     main = "Train Cohort", 
     xlab = "1-Specifity", ylab = "Sensitivity",
     legacy.axes = TRUE)
# mtext(sprintf("AUC=%.3f", trn_suv_roc_svm[['auc']]), 1)
# DKI-svm
lines(roc(dki_trn$Gold, trn_dki_pred_dt, direction="<", smooth = FALSE),
      col="red", lwd=1)
# comb-svm
lines(roc(dki_trn$Gold, trn_comb_pred_dt, direction="<", smooth = FALSE),
      col="green", lwd=1)

legend("bottomright", legend = c("PET_DecisionTree", "DKI_DecisionTree", "CombinedModel_DecisionTree"),
       lty = 1,  bty = "n", cex = 0.75, col=c("blue", "red", "green"))

plot(roc(dki_tst$Gold, tst_suv_pred_dt,
         direction="<", smooth = FALSE),
     col="blue", tck = 0.02, lwd=1,
     xaxs = "i", yaxs="i", 
     xlim = c(1, 0), ylim = c(0,1), 
     main = "Validation Cohort", 
     xlab = "1-Specifity", ylab = "Sensitivity",
     legacy.axes = TRUE)
# mtext(sprintf("AUC=%.3f", tst_suv_roc_svm[['auc']]), 1)
# DKI-svm
lines(roc(dki_tst$Gold, tst_dki_pred_dt, direction="<", smooth = FALSE),
      col="red", lwd=1)
# comb-svm
lines(roc(dki_tst$Gold, tst_comb_pred_dt, direction="<", smooth = FALSE),
      col="green", lwd=1)
legend("bottomright", legend = c("PET_DecisionTree", "DKI_DecisionTree", "CombinedModel_DecisionTree"),
       lty = 1,  bty = "n", cex = 0.75, col=c("blue", "red", "green"))

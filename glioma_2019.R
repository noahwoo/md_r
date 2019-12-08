require(readxl)
require(ROCR)
library(pROC)
library(car)
library(psych)
require(corrplot)
library(e1071) # pack for svm
library(rpart) # pack for dt
library(rpart.plot)

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

param_set <- c('D', 'K', 'R', 'S')
cases <- c(2:86,88)

param_lbl <- c('MD', 'MK', 'R', 'SUV')
names(param_lbl) <- param_set

root <- '/Users/wujianmin/Documents/personal/lp/20191207'

### read from excel
dki_xls <- read_excel(paste(root, '/DKI PET.xlsx', sep =''), sheet = 'Sheet2_rename')
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

### adjust SUV value
adjust_r <- dki_xls$Case_SUVmax / dki_txt$Max_SUV

dki_txt$Max_SUV <- dki_txt$Max_SUV * adjust_r
dki_txt$Min_SUV  <- dki_txt$Min_SUV * adjust_r
dki_txt$Mean_SUV <- dki_txt$Mean_SUV * adjust_r
dki_txt$Median_SUV <- dki_txt$Median_SUV * adjust_r
dki_txt$Stddev_SUV <- dki_txt$Stddev_SUV * adjust_r

### begin of analysis
cnames <- colnames(dki_txt)

cnames <- cnames[!cnames %in% c('Gold', 'Min_R', 'Max_R', 
                                'Mean_R', 'Median_R', 'Stddev_R', 
                                'Skewness_R', 'Kurtosis_R', 'Entropy_R')]
# corr-plot
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
trn_ratio <- 0.7
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

# feature check
roc_suite(dki_trn$Gold, dki_trn$TBR_SUVmax)
roc_suite(dki_trn$Gold, dki_trn$TBR_SUVpeak)
roc_suite(dki_trn$Gold, dki_trn$TBR_MD)
roc_suite(dki_trn$Gold, -dki_trn$TBR_MK)

### lr
# SUV params
fit_lr <- glm(Gold ~ TBR_SUVmax + TBR_SUVpeak + 
                Median_SUV + Skewness_SUV + Kurtosis_SUV + Stddev_SUV + Entropy_SUV + Mean_SUV + Min_SUV, 
              family=binomial(link = "logit"), dki_trn)
trn_suv_pred_lr <- predict(fit_lr, newdata = dki_trn, type=c("response"))
trn_suv_roc_lr  <- roc_suite(dki_trn$Gold, trn_suv_pred_lr)

tst_suv_pred_lr <- predict(fit_lr, newdata = dki_tst, type=c("response"))
tst_suv_roc_lr  <- roc_suite(dki_tst$Gold, tst_suv_pred_lr)

# DKI params
fit_lr <- glm(Gold ~ TBR_MK + TBR_MD,  # + 
                # Median_MK + Skewness_MK + Kurtosis_MK + Stddev_MK + Entropy_MK + Mean_MK + Min_MK + 
                # Median_MD + Skewness_MD + Kurtosis_MD + Stddev_MD + Entropy_MD + Mean_MD + Min_MD, 
              family=binomial(link = "logit"), dki_trn)
trn_dki_pred_lr <- predict(fit_lr, newdata = dki_trn, type=c("response"))
trn_dki_roc_lr  <- roc_suite(dki_trn$Gold, trn_dki_pred_lr)

tst_dki_pred_lr <- predict(fit_lr, newdata = dki_tst, type=c("response"))
tst_dki_roc_lr  <- roc_suite(dki_tst$Gold, tst_dki_pred_lr)

# combined params

### svm
# SUV params
fit_svm <- svm(Gold ~ TBR_SUVmax + TBR_SUVpeak + 
                Median_SUV + Skewness_SUV + Kurtosis_SUV + Stddev_SUV + Entropy_SUV + Mean_SUV + Min_SUV, 
              kernel = 'linear', probability = TRUE, cost = 4.0, dki_trn)
trn_suv_pred_svm <- predict(fit_svm, newdata = dki_trn, type=c("response"), probability = TRUE)
trn_suv_roc_svm  <- roc_suite(dki_trn$Gold, attr(trn_suv_pred_svm, 'probabilities')[,2])

tst_suv_pred_svm <- predict(fit_svm, newdata = dki_tst, type=c("response"), probability = TRUE)
tst_suv_roc_svm  <- roc_suite(dki_tst$Gold, attr(tst_suv_pred_svm, 'probabilities')[,2])

# DKI params
fit_svm <- svm(Gold ~ TBR_MK + TBR_MD + 
              Median_MK + Skewness_MK + Kurtosis_MK + Stddev_MK + Entropy_MK + Mean_MK + Min_MK + 
              Median_MD + Skewness_MD + Kurtosis_MD + Stddev_MD + Entropy_MD + Mean_MD + Min_MD, 
              kernel = 'linear', probability = TRUE, cost = 0.1, dki_trn)
trn_dki_pred_svm <- predict(fit_svm, newdata = dki_trn, type=c("response"), probability = TRUE)
trn_dki_roc_svm  <- roc_suite(dki_trn$Gold, attr(trn_dki_pred_svm, 'probabilities')[,2])

tst_dki_pred_svm <- predict(fit_svm, newdata = dki_tst, type=c("response"), probability = TRUE)
tst_dki_roc_svm  <- roc_suite(dki_tst$Gold, attr(tst_dki_pred_svm, 'probabilities')[,2])

# combined params

### dt(decision tree)
# SUV params
fit_dt <- rpart(Gold ~ TBR_SUVmax + TBR_SUVpeak + 
                Median_SUV + Skewness_SUV + Kurtosis_SUV + Stddev_SUV + Entropy_SUV + Mean_SUV + Min_SUV, 
                data = dki_trn, method = "class", control = rpart.control(minsplit = 10, cp=0.01))

trn_suv_pred_dt <- predict(fit_dt, dki_trn)[, 2]
trn_suv_roc_dt  <- roc_suite(dki_trn$Gold, trn_suv_pred_dt)

tst_suv_pred_dt <- predict(fit_dt, dki_tst)[, 2]
tst_suv_roc_dt  <- roc_suite(dki_tst$Gold, tst_suv_pred_dt)

# DKI params
fit_dt <- rpart(Gold ~ TBR_MK + TBR_MD + 
              Median_MK + Skewness_MK + Kurtosis_MK + Stddev_MK + Entropy_MK + Mean_MK + Min_MK + 
              Median_MD + Skewness_MD + Kurtosis_MD + Stddev_MD + Entropy_MD + Mean_MD + Min_MD, 
              data = dki_trn, method = "class", control = rpart.control(minsplit = 10, cp=0.01))
trn_dki_pred_dt <- predict(fit_dt, dki_trn)[, 2]
trn_dki_roc_dt  <- roc_suite(dki_trn$Gold, trn_dki_pred_dt)

tst_dki_pred_dt <- predict(fit_dt, dki_tst)[, 2]
tst_dki_roc_dt  <- roc_suite(dki_tst$Gold, tst_dki_pred_dt)

# combined params

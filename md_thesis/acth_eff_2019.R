# require(xlsx)
require(readxl)
require(ROCR)
library(pROC)
library(car)
library(psych)

# boxplot with ggplot2
library(reshape2)
library(ggplot2)
library(nortest)
library(dplyr)
library(rsq)

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

gene_raw <- read_excel("/Users/wujianmin/Documents/personal/lp/acth0729/20190730.xlsx", 
                    sheet = "genetic group")
qual_raw <- read_excel("/Users/wujianmin/Documents/personal/lp/acth0729/20190730.xlsx", 
                    sheet = "qualitative analysis")
quan_raw <- read_excel("/Users/wujianmin/Documents/personal/lp/acth0729/20190730.xlsx", 
                    sheet = "quantitative analysis")
unk_raw <- read_excel("/Users/wujianmin/Documents/personal/lp/acth0729/20190730.xlsx", 
                    sheet = "unknown etiology")

gene_cln <- gene_raw[1:18, c("Gender", "MonAge", "therap", "EEG")]
qual_cln <- qual_raw[, c("Gender", "MonAge", "therap","EEG")]
quan_cln <- quan_raw[, c("Gender", "MonAge", "therap","EEG")]
unk_cln  <- unk_raw[, c("Gender", "MonAge", "therap","EEG")]

gene_cln$etiology <- 'G'
qual_cln$etiology <- 'Q'
quan_cln$etiology <- 'Q'
unk_cln$etiology  <- 'U'

combin_cln <- rbind(gene_cln, qual_cln, quan_cln, unk_cln)
combin_cln$EEG <- as.factor(combin_cln$EEG)
combin_cln$therap <- as.factor(combin_cln$therap)
combin_cln$etiology <- as.factor(combin_cln$etiology)

summary(combin_cln)


combin_cln %>% 
  group_by(etiology) %>%  
  summarise(
    count_exa = n(), 
    mean_ma = mean(MonAge, na.rm = TRUE),
    sd_ma = sd(MonAge, na.rm = TRUE),
    n_male = sum(Gender=='男'),
    n_female = sum(Gender=='女'),
    n_eeg0 = sum(EEG==0),
    n_eeg1 = sum(EEG==1)
  )

chisq.test(combin_cln$Gender, y = combin_cln$etiology)
one_way <- aov(MonAge ~ etiology, data = combin_cln)
summary(one_way)
chisq.test(combin_cln$EEG, y = combin_cln$etiology)


combin_cln %>% 
  group_by(therap) %>%  
  summarise(
    count_exa = n(), 
    mean_ma = mean(MonAge, na.rm = TRUE),
    sd_ma = sd(MonAge, na.rm = TRUE),
    n_male = sum(Gender == '男'),
    n_female = sum(Gender == '女'),
    n_eeg0 = sum(EEG == 0),
    n_eeg1 = sum(EEG == 1),
    n_eti_g = sum(etiology == 'G'),
    n_eti_q = sum(etiology == 'Q'),
    n_eti_u = sum(etiology == 'U')
  )
chisq.test(combin_cln$Gender, y = combin_cln$therap)
t.test(MonAge ~ therap, data = combin_cln)
chisq.test(combin_cln$EEG, y = combin_cln$therap)
chisq.test(combin_cln$etiology, y = combin_cln$therap)

#### now we do regression
fit <- glm(therap ~ EEG + etiology + Gender + MonAge, 
           family=binomial(link = "logit"), combin_cln)
fit0 <- glm(therap ~ 1, 
            family=binomial(link = "logit"), combin_cln)

summary(fit)
rsq(fit, adj = FALSE, data = combin_cln)
anova(fit, fit0, test="F")

fit.step <- step(fit)
rsq(fit.step, adj = FALSE, data = combin_cln)
anova(fit.step, fit0, test="F")

summary(fit.step)

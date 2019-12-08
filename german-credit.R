## german-credit
# load data
data <- read.csv("~/Code/datasets/GermanCredit.csv")
# cleaning
data$property <- as.factor(data$property)
data$age <- as.numeric(data$age)
data$amount <- as.double(data$amount)
# transform
data$amount <- as.factor(ifelse(data$amount <= 2500, '0-2500', 
                                    ifelse(data$amount <= 5000, '2500-5000', "5000+")))
# train & test split
d <- sort(sample(nrow(data), .6*nrow(data)))
train <- data[d, ]
test  <- data[-d, ]
# train <- subset(train, select = -default)

# train general linear model 
m <- glm(credit_risk ~ . , data = train, family = binomial())

# evaluate
library(ROCR)
test$score <- predict(m, type="response", test)
pred <- prediction(test$score, test$credit_risk)
perf <- performance(pred, 'tpr', 'fpr')
plot(perf)
ks   <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])

# check for top-3 features
g <-  predict(m, type="terms", test)
ftopk <- function(x, top=3) {
  res <- names(x)[order(x, decreasing = TRUE)][1:top]
  paste(res, collapse = ";", sep = "")
}

topk <- apply(g, 1, ftopk, top=3)
test <-cbind(test, topk)
View(test)

# try some cutting edge techniques then
library(rpart)
fit1 <- rpart(credit_risk ~ ., data=train)
plot(fit1)
text(fit1)

test  <- data[-d, ]
#score test data
test$tscore1 <- predict(fit1, test, type='prob')
pred5 <- prediction(test$tscore1[,2],test$credit_risk)
perf5 <- performance(pred5,"tpr","fpr")

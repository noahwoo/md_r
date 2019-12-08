# load data
library(e1071)

read_fmt <- function(file, ncol) { 
   raw <- read.csv(file)
   summary(raw)
   to <- 3+ncol-1
   dat <- t(raw[, c(3:to)])
   colnames(dat) <- raw$Metrics.SUV.bw.[1:5]
   dat
}

plot_dat <- function(dat, name, npoly) {
   jpeg(name)
   par(mfrow=c(2, 2))
   plot(dat[, 1], dat[, 2], lty = 2, pch = 1, type='o', 
        xlab = "Time", ylab=colnames(dat)[2])
   c <- dat[, 1]
   y <- dat[, 2]
   mod <- lm(y ~ poly(c, npoly))
   # print(mod$coefficients)
   pred <- predict(mod, data.frame(x = c), 
                   interval = 'confidence', level = 0.99)
   lines(c, pred[, 1], col='red', lwd=3)
   
   ## 
   plot(dat[, 1], dat[, 3], lty = 2, pch = 2, type='o', 
     xlab = "Time", ylab=colnames(dat)[3])
   c <- dat[, 1]
   y <- dat[, 3]
   mod <- lm(y ~ poly(c, npoly))
   print(mod$coefficients)
   pred <- predict(mod, data.frame(x = c), 
                   interval = 'confidence', level = 0.99)
   lines(c, pred[, 1], col='red', lwd=3)
   
   ## 
   plot(dat[, 1], dat[, 4], lty = 2, pch = 3, type='o', 
     xlab = "Time", ylab=colnames(dat)[4])
   c <- dat[, 1]
   y <- dat[, 4]
   mod <- lm(y ~ poly(c, npoly))
   pred <- predict(mod, data.frame(x = c), 
                   interval = 'confidence', level = 0.99)
   lines(c, pred[, 1], col='red', lwd=3)
   
   ## 
   plot(dat[, 1], dat[, 5], lty = 2, pch = 4, type='o', 
     xlab = "Time", ylab=colnames(dat)[5])
   c <- dat[, 1]
   y <- dat[, 5]
   mod <- lm(y ~ poly(c, npoly))
   pred <- predict(mod, data.frame(x = c), 
                   interval = 'confidence', level = 0.99)
   lines(c, pred[, 1], col='red', lwd=3)
   
   dev.off()
}

fea_gen <- function(data) {
  tm <- diff(ts(data[, 1]))
  min <- diff(ts(data[, 2]))
  fea <- c(min(min/tm), max(min/tm))
  kmin <- kurtosis(data[, 2])
  
  max <- diff(ts(data[, 3]))
  fea <- c(min(max/tm), max(max/tm))
  kmax <- kurtosis(data[, 3])
  
  avg <- diff(ts(data[, 4]))
  fea <- c(min(avg/tm), max(avg/tm))
  kavg <- kurtosis(data[, 4])
  
  suv <- diff(ts(data[, 5]))
  fea <- c(min(suv/tm), max(suv/tm))
  ksuv <- kurtosis(data[, 5])
  c(c(kmin, kmax, kavg, ksuv), c(mean(data[, 2]), mean(data[, 3]), 
                                 mean(data[, 4]), mean(data[, 5])))
}

root <- "/Users/wujianmin/Documents/personal/lp/xzrcfh/TrueD"

dat1 <- read_fmt(paste(root, "/wang Rzhenjiao.csv", sep=""), 22)
dat2 <- read_fmt(paste(root, "/wang zhenjiao.csv", sep = ""), 24)
dat3 <- read_fmt(paste(root, "/wang zhenjiao40-60.csv", sep = ""), 21)
dat <- rbind(dat1, dat2, dat3)
colnames(dat)
plot_dat(dat, "./zhenjiao_wcy.jpg", 10)


fea_zj_wcy <- fea_gen(dat)
par(mfrow=c(2, 2))

dat1 <- read_fmt(paste(root, "/wang Rey.csv", sep=""), 22)
dat2 <- read_fmt(paste(root, "/wang eye.csv", sep = ""), 24)
dat3 <- read_fmt(paste(root, "/wang 40-60 eye.csv", sep = ""), 21)
dat <- rbind(dat1, dat2, dat3)
colnames(dat)
plot_dat(dat, "./eye_wcy.jpg", 7)
fea_eye_wcy <- fea_gen(dat)

dat1 <- read_fmt(paste(root, "/zhangli 0-24 eye 2.csv", sep=""), 23)
dat2 <- read_fmt(paste(root, "/zhangli 20-40 3.csv", sep = ""), 24)
dat3 <- read_fmt(paste(root, "/zhangli 40-60 2.csv", sep = ""), 21)
dat <- rbind(dat1, dat2, dat3)
colnames(dat)
plot_dat(dat, "./eye2_zl.jpg", 7)
fea_eye2_zl <- fea_gen(dat)

dat1 <- read_fmt(paste(root, "/zhangli 0-24 eye 3.csv", sep=""), 23)
dat2 <- read_fmt(paste(root, "/zhangli 20-40 4.csv", sep = ""), 24)
dat3 <- read_fmt(paste(root, "/zhangli 40-60 3.csv", sep = ""), 21)
dat <- rbind(dat1, dat2, dat3)
colnames(dat)
plot_dat(dat, "./eye3_zl.jpg", 7)
fea_eye3_zl <- fea_gen(dat)

par(mfrow=c(1, 1))
xfea <- cbind(fea_zj_wcy[1:4], fea_eye_wcy[1:4], fea_eye2_zl[1:4], fea_eye3_zl[1:4])
yfea <- cbind(fea_zj_wcy[5:8], fea_eye_wcy[5:8], fea_eye2_zl[5:8], fea_eye3_zl[5:8])

plot(xfea, yfea, pch = c(rep('w', 4), rep('w', 4), rep('z', 4), rep('z', 4)), 
     xlab = "kurtosis", ylab = "mean")


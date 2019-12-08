require(readxl)
require(ROCR)
library(pROC)
library(car)
library(psych)
require(corrplot)
# library(aplpack)


volt1_34 <- read_excel("/Users/wujianmin/Documents/personal/lp/ejnm-2019/MR定量测量20191103.xlsx", 
                     sheet = "VolT1_34")
volt1_57 <- read_excel("/Users/wujianmin/Documents/personal/lp/ejnm-2019/MR定量测量20191103.xlsx", 
                     sheet = "VolT1_57")

# x: Volume, y: SUV
dat1_x <- volt1_34[ 1:34, 2+3*seq(45)]
dat1_y <- volt1_34[38:71, 2+3*seq(45)]

dat2_y <- volt1_57[  1:57, 1+3*seq(45)]
dat2_x <- volt1_57[62:118, 1+3*seq(45)]

dat_x <- rbind(dat1_x, dat2_x)
dat_y <- rbind(dat1_y, dat2_y)

x_mean <- apply(dat_x, 2, mean)
x_sd <- apply(dat_x, 2, sd)

y_mean <- apply(dat_y, 2, mean)
y_sd <- apply(dat_y, 2, sd)

vec_x <- unlist(dat_x)
vec_y <- unlist(dat_y)

par(mfrow=c(2,2))
plot(pmin(vec_x, 1), pmin(vec_y, 1), type = 'p', pch = '.', 
     xlab="Volume", ylab="SUVmean",
     main="Volume & SUVmean in each ROI(cap to 1)"
     )
plot(vec_x, vec_y, type = 'p', pch = '.', 
     xlab="Volume", ylab="SUVmean",
     main="Volume & SUVmean in each ROI"
     )

x <- 1:length(x_mean)
plot(x, x_mean,
     ylim=range(c(x_mean-x_sd, x_mean+x_sd)),
     pch=19, xlab="ROI(1-45)", ylab="Volume",
     main="Scatter plot with std.dev of Volume"
)
arrows(x, x_mean-x_sd, x, x_mean+x_sd, length=0.05, angle=90, code=3)

plot(x, y_mean,
     ylim=range(c(y_mean-y_sd, y_mean+y_sd)),
     pch=19, xlab="ROI(1-45)", ylab="SUVmean",
     main="Scatter plot with std.dev of SUVmean"
)
arrows(x, y_mean-y_sd, x, y_mean+y_sd, length=0.05, angle=90, code=3)


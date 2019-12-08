a <- read.table("~/lost.txt")
b <- read.table("~/reten.txt")


age1 <- c(68,
        53,
        82,
        65,
        53,
        58,
        61,
        52,
        67,
        78,
        64,
        62,
        57,
        54,
        61,
        47)

age2 <- c(53,
          48,
          64,
          71,
          52,
          51,
          74,
          66)

t.test(age1, age2)

t1 <- c(4,
        4,
        3,
        3,
        2,
        4,
        3,
        4,
        2,
        4,
        4,
        4,
        3,
        3,
        4,
        4)
t2 <- c(4,
        2,
        3,
        2,
        3,
        4,
        4,
        4)

t.test(t1, t2)

# V1 低俗
# V2 点击
a$V1[a$V1>0] <- 1
a$V2[a$V2>0] <- 1
b$V1[b$V1>0] <- 1
b$V2[b$V2>0] <- 1

ca <- a$V1 + 2*a$V2
cb <- b$V1 + 2*b$V2

ca <- as.factor(ca)
cb <- as.factor(cb)

sa <- summary(ca)
sb <- summary(cb)

sa <- sa/sum(sa) # lost
sb <- sb/sum(sb) # retention
names(sa) <- c("nosex-noclk","sex-noclk","nosex-clk","sex-clk")

mat <- rbind(lost=sa, retention=sb)
mat

# t-test of retention
# chisq
a <- rbind(c(0.5, 0.3, 0.8, 0.9), c(0.4, 0.2, 0.6, 0.4))
chisq.test(a)

b <- 100*a
chisq.test(b)

c <- 1000*a
chisq.test(c)

sum <- 0
for(k in seq(1:510)) {
  sum = sum + 30.957*k^(-0.392)
  print(30.957*k^(-0.392))
}
sum/100

30.957*730^(-0.392)/100
6.7
sum <- 0
for(k in seq(1:730)) {
  sum = sum + 26.546*k^(-0.385)
}
sum/1006/1.6


30.957*517^(-0.392)/100

# V1
sum <- 0
for(k in seq(1:730)) {
  sum = sum + 61.36*k^(-0.609)
}
sum/100

# V2
sum <- 0
for(k in seq(1:730)) {
  sum = sum + 60.21*k^(-0.592)
}
sum/100

# V2.5
sum <- 0
for(k in seq(1:730)) {
  sum = sum + 56.28*k^(-0.564)
}
sum/100

# V3
sum <- 0
for(k in seq(1:730)) {
  sum = sum + 38.1*k^(-0.474)
}
sum/100


26.609*730^(-0.429)/100

sum <- 0
for(k in seq(1:60)) {
  a = 27.315*k^(-0.392)
  sum = sum + a
  print(a)
}
sum/100

26.609*517^(-0.429)/100

24.4*0.18


s <- c(0, 9)
v <- c(0, 14)
t.test(s,v)
       

sum <- 0
for(k in seq(1:24)) {
  a = 0.3583*k^(-0.499)
  sum = sum + a
  print(a)
}
sum


sum <- 0
for(k in seq(1:24)) {
  a = 0.3429*k^(-0.448)
  sum = sum + a
  print(a)
}
sum


### beta dist. with alpha and beta less than 1 ###
set.seed(1212);  m = 20000;  n = 162;  prop.ahead = numeric(m)
for (i in 1:m)
{
  x = sample(c(-1,1), n, repl=T);  cum = cumsum(x)
  ahead = (c(0, cum) + c(cum,0))[1:n]  # Adjustment for ties
  prop.ahead[i] = mean(ahead >= 0)
}

cut=seq(0, 1, by=.1); hdr="Proportion of 162-Game Season when Team Leads"  
hist(prop.ahead, breaks=cut, prob=T, col="skyblue2", xlab="Proportion", main=hdr)
curve(dbeta(x, .5, .5), add=T, col="blue", lwd=2)


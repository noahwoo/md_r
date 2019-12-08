library(ROCR)
library(woe)
########################
# connect to h2o service
libary(h2o)
h2o.init(ip="10.103.234.31", 
         port=8800, nthread=-1, max_mem_size = "8G")

# load from remote
arrFile <- system.file("/home/wujianmin/data", "arrhythmia.csv.gz", package="h2o")
arrFile <- "/home/wujianmin/data/arrhythmia.csv.gz"
arr.hex <- h2o.importFile(path = arrFile, destination_frame = "arr.hex")
summary(arr.hex)

# split data frame
arr.split <- h2o.splitFrame(arr.hex, ratios = 0.75)
arr.train.hex <- arr.split[[1]]
arr.test.hex <- arr.split[[2]]
summary(arr.hex$C1)

# train glm model
arr.glm <- h2o.glm(y="C1", 
                   x=c("C2","C3","C4","C5","C6","C7","C8","C9","C10"), 
                   training_frame = arr.train.hex,
                   family="gaussian", nfolds=10, alpha=0.5)
summary(arr.glm)
arr.pred <- h2o.predict(object=arr.glm, newdata = arr.test.hex)
glm.mse <- mean(sqrt((arr.pred$predict - arr.test.hex$C1)^2))
print(glm.mse)

# train gbm model
arr.gbm <- h2o.gbm(y="C1", 
                   x=c("C2","C3","C4","C5","C6","C7","C8","C9","C10"), 
                   training_frame = arr.train.hex,
                   distribution="gaussian", ntrees=50, max_depth = 5, min_rows = 10, nfolds = 10)
summary(arr.gbm)
arr.pred <- h2o.predict(object=arr.gbm, newdata = arr.test.hex)
gbm.mse <- mean(sqrt((arr.pred$predict - arr.test.hex$C1)^2))
print(gbm.mse)

# merge data frame
left <- data.frame(fruit = c('apple','orange','banana','lemon','strawberry','blueberry'), color = c('red','orange','yellow','yellow','red','blue'))
right <- data.frame(fruit = c('apple','orange','banana','lemon','strawberry','watermelon'), citrus = c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))
l.hex <- as.h2o(left)
print(l.hex)
r.hex <- as.h2o(right)
print(r.hex)
left.hex <- h2o.merge(l.hex, r.hex, all.x = TRUE)
print(left.hex)

# merge with instance id for dense and sparse file format
dnsfile <- "/home/wujianmin/data/test.dns.txt"
spsfile <- "/home/wujianmin/data/test.sps2.txt"

dns.hex <- h2o.importFile(path = dnsfile, destination_frame = "dns.hex", header = TRUE, sep=",")
sps.raw <- h2o.importFile(path = spsfile,  parse = FALSE)
sps.hex <- h2o.parseRaw(sps.raw, destination_frame = "sps.hex", parse_type = "SVMLight")

print(dns.hex)
print(sps.hex)

merge.hex <- h2o.merge(dns.hex, sps.hex, all.x = TRUE, by.x = "id", by.y = "C1")
print(merge.hex)
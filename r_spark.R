# R spark sparklyr dplyr sparkling h2o
# install sparklyr & spark
install.packages("sparklyr")
library(sparklyr)
spark_install(version = "2.1.0")
# latest version
devtools::install_github("rstudio/sparklyr")

# conn to spark
sc <- spark_connect(master = "local", version = "2.1.0")
# install demo data package
install.packages(c("nycflights13", "Lahman"))

# cp transit data to spark
library(dplyr)
iris_tbl <- copy_to(sc, iris)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
batting_tbl <- copy_to(sc, Lahman::Batting, "batting")
src_tbls(sc)

# a filtering sample
flights_tbl %>% filter(dep_delay == 2)

# common data manipulation pipeline: group-by -> summary -> filter -> return
delay <- flights_tbl %>% 
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  collect

# plot what you get
library(ggplot2)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

# window function demo
batting_tbl %>%
  select(playerID, yearID, teamID, G, AB:H) %>%
  arrange(playerID, yearID, teamID) %>%
  group_by(playerID) %>%
  filter(min_rank(desc(H)) <= 2 & H > 0)

# spark MLlib
mtcars_tbl <- copy_to(sc, mtcars, "mtcars")
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

# linreg
fit <- mtcars_tbl %>%
  ml_linear_regression(response = "mpg", features = c("wt", "cyl"), alpha = 0, lambda = 0)
summary(fit)

# try h2o sparkling water
install.packages("rsparkling")

options(rsparkling.sparklingwater.version = "2.1.0")
library(rsparkling)
library(h2o)

mtcars_h2o <- as_h2o_frame(sc, mtcars_tbl, strict_version_check = FALSE)
h2o_context(sc)
h2o.ls()
h2o.clusterStatus()

y <- "mpg"
x <- setdiff(names(mtcars_h2o), y)
splits <- h2o.splitFrame(mtcars_h2o, ratios = 0.7, seed = 1)

mtcars_glm <- h2o.glm(x = x,
                      y = y, family = "gaussian", link = "family_default", 
                      training_frame = splits[[1]],
                      lambda_search = FALSE, alpha = 0, lambda = 0, seed = 1)
summary(mtcars_glm)
perf <- h2o.performance(mtcars_glm, splits[[2]])
print(perf)

# disconnect
spark_disconnect(sc)
h2o.shutdown()

# connect to remove spark
# test locally first
livy_install(spark_version = "2.1.0")
livy_service_start()
sc <- spark_connect(master = "http://localhost:8998", method = "livy")
copy_to(sc, iris)
spark_disconnect(sc)
livy_service_stop()

# try remote 
config <- livy_config_auth("<username>", "<password>")
sc <- spark_connect(master = "<address>", method = "livy", config = config)
spark_disconnect(sc)

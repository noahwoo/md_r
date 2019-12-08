# dplyr & tidyr
library(dplyr)
library(tidyr)

# summary
summarise(iris, avg=mean(Sepal.Length))
summarise_each(iris, funs(mean))
count(iris, Species)
count(iris, Species, wt=Sepal.Length)

# group-by
iris %>% group_by(Species) %>% summarise(avg=mean(Sepal.Length))

# mutate
mutate(iris, sepal = Sepal.Length + Sepal.Width)
mutate_each(iris, funs(min_rank))
transmute(iris, sepal = Sepal.Length + Sepal.Width) %>% mutate_all(funs(min_rank))
transmute(iris, sepal = Sepal.Length + Sepal.Width) %>% mutate_all(funs(lead))
transmute(iris, sepal = Sepal.Length + Sepal.Width) %>% mutate_all(funs(percent_rank))
transmute(iris, sepal = Sepal.Length + Sepal.Width) %>% mutate_all(funs(cume_dist))

# join
a <- data_frame(x1=c("A", "B", "C"), x2=c(1,2,3))
b <- data_frame(x1=c("A", "B", "D"), x3=c(TRUE, FALSE, TRUE))
left_join(a, b, by="x1")
right_join(a, b, by = "x1")
inner_join(a, b, by = "x1")
full_join(a, b, by = "x1")
semi_join(a, b, by = "x1")
anti_join(a, b, by = "x1")

# set op
y <- data_frame(x1=c("A", "B", "C"), x2=c(1,2,3))
z <- data_frame(x1=c("B", "C", "D"), x2=c(2,3,4))

intersect(y, z)
union(y, z)
setdiff(y, z)
w <- bind_cols(y, z) %>% select(x1, x21)
bind_rows(y, z)
bind_rows(w, z)

# more formal plyr

one <- ozone[1, 1, ]

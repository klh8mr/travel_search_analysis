library(anytime)
library(lubridate)
library(magrittr)
library(dplyr)

setwd("~/UVaMSDS/MachineLearning/FinalProject")
df <- read.csv("city_search_sparse.csv")

## Explore Data
###############################################

# Unique users - 5777 
length(unique(df$user_id)) 
# Number of users visitng x times
table(table(df$user_id))

# Get the user_id's of those who visited x number of times
x <- 1
table(df$user_id)[(table(df$user_id)==x)] %>%
  rownames()

# missing country
unique(df$country)
df$country[7]==''
countryNA <- df[df$country=='',]


## Datetime
###############################################

# Convert session and joining date to lubridate format
df$session_date <- ymd(anydate(df$unix_timestamp))
df$joining_date <- ymd(df$joining_date)

# Calculate days elapsed between join and session date
df$daysSinceJoin <- (df$session_date - df$joining_date) %>%
  as.character() %>%
  as.numeric()

# How old are most of the accounts?
table(df$daysSinceJoin) %>%
  sort()

hist(df$daysSinceJoin, breaks=150)

df$session_wday <- wday(df$session_date)
table(df$session_wday)


## Cities
###############################################
# no repeat city names with mispellings
sort(names(df[,8:ncol(df)]))

# list of most frequently searched cities - lines up with market basket analysis
city_counts <- sapply(df[,8:ncol(df)], function(x) sum(x))
sort(city_counts, decreasing=TRUE)


## PCA
###############################################
## Perform PCA
pr.out = prcomp(df[,-5], scale = TRUE)
names(pr.out)

## means and standard deviations used for scaling prior to PCA
pr.out$center
pr.out$scale
pr.out$x

plot(pr.out$x[,"PC1"], pr.out$x[,"PC2"])
df$pc1 <- pr.out$x[,"PC1"]
df$pc2 <- pr.out$x[,"PC2"]
plot(df$pc1, df$pc2)

library(ggplot2)
ggplot(df, aes(pc1, pc2)) +
  geom_point(aes(colour=Species))


## Cluster Users
###############################################
clusters <- kmeans(df_users[,-c(1:3)], 2)
df_users$label <- clusters$cluster

ggplot(df_users, aes(x=avgTimeElapsed, y=n_visits, color=label)) +
  geom_point()


## Recomender Systems
###############################################

#https://www.r-bloggers.com/recommender-systems-101-a-step-by-step-practical-example-in-r/
#http://blog.yhat.com/posts/recommender-system-in-r.html


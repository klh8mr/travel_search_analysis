library(anytime)
library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)

setwd("~/UVaMSDS/MachineLearning/FinalProject")
df <- read.csv("city_search_sparse.csv")
df_users <- read.csv("df_users.csv")

# Convert session and joining date to lubridate format
df$session_date <- ymd(anydate(df$unix_timestamp))
df$joining_date <- ymd(df$joining_date)

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
hist(df$session_wday)

df$session_wknd <- 0
df$session_wknd[df$session_wday==1 | df$session_wday==7] <- 1


## Cities
###############################################
# no repeat city names with mispellings
sort(names(df[,8:ncol(df)]))

# list of most frequently searched cities - lines up with market basket analysis
city_counts <- sapply(df[,8:ncol(df)], function(x) sum(x))
sort(city_counts, decreasing=TRUE)


## PCA
###############################################
##### Perform PCA on USERS #####
pr.out = prcomp(df_users[,8:96], scale = TRUE)
names(pr.out)

## means and standard deviations used for scaling prior to PCA
pr.out$center
pr.out$scale
pr.out$x

plot(pr.out$x[,"PC1"], pr.out$x[,"PC2"])
df_users$pc1 <- pr.out$x[,"PC1"]
df_users$pc2 <- pr.out$x[,"PC2"]
df_users$pc3 <- pr.out$x[,"PC3"]
df_users$pc4 <- pr.out$x[,"PC4"]
df_users$pc5 <- pr.out$x[,"PC5"]
plot(df_users$pc1, df_users$pc2)

ggplot(df_users, aes(pc1, pc2)) +
  geom_point(aes(colour=country))

plot_ly(df_users, x = ~pc1, y = ~pc2, z = ~pc3,
        type = "scatter3d", color = ~country)

## PC loadings
pr.out$rotation[,1:5]
## Dimensions of PC Scores
dim(pr.out$x)
## Biplot to look at scores and loadings:
biplot(pr.out,scale=0)


##### Perform PCA on TXNS #####
pr.out = prcomp(df[,8:96], scale = TRUE)
names(pr.out)

## means and standard deviations used for scaling prior to PCA
pr.out$center
pr.out$scale
pr.out$x

plot(pr.out$x[,"PC1"], pr.out$x[,"PC2"])
df$pc1 <- pr.out$x[,"PC1"]
df$pc2 <- pr.out$x[,"PC2"]
df$pc3 <- pr.out$x[,"PC3"]
df$pc4 <- pr.out$x[,"PC4"]
df$pc5 <- pr.out$x[,"PC5"]
plot(df$pc1, df$pc2)

ggplot(df, aes(pc1, pc2)) +
  geom_point(aes(colour=session_date))

df$month <- month(df$session_date)

plot_ly(df, x = ~pc1, y = ~pc2, z = ~pc3,
        type = "scatter3d", color = ~month)

## PC loadings
pr.out$rotation[,1:5]
## Dimensions
dim(pr.out$x)
## Make biplot to look at scores and loadings:
biplot(pr.out,scale=0)


## Cluster Users
###############################################
clusters <- kmeans(df_users[,c(3, 5:7)], 2) # based on user characteristics
clusters <- kmeans(df_users[,c(8:96)], 2) # based on cities - sparse
clusters <- kmeans(df_users[,c(97:101)], 2) # based on cities - PCA
clusters <- kmeans(df_users[,c(5:7, 97:101)], 2) # based on user characteristics AND cities - PCA
df_users$label <- clusters$cluster

table(df_users$label, df_users$country)

### Explore what these labels might mean... play around with different axes to see if can identify pattern
ggplot(df_users, aes(x=avgTimeElapsed, y=pc1, color=label)) +
  geom_point()

plot_ly(df_users, x = ~avgTimeElapsed, y = ~pc1, z = ~n_visits,
        type = "scatter3d", color = ~label)


## Recomender Systems
###############################################

#https://www.r-bloggers.com/recommender-systems-101-a-step-by-step-practical-example-in-r/
#http://blog.yhat.com/posts/recommender-system-in-r.html


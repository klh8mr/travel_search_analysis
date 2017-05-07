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
df$session_date <- ymd(df$session_date)
df$joining_date <- ymd(df$joining_date)

## Explore Data
###############################################

# Unique users - 5777 
length(unique(df$user_id)) 
# Number of users visitng x times
t <- table(table(df$user_id))
plot(t, type = "h")

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
sort(names(df[,10:98]))

# list of most frequently searched cities - lines up with market basket analysis
city_counts <- sapply(df[,10:98], function(x) sum(x))
sort(city_counts, decreasing=TRUE)


## PCA
###############################################
##### Perform PCA on USERS #####
pr.out = prcomp(df_users[,9:97], scale = TRUE)
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
pr.out = prcomp(df[,10:98], scale = TRUE)
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
# add some user characteristics
clusters <- kmeans(df_users[,c(4:8)], 2) # based on user characteristics
clusters <- kmeans(df_users[,c(9:97)], 2) # based on cities - sparse
clusters <- kmeans(df_users[,c(98:102)], 2) # based on cities - PCA
clusters <- kmeans(df_users[,c(4:7,8, 98:102)], 2) # based on user characteristics AND cities - PCA
df_users$label <- clusters$cluster

table(df_users$label, df_users$country)

### Explore what these labels might mean... play around with different axes to see if can identify pattern
ggplot(df_users, aes(x=avgTimeElapsed, y=pc1, color=label)) +
  geom_point()

plot_ly(df_users, x = ~avgTimeElapsed, y = ~pc1, z = ~n_visits,
        type = "scatter3d", color = ~label)

# is min_distance the dividing factor? under 5000 are "serious" buyers
ggplot(df_users, aes(x=avgTimeElapsed, y=min_distance, color=label)) +
  geom_point()

plot_ly(df_users, x = ~avgTimeElapsed, y = ~min_distance, z = ~CitiesSearched_avg,
        type = "scatter3d", color = ~label)

# different way to visualize
library(cluster)
library(fpc)
dat <- df_users[,c(4:8)]
clus <- kmeans(dat, centers=2)
plotcluster(dat,clus$cluster)

with(df_users[,c(4:8)], pairs(dat, col=c(4:7)[clus$cluster]))

## Recomender Systems
###############################################

#https://www.r-bloggers.com/recommender-systems-101-a-step-by-step-practical-example-in-r/
#http://blog.yhat.com/posts/recommender-system-in-r.html


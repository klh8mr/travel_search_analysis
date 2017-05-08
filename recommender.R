library(vegan)
library(bnlearn)
library(magrittr)
library(dplyr)
<<<<<<< Updated upstream
=======
<<<<<<< HEAD

=======
>>>>>>> origin/master
>>>>>>> Stashed changes
########################
## Recommender System ##
########################

## By User/Day
#########################################
# create distance matrix using Jaccard
df <- read.csv("df_users.csv")
# get just the cities
df <- df[,c(9:97)]

# transpose matrix
df <- as.data.frame(t(as.matrix(df)))

# distance df - smaller is better
dist_df <- as.data.frame(as.matrix(designdist(df, "(A+B-2*J)/(A+B-J)")))

# write out to csv for function
write.csv(dist_df, "dist_df.csv", row.names = FALSE)

# read in for function
dist_df <<- read.csv("dist_df.csv")

## Bayesian Network
##########################################
df <- read.csv("city_search_sparse.csv")
# get just the cities
df <- df[,c(11:99)]

# change all column types
for (i in 1:ncol(df)){
  df[,i] <- as.factor(df[,i])
}

res <- hc(df)
plot(res)

# Create conditional tables for each node
fittedbn <- bn.fit(res, data = df)

# For example, let look at what is inside the NY node.
print(fittedbn$New.York.NY)


## Recommender System
##########################################
i <- 3 # Set the row number for the transaction of interest

### First return the top 5 closest cities to the first city the user searched
# subset the df
i_cities <- names(df)[df[i, 1:ncol(df)]=="1"]
i_distDF <- dist_df[rownames(dist_df) %in% i_cities, !(names(dist_df) %in% i_cities)]
i_top5 <- colSums(i_distDF) %>%
  sort() %>%
  head(5) %>%
  names()

### Then get top probs for each top 5 cities
probs <- data.frame(city=i_top5, similar_rank=(1:length(i_top5)), prob=0)

<<<<<<< HEAD
# query into BN to find top probability
=======
# to narrow down the events, find top 5 shortest distances from matrix above to first city
# Example: top 5 closest to NY:
# LA, Philly, etc

## Recommender system (without function)
##########################################
i <- 509 # Set the row number for the transaction of interest

i_cities <- names(df)[df[i, 1:ncol(df)]=="1"]
i_distDF <- dist_df[rownames(dist_df) %in% i_cities, !(names(dist_df) %in% i_cities)]
i_top5 <- colSums(i_distDF) %>%
  sort() %>%
  head(5) %>%
  names()

probs <- data.frame(city=i_top5, similar_rank=(1:length(i_top5)), prob=0)

<<<<<<< Updated upstream
=======
>>>>>>> origin/master
>>>>>>> Stashed changes
for (y_city in i_top5){
  str = paste("(", y_city, "==", 1, ")", sep = "")
  prob_city <- cpquery(fittedbn, eval(parse(text = str)), evidence=as.list(df[i, !(names(df) %in% y_city)]), method="lw")
  
  probs$prob[probs$city==y_city] <- prob_city
}

i_cities
arrange(probs, desc(prob))


<<<<<<< Updated upstream
=======
<<<<<<< HEAD
## Recommender System Function (still in the works - how to call conditional probs globally?)
##########################################
recommend <- function(x){
  ### First return the top 5 closest cities to the first city the user searched
  # subset the df
  i<-as.double(x)
  i_cities <- names(df)[df[i, 1:ncol(df)]=="1"]
  i_distDF <- dist_df[rownames(dist_df) %in% i_cities, !(names(dist_df) %in% i_cities)]
  i_top5 <- colSums(i_distDF) %>%
    sort() %>%
    head(5) %>%
    names()
  
  ### Then get top probs for each top 5 cities
  probs <- data.frame(city=i_top5, similar_rank=(1:length(i_top5)), prob=0)
  
  # query into BN to find top probability
  for (y_city in i_top5){
    str = paste("(", y_city, "==", 1, ")", sep = "")
    prob_city <- cpquery(fittedbn, eval(parse(text = str)), evidence=as.list(df[i, !(names(df) %in% y_city)]), method="lw")
    
    probs$prob[probs$city==y_city] <- prob_city
  }
  
  return(list(i_cities, arrange(probs, desc(prob))))
  
}
=======
>>>>>>> Stashed changes
## References
##########################################

# Collaborative filtering overview
#http://ijcai13.org/files/tutorial_slides/td3.pdf

# Simply BN classifier
#https://www.cs.rutgers.edu/~pazzani/Publications/koji.pdf
<<<<<<< Updated upstream
=======
>>>>>>> origin/master
>>>>>>> Stashed changes

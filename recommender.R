library(vegan)
library(bnlearn)
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

## Bayesian Network
##########################################
df <- read.csv("city_search_sparse.csv")
# get just the cities
df <- df[,c(11:99)]

# change all column types
for (i in 1:ncol(df)){
  df[,i] <- as.numeric(df[,i])
}
  
res <- hc(df)
plot(res)

# Create conditional tables for each node
fittedbn <- bn.fit(res, data = df)

#For example, let look at what is inside the NY node.
print(fittedbn$New.York.NY)

# Create a function that queries into it?

#Example: User searched NY, NJ:
# prob1 = cpquery(fittedbn, event = (LA == "1"), evidence = (NY == "1" & NJ =="1"))
# prob2 = cpquery(fittedbn, event = (Philly == "1"), evidence = (NY == "1" & NJ =="1"))
# prob2 is greater, so we recommend Philly

# to narrow down the events, find top 5 shortest distances from matrix above to first city
# Example: top 5 closest to NY:
# LA, Philly, etc

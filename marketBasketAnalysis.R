library(jsonlite)
library(plyr)
library(dplyr)
library(tidyr)
library(arules)


setwd("~/UVaMSDS/MachineLearning/FinalProject")

###################################################
## Data Set-up - to consolidate with other files ##
###################################################

# import data
cityData <- fromJSON(file("city_search.json"))

# unlist the "user" column into a temp df
dftemp<-matrix(unlist(cityData$user), ncol=3, byrow=TRUE)

# combine the two dfs and rename the columns 
cityData <- cbind(cityData, dftemp)
names(cityData)[names(cityData)=="1"] <- "user_id"
names(cityData)[names(cityData)=="2"] <- "joining_date"
names(cityData)[names(cityData)=="3"] <- "country"

# drop the old "user" column
cityData = subset(cityData, select = -c(user))

# temp df with city columns
dftemp <-cityData[c(3)]

# split the cities in the column into a list
citylist <- strsplit(as.character(cityData$cities), ",")

# add a column for the number of cities in the list
dftemp["num_of_cities"] <- NA

# populate the "num_of_cities" column
for (i in 1: length(citylist)){
  dftemp$num_of_cities[i] <- length(citylist[[i]])
}

# find the max number of cities searched in one session
max(dftemp[,2]) #11

# create new temp df to put the cities into
dftemp2 <- data.frame(matrix(ncol=11, nrow=length(citylist)))

# populate the df
for (j in 1:11){
  for (i in 1:length(citylist)){
    dftemp2[j][[1]][i] <- citylist[[i]][j]
  }
}

# rename the columns
names(dftemp2) <- c("city1", "city2", "city3", "city4", "city5", "city6", "city7", "city8", "city9", "city10", "city11")

# combine the dfs
dftemp <- cbind(dftemp, dftemp2)

# add to master cityData df
cityData <- cbind(cityData, dftemp[2:13])

# reorder the columns
cityData <- cityData[,c(1:3,7:18,4:6)]


#######################
## Association Rules ##
#######################

### Create transactions datasets
#######################################

# 1. All transactions
df <- cityData[ , c(5:15)]
names(df) <- NULL
write.csv(df, "data_all.csv", row.names=FALSE)


# 2. Transactions by country group
df <- cityData[ , c(5:15, 18)] # same as above but keep country column
table(df$country) # Sanity Check: n transactions for each country

## Local - US and Canada
df_sub <- df[df$country=="US" | df$country=="", -12]
names(df_sub) <- NULL
write.csv(df_sub, "data_local.csv", row.names=FALSE)

## Distance Travelers - Europe
df_sub <- df[df$country!="US" & df$country!="", -12]
names(df_sub) <- NULL
write.csv(df_sub, "data_dist.csv", row.names=FALSE)


# 3. Split US and Canada
df <- cityData[ , c(5:15, 18)] # same as above but keep country column

## US
df_sub <- df[df$country=="US", -12]
names(df_sub) <- NULL
write.csv(df_sub, "data_us.csv", row.names=FALSE)

## Canada
df_sub <- df[df$country=="", -12]
names(df_sub) <- NULL
write.csv(df_sub, "data_can.csv", row.names=FALSE)



### Generate Rules
#######################################

# 1. All transactions
txn_all <- read.transactions("data_all.csv", rm.duplicates=FALSE, format = "basket", sep = ",")

inspect(txn_all) # 20022
itemFrequencyPlot(txn_all, support=0.1) # frequency of item across all baskets
itemFrequencyPlot(txn_all, topN=10)

# Sparse matrix
image(sample(txn_all, 100))

# Generate rules:  Two options for generating rules below
rules_all <- apriori(data=txn_all, 
                 parameter=list(support=0.005, confidence=0.1, minlen=2)) 
summary(rules_all)
inspect(sort(rules_all, by="lift", decreasing=TRUE))


# 2. Transactions by country group

## Local - US and Canada
txn_local <- read.transactions("data_local.csv", rm.duplicates=FALSE, format = "basket", sep = ",")

inspect(txn_local) # 6696
itemFrequencyPlot(txn_local, support=0.05) # frequency of item across all baskets
itemFrequencyPlot(txn_local, topN=10)

rules_local <- apriori(data=txn_local, 
                     parameter=list(support=0.01, confidence=0.1, minlen=2))
summary(rules_local)
inspect(sort(rules_local, by="lift", decreasing=TRUE))

## Distance Travelers - Europe
txn_dist <- read.transactions("data_dist.csv", rm.duplicates=FALSE, format = "basket", sep = ",")

inspect(txn_dist) # 13326
itemFrequencyPlot(txn_dist, support=0.05) # frequency of item across all baskets
itemFrequencyPlot(txn_dist, topN=10)

rules_dist <- apriori(data=txn_dist, 
                       parameter=list(support=0.01, confidence=0.1, minlen=2))
summary(rules_dist)
inspect(sort(rules_dist, by="lift", decreasing=TRUE))

# 3. Split US and Canada

## US
txn_us <- read.transactions("data_us.csv", rm.duplicates=FALSE, format = "basket", sep = ",")
inspect(txn_us) # 3876

itemFrequencyPlot(txn_us, support=0.05) # frequency of item across all baskets
itemFrequencyPlot(txn_us, topN=10)

rules_us <- apriori(data=txn_us, 
                      parameter=list(support=0.01, confidence=0.1, minlen=2))
summary(rules_us)
inspect(sort(rules_us, by="lift", decreasing=TRUE))

## Canada
txn_can <- read.transactions("data_can.csv", rm.duplicates=FALSE, format = "basket", sep = ",")

inspect(txn_can) # 2820
itemFrequencyPlot(txn_can, support=0.05) # frequency of item across all baskets
itemFrequencyPlot(txn_can, topN=10)

rules_can <- apriori(data=txn_can, 
                    parameter=list(support=0.01, confidence=0.1, minlen=2))
summary(rules_can)
inspect(sort(rules_can, by="lift", decreasing=TRUE))


### Visualization
###############################################
library("arulesViz") 
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
# http://www.jmlr.org/papers/volume12/hahsler11a/hahsler11a.pdf
plot(rules_all)
plot(rules_local)
plot(rules_dist)
plot(rules_us)
plot(rules_can)

plot(rules_all,method="graph")#,interactive=TRUE)
plot(rules_local,method="graph",interactive=TRUE)
plot(rules_dist,method="graph",interactive=TRUE,shading=NA)
plot(rules_us,method="graph")#,interactive=TRUE,shading=NA)
plot(rules_can,method="graph",interactive=TRUE,shading=NA)

plot(rules,method="graph")
# Size = support
# Color = lift

plot(rules_all, method="paracoord", control=list(reorder=TRUE))
plot(rules_local, method="paracoord", control=list(reorder=TRUE))
plot(rules_dist, method="paracoord", control=list(reorder=TRUE))
plot(rules_us, method="paracoord", control=list(reorder=TRUE)) # Not sure why it doesn't work!
plot(rules_can, method="paracoord", control=list(reorder=TRUE))


inspectDT(rules) # Cool interactive feature





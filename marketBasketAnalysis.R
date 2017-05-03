library(jsonlite)
library(plyr)
library(dplyr)
library(tidyr)

setwd("~/UVaMSDS/MachineLearning/FinalProject")
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
library(arules)
df <- cityData[ , c(1, 5:15)]
names(df) <- NULL
write.csv(df[,-1], "data.csv", row.names=FALSE)
txn <- read.transactions("data.csv", rm.duplicates=FALSE, format = "basket", sep = ",")

inspect(txn) # 20022

# Explore data
inspect(txn[1:40])
itemFrequency(txn[,1:3])
itemFrequencyPlot(txn, support=0.1) # frequency of item across all baskets
itemFrequencyPlot(txn, confidence=0.8)
itemFrequencyPlot(txn, topN=10)

# Sparse matrix
image(sample(txn, 100))

# Generate some rules
rules <- apriori(data=txn, 
                   parameter=list(support=0.01, confidence=0.1, minlen=1)) 
summary(rules)
inspect(sort(rules, by="lift", decreasing=TRUE))


### Visualization
###############################################
library("arulesViz") # https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
plot(rules)

plot(rules,method="graph",interactive=TRUE,shading=NA)
plot(rules,method="graph")

plot(rules, method="paracoord", control=list(reorder=TRUE))

inspectDT(rules)


### By Country
###############################################

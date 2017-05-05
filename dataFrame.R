library(jsonlite)
library(plyr)
library(fossil)
library(anytime)
library(lubridate)

#########################################################
### This file cleans the df and creates some features ###
#########################################################

# set the working directory to where the data file is

##############################
## Working with JSON format ##
##############################
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

# first columns are still lists
cityData[,1:3] <- sapply(cityData[,1:3], function(x) unlist(x))

#################################
## Breaking Out List of Cities ## 
#################################
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

# write out to csv for market analysis
write.csv(cityData[,c(5:15,18)], "marketbasket.csv",row.names = FALSE)

#################################
## Breaking Out List of States ## 
#################################
# functions for getting the states
getStates <- function(df){
  # create placeholder df
  df2 <- data.frame(matrix(ncol=ncol(df), nrow=nrow(df)))
  # populate the df
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if(!is.na(df[[j]][i])){
        if(nchar(df[[j]][i]) < 3){
          df2[j][[1]][i] <- df[[j]][i] # [col][[1]][row]
        }
      }
    }
  }
  return(df2)
}

oneColumn <- function(df){
  # create placeholder df
  df2 <- data.frame(matrix(ncol=1, nrow=nrow(df)))
  # collapse to one col
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if(!is.na(df[j][[1]][i])){
        df2[1][[1]][i] <- df[j][[1]][i]
      }
    }
  }
  return(df2)
}

# temp df with city columns from master
dftemp <-cityData[c(5:15)]

# get states for each city
# city 1
dftemp2 <- as.data.frame(do.call('rbind', strsplit(as.character(dftemp$city1), " ", fixed = TRUE)), stringsAsFactors = FALSE)
is.na(dftemp2) <- dftemp2==''
# pull out states
df_city1 <- getStates(dftemp2)
# get one column
df_city1 <- oneColumn(df_city1)

# city 2
dftemp2 <- as.data.frame(do.call('rbind', strsplit(as.character(dftemp$city2), " ", fixed = TRUE)), stringsAsFactors = FALSE)
is.na(dftemp2) <- dftemp2==''
# pull out states
df_city2 <- getStates(dftemp2)
# get one column
df_city2 <- oneColumn(df_city2)

# city 3
dftemp2 <- as.data.frame(do.call('rbind', strsplit(as.character(dftemp$city3), " ", fixed = TRUE)), stringsAsFactors = FALSE)
is.na(dftemp2) <- dftemp2==''
# pull out states
df_city3 <- getStates(dftemp2)
# get one column
df_city3 <- oneColumn(df_city3)

# city 4
dftemp2 <- as.data.frame(do.call('rbind', strsplit(as.character(dftemp$city4), " ", fixed = TRUE)), stringsAsFactors = FALSE)
is.na(dftemp2) <- dftemp2==''
# pull out states
df_city4 <- getStates(dftemp2)
# get one column
df_city4 <- oneColumn(df_city4)

# city 5
dftemp2 <- as.data.frame(do.call('rbind', strsplit(as.character(dftemp$city5), " ", fixed = TRUE)), stringsAsFactors = FALSE)
is.na(dftemp2) <- dftemp2==''
# pull out states
df_city5 <- getStates(dftemp2)
# get one column
df_city5 <- oneColumn(df_city5)

# city 6
dftemp2 <- as.data.frame(do.call('rbind', strsplit(as.character(dftemp$city6), " ", fixed = TRUE)), stringsAsFactors = FALSE)
is.na(dftemp2) <- dftemp2==''
# pull out states
df_city6 <- getStates(dftemp2)
# get one column
df_city6 <- oneColumn(df_city6)

# city 7
dftemp2 <- as.data.frame(do.call('rbind', strsplit(as.character(dftemp$city7), " ", fixed = TRUE)), stringsAsFactors = FALSE)
is.na(dftemp2) <- dftemp2==''
# pull out states
df_city7 <- getStates(dftemp2)
# get one column
df_city7 <- oneColumn(df_city7)

# city 8
dftemp2 <- as.data.frame(do.call('rbind', strsplit(as.character(dftemp$city8), " ", fixed = TRUE)), stringsAsFactors = FALSE)
is.na(dftemp2) <- dftemp2==''
# pull out states
df_city8 <- getStates(dftemp2)
# get one column
df_city8 <- oneColumn(df_city8)

# city 9
dftemp2 <- as.data.frame(do.call('rbind', strsplit(as.character(dftemp$city9), " ", fixed = TRUE)), stringsAsFactors = FALSE)
is.na(dftemp2) <- dftemp2==''
# pull out states
df_city9 <- getStates(dftemp2)
# get one column
df_city9 <- oneColumn(df_city9)

# city 10
dftemp2 <- as.data.frame(do.call('rbind', strsplit(as.character(dftemp$city10), " ", fixed = TRUE)), stringsAsFactors = FALSE)
is.na(dftemp2) <- dftemp2==''
# pull out states
df_city10 <- getStates(dftemp2)
# get one column
df_city10 <- oneColumn(df_city10)

# city 11
dftemp2 <- as.data.frame(do.call('rbind', strsplit(as.character(dftemp$city11), " ", fixed = TRUE)), stringsAsFactors = FALSE)
is.na(dftemp2) <- dftemp2==''
# pull out states
df_city11 <- getStates(dftemp2)
# get one column
df_city11 <- oneColumn(df_city11)

# put the columns together
df_states <- cbind(df_city1, df_city2, df_city3, df_city4, df_city5,
                   df_city6, df_city7, df_city8, df_city9, df_city10, df_city11)

# rename the columns
names(df_states) <- c("state1", "state2", "state3", "state4", "state5", "state6", "state7", "state8", "state9", "state10", "state11")

# remove duplicated states
dups <- function(df_states) df_states[!duplicated(df_states)]
dftemp3 <- data.frame(as.list(t(t(apply(df_states, 1, function(x) dups(na.omit(x)))))))

# populate with count
dftemp4 <-data.frame(matrix(ncol=1, nrow=nrow(dftemp3)))
for (i in 1:nrow(dftemp3)){
  dftemp4[1][[1]][i] <-length(dftemp3[1][[1]][i][[1]])
}

df_states <- cbind(df_states, dftemp4)
colnames(df_states)[12] <- "num_of_states"

# write out to csv to do formatting in excel - 
# count number of countries
write.csv(df_states, "states.csv")

# read in new dftemp
dftemp <- read.csv("states_clean.csv")

# select columns
dftemp <-dftemp[,c(12,13)]

# combine with master
cityData <- cbind(cityData, dftemp)

# reorder the columns
cityData <- cityData[,c(1:4,19:20,5:18)]

############################################
## Get Lat and Long to Calculate Distance ## 
############################################
# temp df with all cities from master
dftemp <-cityData[c(7:17)]

# write out to csv to get lat and long from another excel sheet
write.csv(dftemp, "latandlong.csv")

# read in new dftemp
dftemp <- read.csv("latandlong_clean.csv")

# add city count
dftemp <- cbind(cityData$num_of_cities, dftemp)

dftemp["avg_distance"] <- NA

# populate avg distance
for (i in 1:nrow(dftemp)){
  # no average for one city
  if(dftemp[1][[1]][i]==1){
    dftemp[35][[1]][i] <- 0
  } else if(dftemp[1][[1]][i]==2){
    # calculate the average between 2 cities
    dist <- deg.dist(dftemp[14][[1]][i],dftemp[13][[1]][i],dftemp[16][[1]][i],dftemp[15][[1]][i])
    dftemp[35][[1]][i] <- dist
  } else if(dftemp[1][[1]][i]==3){
    # caluculate the average between 3 cities
    dist1 <- deg.dist(dftemp[14][[1]][i],dftemp[13][[1]][i],dftemp[16][[1]][i],dftemp[15][[1]][i])
    dist2 <- deg.dist(dftemp[16][[1]][i],dftemp[15][[1]][i],dftemp[18][[1]][i],dftemp[17][[1]][i])
    dftemp[35][[1]][i]<-((dist1+dist2)/2)
  } else if(dftemp[1][[1]][i]==4){
    # caluculate the average between 4 cities
    dist1 <- deg.dist(dftemp[14][[1]][i],dftemp[13][[1]][i],dftemp[16][[1]][i],dftemp[15][[1]][i])
    dist2 <- deg.dist(dftemp[16][[1]][i],dftemp[15][[1]][i],dftemp[18][[1]][i],dftemp[17][[1]][i])
    dist3 <- deg.dist(dftemp[18][[1]][i],dftemp[17][[1]][i],dftemp[20][[1]][i],dftemp[19][[1]][i])
    dftemp[35][[1]][i]<-((dist1+dist2+dist3)/3)
  } else if(dftemp[1][[1]][i]==5){
    # caluculate the average between 5 cities
    dist1 <- deg.dist(dftemp[14][[1]][i],dftemp[13][[1]][i],dftemp[16][[1]][i],dftemp[15][[1]][i])
    dist2 <- deg.dist(dftemp[16][[1]][i],dftemp[15][[1]][i],dftemp[18][[1]][i],dftemp[17][[1]][i])
    dist3 <- deg.dist(dftemp[18][[1]][i],dftemp[17][[1]][i],dftemp[20][[1]][i],dftemp[19][[1]][i])
    dist4 <- deg.dist(dftemp[20][[1]][i],dftemp[19][[1]][i],dftemp[22][[1]][i],dftemp[21][[1]][i])
    dftemp[35][[1]][i]<-((dist1+dist2+dist3+dist4)/4)
  } else if(dftemp[1][[1]][i]==6){
    # caluculate the average between 6 cities
    dist1 <- deg.dist(dftemp[14][[1]][i],dftemp[13][[1]][i],dftemp[16][[1]][i],dftemp[15][[1]][i])
    dist2 <- deg.dist(dftemp[16][[1]][i],dftemp[15][[1]][i],dftemp[18][[1]][i],dftemp[17][[1]][i])
    dist3 <- deg.dist(dftemp[18][[1]][i],dftemp[17][[1]][i],dftemp[20][[1]][i],dftemp[19][[1]][i])
    dist4 <- deg.dist(dftemp[20][[1]][i],dftemp[19][[1]][i],dftemp[22][[1]][i],dftemp[21][[1]][i])
    dist5 <- deg.dist(dftemp[22][[1]][i],dftemp[21][[1]][i],dftemp[24][[1]][i],dftemp[23][[1]][i])
    dftemp[35][[1]][i]<-((dist1+dist2+dist3+dist4+dist5)/5)
  } else if(dftemp[1][[1]][i]==7){
    # caluculate the average between 7 cities
    dist1 <- deg.dist(dftemp[14][[1]][i],dftemp[13][[1]][i],dftemp[16][[1]][i],dftemp[15][[1]][i])
    dist2 <- deg.dist(dftemp[16][[1]][i],dftemp[15][[1]][i],dftemp[18][[1]][i],dftemp[17][[1]][i])
    dist3 <- deg.dist(dftemp[18][[1]][i],dftemp[17][[1]][i],dftemp[20][[1]][i],dftemp[19][[1]][i])
    dist4 <- deg.dist(dftemp[20][[1]][i],dftemp[19][[1]][i],dftemp[22][[1]][i],dftemp[21][[1]][i])
    dist5 <- deg.dist(dftemp[22][[1]][i],dftemp[21][[1]][i],dftemp[24][[1]][i],dftemp[23][[1]][i])
    dist6 <- deg.dist(dftemp[24][[1]][i],dftemp[23][[1]][i],dftemp[26][[1]][i],dftemp[25][[1]][i])
    dftemp[35][[1]][i]<-((dist1+dist2+dist3+dist4+dist5+dist6)/6)
  } else if(dftemp[1][[1]][i]==8){
    # caluculate the average between 8 cities
    dist1 <- deg.dist(dftemp[14][[1]][i],dftemp[13][[1]][i],dftemp[16][[1]][i],dftemp[15][[1]][i])
    dist2 <- deg.dist(dftemp[16][[1]][i],dftemp[15][[1]][i],dftemp[18][[1]][i],dftemp[17][[1]][i])
    dist3 <- deg.dist(dftemp[18][[1]][i],dftemp[17][[1]][i],dftemp[20][[1]][i],dftemp[19][[1]][i])
    dist4 <- deg.dist(dftemp[20][[1]][i],dftemp[19][[1]][i],dftemp[22][[1]][i],dftemp[21][[1]][i])
    dist5 <- deg.dist(dftemp[22][[1]][i],dftemp[21][[1]][i],dftemp[24][[1]][i],dftemp[23][[1]][i])
    dist6 <- deg.dist(dftemp[24][[1]][i],dftemp[23][[1]][i],dftemp[26][[1]][i],dftemp[25][[1]][i])
    dist7 <- deg.dist(dftemp[26][[1]][i],dftemp[25][[1]][i],dftemp[28][[1]][i],dftemp[27][[1]][i])
    dftemp[35][[1]][i]<-((dist1+dist2+dist3+dist4+dist5+dist6+dist7)/7)
  } else if(dftemp[1][[1]][i]==9){
    # caluculate the average between 9 cities
    dist1 <- deg.dist(dftemp[14][[1]][i],dftemp[13][[1]][i],dftemp[16][[1]][i],dftemp[15][[1]][i])
    dist2 <- deg.dist(dftemp[16][[1]][i],dftemp[15][[1]][i],dftemp[18][[1]][i],dftemp[17][[1]][i])
    dist3 <- deg.dist(dftemp[18][[1]][i],dftemp[17][[1]][i],dftemp[20][[1]][i],dftemp[19][[1]][i])
    dist4 <- deg.dist(dftemp[20][[1]][i],dftemp[19][[1]][i],dftemp[22][[1]][i],dftemp[21][[1]][i])
    dist5 <- deg.dist(dftemp[22][[1]][i],dftemp[21][[1]][i],dftemp[24][[1]][i],dftemp[23][[1]][i])
    dist6 <- deg.dist(dftemp[24][[1]][i],dftemp[23][[1]][i],dftemp[26][[1]][i],dftemp[25][[1]][i])
    dist7 <- deg.dist(dftemp[26][[1]][i],dftemp[25][[1]][i],dftemp[28][[1]][i],dftemp[27][[1]][i])
    dist8 <- deg.dist(dftemp[28][[1]][i],dftemp[27][[1]][i],dftemp[30][[1]][i],dftemp[29][[1]][i])
    dftemp[35][[1]][i]<-((dist1+dist2+dist3+dist4+dist5+dist6+dist7+dist8)/8)
  } else if(dftemp[1][[1]][i]==10){
    # caluculate the average between 10 cities
    dist1 <- deg.dist(dftemp[14][[1]][i],dftemp[13][[1]][i],dftemp[16][[1]][i],dftemp[15][[1]][i])
    dist2 <- deg.dist(dftemp[16][[1]][i],dftemp[15][[1]][i],dftemp[18][[1]][i],dftemp[17][[1]][i])
    dist3 <- deg.dist(dftemp[18][[1]][i],dftemp[17][[1]][i],dftemp[20][[1]][i],dftemp[19][[1]][i])
    dist4 <- deg.dist(dftemp[20][[1]][i],dftemp[19][[1]][i],dftemp[22][[1]][i],dftemp[21][[1]][i])
    dist5 <- deg.dist(dftemp[22][[1]][i],dftemp[21][[1]][i],dftemp[24][[1]][i],dftemp[23][[1]][i])
    dist6 <- deg.dist(dftemp[24][[1]][i],dftemp[23][[1]][i],dftemp[26][[1]][i],dftemp[25][[1]][i])
    dist7 <- deg.dist(dftemp[26][[1]][i],dftemp[25][[1]][i],dftemp[28][[1]][i],dftemp[27][[1]][i])
    dist8 <- deg.dist(dftemp[28][[1]][i],dftemp[27][[1]][i],dftemp[30][[1]][i],dftemp[29][[1]][i])
    dist9 <- deg.dist(dftemp[30][[1]][i],dftemp[29][[1]][i],dftemp[32][[1]][i],dftemp[31][[1]][i])
    dftemp[35][[1]][i]<-((dist1+dist2+dist3+dist4+dist5+dist6+dist7+dist8+dist9)/9)
  } else {
    # caluculate the average between 11 cities
    dist1 <- deg.dist(dftemp[14][[1]][i],dftemp[13][[1]][i],dftemp[16][[1]][i],dftemp[15][[1]][i])
    dist2 <- deg.dist(dftemp[16][[1]][i],dftemp[15][[1]][i],dftemp[18][[1]][i],dftemp[17][[1]][i])
    dist3 <- deg.dist(dftemp[18][[1]][i],dftemp[17][[1]][i],dftemp[20][[1]][i],dftemp[19][[1]][i])
    dist4 <- deg.dist(dftemp[20][[1]][i],dftemp[19][[1]][i],dftemp[22][[1]][i],dftemp[21][[1]][i])
    dist5 <- deg.dist(dftemp[22][[1]][i],dftemp[21][[1]][i],dftemp[24][[1]][i],dftemp[23][[1]][i])
    dist6 <- deg.dist(dftemp[24][[1]][i],dftemp[23][[1]][i],dftemp[26][[1]][i],dftemp[25][[1]][i])
    dist7 <- deg.dist(dftemp[26][[1]][i],dftemp[25][[1]][i],dftemp[28][[1]][i],dftemp[27][[1]][i])
    dist8 <- deg.dist(dftemp[28][[1]][i],dftemp[27][[1]][i],dftemp[30][[1]][i],dftemp[29][[1]][i])
    dist9 <- deg.dist(dftemp[30][[1]][i],dftemp[29][[1]][i],dftemp[32][[1]][i],dftemp[31][[1]][i])
    dist10 <- deg.dist(dftemp[32][[1]][i],dftemp[31][[1]][i],dftemp[34][[1]][i],dftemp[33][[1]][i])
    dftemp[35][[1]][i]<-((dist1+dist2+dist3+dist4+dist5+dist6+dist7+dist8+dist9+dist10)/10)
  }
}

# combine with master 
cityData <- cbind(cityData, dftemp$avg_distance)

# rename columns
colnames(cityData)[21] <- "avg_distance"

# reorder the columns
cityData <- cityData[,c(1:17,21,18:20)]

# Convert session and joining date to lubridate format
cityData["session_date"] <- ymd(anydate(as.POSIXct(as.numeric(cityData$unix_timestamp),origin="1970-01-01")))
cityData$joining_date <- ymd(cityData$joining_date)

# reorder the columns and save useful columns
cityData <- cityData[,c(1,22,3:5,18:21)]

# write out master df
write.csv(cityData, "cityData.csv", row.names = FALSE)

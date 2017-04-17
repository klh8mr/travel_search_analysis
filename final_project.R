library(jsonlite)
library(plyr)

setwd("~/Documents/MSDS/SYS_6016/final_project")

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

df_states <- cbind(df_states, dftemp3, dftemp4)
colnames(df_states)[12] <- "states"
colnames(df_states)[13] <- "num_of_states"

# combine with master
cityData <- cbind(cityData, df_states)

# reorder the columns
cityData <- cityData[,c(1:15,30:31,19:29,16:18)]
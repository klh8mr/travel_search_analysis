library(jsonlite)
library(plyr)
library(stringr)
library(arules)
library(magrittr)

setwd("~/UVaMSDS/MachineLearning/FinalProject")
df <- fromJSON(file("city_search.json"))
df_save <- df

df_new <- matrix(unlist(df$user), ncol=3, byrow=TRUE)

df <- cbind(df, df_new) %>%
  data.frame()





### Continue Cleaning dataframe
###############################################
# NOTE - added below because columns were still lists
df[,1:3] <- sapply(df[,1:3], function(x) unlist(x))

names(df)[names(df)=="1"] <- "user_id"
names(df)[names(df)=="2"] <- "joining_date"
names(df)[names(df)=="3"] <- "country"

df = subset(df, select = -c(user))


### Create Sparse Matrix
###############################################
# List of unique cities
city <- strsplit(as.character(df$cities), ", ") 

cities <- city %>%
  unlist() %>%
  unique()

# Create column for each city
names <- c(colnames(df), cities)

for (i in 7:(7 + length(cities))) {
  df[, i] <- NA
}

colnames(df) <- names

# Loop through each row and city column to create binary indicators
for (i in 1:nrow(df)){
  for (j in 7:ncol(df)){
    if (names(df)[j] %in% city[[i]]){
      df[i, j] <- 1
    }
    else df[i, j] <- 0
  }
}

write.csv(df, "city_search_sparse.csv")

### Explore Data
###############################################
df <- read.csv("city_search_sparse.csv")
names(df)[names(df)=="X1"] <- "user_id"
names(df)[names(df)=="X2"] <- "joining_date"
names(df)[names(df)=="X3"] <- "country"

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


### Datetime
###############################################
library(anytime)
library(lubridate)

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


### Cities
###############################################
# no repeat city names with mispellings
sort(names(df[,8:ncol(df)]))

# list of most frequently searched cities
city_counts <- sapply(df[,8:ncol(df)], function(x) sum(x))
sort(city_counts, decreasing=TRUE)

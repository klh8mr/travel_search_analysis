library(jsonlite)
library(plyr)
library(stringr)
library(arules)
library(magrittr)
library(dplyr)

setwd("~/UVaMSDS/MachineLearning/FinalProject")

df <- read.csv("cityData.csv")
df_city <- df # save version of df to get avg and min distance later

## Create Sparse Matrix
###############################################
# List of unique cities
city <- strsplit(as.character(df$cities), ", ") 

cities <- city %>%
  unlist() %>%
  unique()

# Create column for each city
names <- c(colnames(df), cities)

for (i in 11:(10 + length(cities))) {
  df[, i] <- NA
}

colnames(df) <- names

# Loop through each row and city column to create binary indicators
for (i in 1:nrow(df)){
  for (j in 11:ncol(df)){
    if (names(df)[j] %in% city[[i]]){
      df[i, j] <- 1
    }
    else df[i, j] <- 0
  }
}

# Convert session and joining date to lubridate format
df$session_date <- ymd(df$session_date)
df$joining_date <- ymd(df$joining_date)

# Calculate days elapsed between join and session date
df$daysSinceJoin <- (df$session_date - df$joining_date) %>%
  as.character() %>%
  as.numeric()

# Write sparse Dataframe
write.csv(df, "city_search_sparse.csv", row.names = FALSE)


## Create user dataframe
###############################################
df <- read.csv("city_search_sparse.csv")
df$session_date <- ymd(df$session_date)
# Select Columns of interest from sparse dataframe
df_users <- unique(df[,c(2,8,10)]) 

# Create empty columns for calculations below
df_users$avgTimeElapsed <- 0
df_users$n_visits <- 0
df_users$CitiesSearched_avg <- 0
# Create object to store sum of city columns per user
cities_tot <- c()

for (x in unique(df$user_id)) {
  n_visits <- nrow(df[df$user_id==x,])
  df_users$n_visits[df_users$user_id==x] <- n_visits
  
  df_users$CitiesSearched_avg[df_users$user_id==x] <- sum(rowSums(df[df$user_id==x, 11:99], na.rm=T), na.rm=T)/n_visits
  
  if (n_visits>1){
    dates <- sort(df$session_date[df$user_id==x]) # dates a user visited the site, sorted
    timeElapsed <- diff(dates) # number of days between each visit
    df_users$avgTimeElapsed[df_users$user_id==x] <- mean(timeElapsed) # add the average days between visits to user df
  }
  
  cities_x <- c(x, colSums(df[df$user_id==x, 11:99]))
  cities_tot <- rbind(cities_tot, cities_x)
}

# Join df_users and city_tot by user_id
cities_tot <- data.frame(cities_tot)
names(cities_tot)[1] <- "user_id"
df_users <- full_join(df_users, cities_tot, by="user_id")

# get average and max distance searched
df_users["avg_distance"] <- NA
df_users["min_distance"] <- NA

df_users$concat <- paste(as.character(df_users$session_date), as.character(df_users$user_id))
df_city$concat <- paste(as.character(df_city$session_date), as.character(df_city$user_id))

for (i in 1:nrow(df_users)){
  if(df_users[4][[1]][i] == 0 && df_users[5][[1]][i] == 2){
    df_users[96][[1]][i] <- NA
    df_users[97][[1]][i] <- NA
  } else if(df_users[4][[1]][i] == 0.5 && df_users[5][[1]][i] == 3){
    df_users[96][[1]][i] <- NA
    df_users[97][[1]][i] <- NA
  } else {
    join_on <- df_users[98][[1]][i]
    df_users[96][[1]][i] <- df_city[which(df_city$concat == join_on),][,c(6)]
    df_users[97][[1]][i] <- df_city[which(df_city$concat == join_on),][,c(7)]
  }
}
    
# write out to csv to do formatting in excel - 
# fill in rest of avg_distance and min_distance
write.csv(df_users, "df_users_working.csv",row.names = FALSE)

# read in new df_users
df_users <- read.csv("df_users_working_clean.csv")

# reorder the columns
df_users <- df_users[,c(1:6,96:97,7:95)]

# Write user Dataframe
write.csv(df_users, "df_users.csv", row.names = FALSE)

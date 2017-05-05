library(jsonlite)
library(plyr)
library(stringr)
library(arules)
library(magrittr)
library(anytime)
library(lubridate)

setwd("~/UVaMSDS/MachineLearning/FinalProject")
df <- fromJSON(file("city_search.json"))
df_save <- df


## Clean dataframe
###############################################
df_new <- matrix(unlist(df$user), ncol=3, byrow=TRUE)

df <- cbind(df, df_new) %>%
  data.frame()

# NOTE - added below because columns were still lists
df[,1:3] <- sapply(df[,1:3], function(x) unlist(x))

names(df)[names(df)=="X1"] <- "user_id"
names(df)[names(df)=="X2"] <- "joining_date"
names(df)[names(df)=="X3"] <- "country"

df = subset(df, select = -c(user))


## Create Sparse Matrix
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

# Convert session and joining date to lubridate format
df$session_date <- ymd(anydate(df$unix_timestamp))
df$joining_date <- ymd(df$joining_date)

# Calculate days elapsed between join and session date
df$daysSinceJoin <- (df$session_date - df$joining_date) %>%
  as.character() %>%
  as.numeric()

# Write sparse Dataframe
write.csv(df, "city_search_sparse.csv")



## Create user dataframe
###############################################
df <- read.csv("city_search_sparse.csv")
df$session_date <- ymd(anydate(df$unix_timestamp))
# Select Columns of interest from sparse dataframe
df_users <- unique(df[,c(5:7)])

# Create empty columns for calculations below
df_users$avgTimeElapsed <- 0
df_users$n_visits <- 0
df_users$CitiesSearched_avg <- 0
# Create object to store sum of city columns per user
cities_tot <- c()

for (x in unique(df$user_id)) {
  n_visits <- nrow(df[df$user_id==x,])
  df_users$n_visits[df_users$user_id==x] <- n_visits
  
  df_users$CitiesSearched_avg[df_users$user_id==x] <- sum(rowSums(df[df$user_id==x, 8:96], na.rm=T), na.rm=T)/n_visits
  
  if (n_visits>1){
    dates <- sort(df$session_date[df$user_id==x]) # dates a user visited the site, sorted
    timeElapsed <- diff(dates) # number of days between each visit
    df_users$avgTimeElapsed[df_users$user_id==x] <- mean(timeElapsed) # add the average days between visits to user df
  }
  
  cities_x <- c(x, colSums(df[df$user_id==x, 8:96]))
  cities_tot <- rbind(cities_tot, cities_x)
}

# Join df_users and city_tot by user_id
cities_tot <- data.frame(cities_tot)
names(cities_tot)[1] <- "user_id"
df_users <- full_join(df_users, cities_tot, by="user_id")


# Write user Dataframe
write.csv(df_users, "df_users.csv")


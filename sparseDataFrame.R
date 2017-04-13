
library(jsonlite)
library(plyr)
library(stringr)

setwd("~/UVaMSDS/MachineLearning/FinalProject")
df <- fromJSON(file("city_search.json"))
df_save <- df

df_new <- matrix(unlist(df$user), ncol=3, byrow=TRUE)

df <- cbind(df, df_new) %>%
  data.frame()

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


### Association Rules
###############################################
library(arulesViz)
plot(rules,method="graph",interactive=TRUE,shading=NA)



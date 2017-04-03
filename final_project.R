
library(jsonlite)
library(plyr)

df <- fromJSON(file("city_search.json"))

df_new<-matrix(unlist(df$user), ncol=3, byrow=TRUE)

df <- cbind(df, df_new)

names(df)[names(df)=="1"] <- "user_id"
names(df)[names(df)=="2"] <- "joining_date"
names(df)[names(df)=="3"] <- "country"

df = subset(df, select = -c(user))

df_cities <-df[c(4,3)]
citylist <- strsplit(as.character(df_cities$cities), ",")

df_cities["num_of_cities"] <- NA

for (i in 1: length(citylist)){
  df_cities$num_of_cities[i] <- length(citylist[[i]])
}
df_cities$num_of_cities[1] <-length(citylist[[1]])


dftemp <- data.frame(matrix(ncol=1))
colnames(dftemp) <- c("num_of_cities")
dftemp2 <-data.frame(matrix(ncol=1))
colnames(dftemp2) <- c("num_of_cities")

for (i in 1:(length(df_cities))) {
  dftemp$num_of_cities <- as.character(length(df_cities[[i]]))
  dftemp2 <- rbind(dftemp, dftemp2)
}

dftemp2 <- dftemp2[-c(1),]

length(df_cities[[1]])
dftemp <- data.frame(matrix(ncol=5))
dftemp["File Name"] <- NA
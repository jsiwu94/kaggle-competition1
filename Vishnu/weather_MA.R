#Clear the list
rm(list = ls(all=TRUE)) 

#import the dataset
weather <- read.csv('/Users/vishnu/Documents/weather.csv',header=T) #weather cleaned file
data <- read.csv('/Users/vishnu/Documents/train_spray_merged_v2.csv',header=T) #masterdataset before merging weather variables, or remove weather variables.

#Check data type and format the date for Weather
str(weather)
weather$Date <- as.Date(weather$Date,format ="%Y-%m-%d",origin ="1970-01-01")

#Check data type and format the date for data
str(data)
data$Date <- as.Date(data$Date,format ="%Y-%m-%d",origin ="1970-01-01")

#Filtering the columns
data_1 <- data[c(2,4,5,13,14,15,16,37,38,39,12,17:36)]
weather_1 <- weather[c(1,42,4:11,14:18,23:41)]

#Filtering the weather data for stations
weather_1_station_1 <- subset(weather_1, Station ==1)
weather_1_station_2 <- subset(weather_1, Station ==2)

#Columns for calculating the moving average (only took following variables)
cols <- c('Tmax_Station','Tmin_Station','Tavg_Station','PrecipTotal_Station', 'rel_hum_station')


#Moving average calculation
MA1 =as.data.frame(weather_1_station_1$Date)
colnames(MA1)[1] <- "Date"
for ( i in 1:length(cols))
{
  savings <- weather_1_station_1 %>%
    select(Date, var = cols[i]) %>%
    mutate(V_30 = rollmean(var, k = 60, fill = 0),
           V_45 = rollmean(var, k = 90, fill = 0),
           V_60 = rollmean(var, k = 120, fill = 0),
           V_75 = rollmean(var, k = 150, fill = 0),
           V_90 = rollmean(var, k = 180, fill = 0))
  colnames(savings)[3] <- paste("MA_30",cols[i])
  colnames(savings)[4] <- paste("MA_45",cols[i])
  colnames(savings)[5] <- paste("MA_60",cols[i])
  colnames(savings)[6] <- paste("MA_75",cols[i])
  colnames(savings)[7] <- paste("MA_90",cols[i])
  savings <- subset(savings, select = -c(var))
  #savings$v <- c(cols[i])
  MA1 <- merge(MA1,savings,by="Date",all.x=TRUE)
}

#STATION 2
MA2 =as.data.frame(weather_1_station_2$Date)
colnames(MA2)[1] <- "Date"
for ( i in 1:length(cols))
{
  savings <- weather_1_station_2 %>%
    select(Date, var = cols[i]) %>%
    mutate(V_30 = rollmean(var, k = 60, fill = 0),
           V_45 = rollmean(var, k = 90, fill = 0),
           V_60 = rollmean(var, k = 120, fill = 0),
           V_75 = rollmean(var, k = 150, fill = 0),
           V_90 = rollmean(var, k = 180, fill = 0))
  colnames(savings)[3] <- paste("MA_30",cols[i])
  colnames(savings)[4] <- paste("MA_45",cols[i])
  colnames(savings)[5] <- paste("MA_60",cols[i])
  colnames(savings)[6] <- paste("MA_75",cols[i])
  colnames(savings)[7] <- paste("MA_90",cols[i])
  savings <- subset(savings, select = -c(var))
  #savings$v <- c(cols[i])
  MA2 <- merge(MA2,savings,by="Date",all.x=TRUE)
}


MA1$Station <- 1
MA2$Station <- 2
MA <- rbind(MA1,MA2)

weather_2 <- merge(weather_1,MA,by=c("Date","Station"))

#Data merge
data_final <- merge(data_1,weather_2,by=c("Date","Station"),all.x=TRUE)

#Mosquitoes species on hot encoding
data_final_1 = dummy.data.frame(data_final,names=c("Species"),sep="_")

data_final_1 <- data_final_1[c(1,2,10:17,3:9,18:94)]

#dropping the variables
rm(data,data_1,MA,MA1,MA2,savings,weather,weather_1,weather_1_station_1,weather_1_station_2,cols,i)
rm(data_final,weather_2)

write.csv(data_final_1,"Documents/Data_final.csv",row.names=F)

#########################################################################################
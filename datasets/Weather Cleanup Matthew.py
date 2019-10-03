# Cleaning Script written by Matthew Littman

#Station 1: CHICAGO O'HARE INTERNATIONAL AIRPORT Lat: 41.995 Lon: -87.933 Elev: 662 ft. above sea level
#Station 2: CHICAGO MIDWAY INTL ARPT Lat: 41.786 Lon: -87.752 Elev: 612 ft. above sea level

import pandas as pd
import numpy as np

# Load file "weather.csv"
df = pd.read_csv('weather.csv')

# Drop columns: "Dewpoint", "Depth", "Water1", "Snowfall"
df.drop(columns = ["DewPoint","Depth","Water1","SnowFall"])

############ Lat and Lon Columns ############

# Add columns to show where the station is (lat,lon) and its reading measures
df['WeatherLat'] = np.where(df['Station']==1, 41.995, 41.786)
df['WeatherLon'] = np.where(df['Station']==1, -87.933, -87.752)

# Move newly added columns from the end 2 spots to after the Station column
my_column = df.pop('WeatherLat')
my_column1 = df.pop('WeatherLon')
df.insert(1, my_column.name, my_column)
df.insert(2, my_column1.name, my_column1)

############ Tavg Column ############

# Replace M in Tavg with Tmax-Tmin / 2 + Tmin
df['Tavg'].replace(to_replace='M',value = (df['Tmax']-df['Tmin'])/2 + df['Tmin'], inplace=True)

# Change Tavg type from object to int
df[["Tavg"]] = df[["Tavg"]].astype(int)

# Separate into two dataframes by station 1 and 2
is_station1 =  df['Station']==1
dfStation1 = df[is_station1]
is_station2 =  df['Station']==2
dfStation2 = df[is_station2]

# Join both dataframes next to each other on the Date and add respective station label to end
a = dfStation1.join(dfStation2.set_index('Date'), on='Date', lsuffix = '_Station1', rsuffix='_Station2')

# Reset the Index of the whole dataframe
a.reset_index(drop = True,inplace = True)

############ Preciptotal Column ############

# Replace M value with value from Station on same date that actually has a value
a['PrecipTotal_Station1'].replace(to_replace='M',value = a['PrecipTotal_Station2'], inplace=True)
a['PrecipTotal_Station2'].replace(to_replace='M',value = a['PrecipTotal_Station1'], inplace=True)

# Replace T value with value from Station on same date that actually has a value
a['PrecipTotal_Station1'].replace(to_replace='  T',value = a['PrecipTotal_Station2'], inplace=True)
a['PrecipTotal_Station2'].replace(to_replace='  T',value = a['PrecipTotal_Station1'], inplace=True)

# Now that the only T's remaining are where its in both Station1 and 2, replace with 0
a['PrecipTotal_Station1'].replace(to_replace='  T',value = 0.00, inplace=True)
a['PrecipTotal_Station2'].replace(to_replace='  T',value = 0.00, inplace=True)

# Change PrecipTotal types to floats from objects
a[["PrecipTotal_Station1"]] = a[["PrecipTotal_Station1"]].astype(float)
a[["PrecipTotal_Station2"]] = a[["PrecipTotal_Station2"]].astype(float)

############ STNpressure Column ############

# Replace M value with value from Station on same date that actually has a value
a['StnPressure_Station1'].replace(to_replace='M',value = a['StnPressure_Station2'], inplace=True)
a['StnPressure_Station2'].replace(to_replace='M',value = a['StnPressure_Station1'], inplace=True)

# Find Indexes of spots where both Station 1 and Station 2 have value "M" and replace with avg of index above and below
Index_label1 = a[a['StnPressure_Station1']=='M'].index.tolist()
for i in Index_label1:
    a['StnPressure_Station2'].replace(to_replace='M',value = (float(a.at[i+1,'StnPressure_Station2']) + float(a.at[i-1,'StnPressure_Station2']))/2 , inplace=True)
    a['StnPressure_Station1'].replace(to_replace='M', value=(float(a.at[i+1, 'StnPressure_Station1']) + float(a.at[i-1, 'StnPressure_Station1'])) / 2, inplace=True)

# Change StnPressure types to floats from objects
a[["StnPressure_Station1"]] = a[["StnPressure_Station1"]].astype(float)
a[["StnPressure_Station2"]] = a[["StnPressure_Station2"]].astype(float)

############ Sealevel Column ############

# Replace M value with value from Station on same date that actually has a value
a['SeaLevel_Station1'].replace(to_replace='M',value = a['SeaLevel_Station2'], inplace=True)
a['SeaLevel_Station2'].replace(to_replace='M',value = a['SeaLevel_Station1'], inplace=True)

# Change Sealevel types to floats from objects
a[["SeaLevel_Station1"]] = a[["SeaLevel_Station1"]].astype(float)
a[["SeaLevel_Station2"]] = a[["SeaLevel_Station2"]].astype(float)

############ AvgSpeed Column ############

# Replace M value with value from Station on same date that actually has a value
a['AvgSpeed_Station1'].replace(to_replace='M',value = a['AvgSpeed_Station2'], inplace=True)
a['AvgSpeed_Station2'].replace(to_replace='M',value = a['AvgSpeed_Station1'], inplace=True)

# Change AvgSpeed types to floats from objects
a[["AvgSpeed_Station1"]] = a[["AvgSpeed_Station1"]].astype(float)
a[["AvgSpeed_Station2"]] = a[["AvgSpeed_Station2"]].astype(float)



# Write dataframe to excel
a.to_excel("weather_edit.xlsx")





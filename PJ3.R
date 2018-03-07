install.packages("sf")
install.packages("lubridate")
#install.packages("ggmap", type = "source")
install.packages("devtools")
library(devtools)
install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")
devtools::install_github("dkahle/ggmap")
#devtools::install_github("hadley/ggplot2")
install.packages("geosphere")

library(ggmap)
library(ggplot2)
library(lubridate)
library(readr)
library(sf)
library(ggrepel)
library(geosphere)

####PROBLEM-1####
#first function to load "sf_bikeshare_trips.csv"
#and to covert each column to appropriate data types, and save the file as "rds" file
clean_babstrip_rds = function(
  path = "~/Documents/UCDavis/STA141A/hw/hw3/bikes/sf_bikeshare_trips.csv",
  output = "~/Documents/UCDavis/STA141A/hw/hw3/sf_bikeshare_trips.rds"
){
  # 1. Clean up the data set. convert to appropriate data type
  babstrip = read_csv(path)
  babstrip$trip_id <- as.factor(babstrip$trip_id)
  babstrip$start_date <- strptime(babstrip$start_date,"%Y-%m-%d %H:%M:%S")
  babstrip$start_station_id <- as.factor(babstrip$start_station_id)
  babstrip$end_date <- strptime(babstrip$end_date,"%Y-%m-%d %H:%M:%S")
  babstrip$end_station_id <- as.factor(babstrip$end_station_id)
  # 2. save it as RDS file
  saveRDS(babstrip, output)
  
  babstrip

}


#second function to load "sf_bikeshare_station.csv"
#and to covert each column to appropriate data type, and save the file as "rds" file
clean_babsstation_rds = function(
  path2 = "~/Documents/UCDavis/STA141A/hw/hw3/bikes/sf_bike_share_stations.csv",
  output2 = "~/Documents/UCDavis/STA141A/hw/hw3/sf_bike_share_stations.rds"
){
  # 1. Clean up the data set. convert to appropriate data type
  babsstation = read_csv(path2)
  babsstation$station_id <- as.factor(babsstation$station_id)
  # 2. save it as RDS file  
  saveRDS(babsstation, output2)
  
  babsstation
  
}



####PROBLEM-2####
#call two previous created function to creat two "rds" file on the computer
clean_babsstation_rds()
clean_babstrip_rds()

#load two previous coverted rds files
ba_station = read_rds("~/Documents/UCDavis/STA141A/hw/hw3/sf_bike_share_stations.rds")
ba_trip = read_rds("~/Documents/UCDavis/STA141A/hw/hw3/sf_bikeshare_trips.rds")

# subset data for SF station, and remove all duplicate data
sf_station = subset(ba_station,ba_station$landmark == "San Francisco")
duplicate = duplicated(sf_station$station_id)
sf_station = sf_station[duplicate==FALSE,]

#find trip started at SF stations
sf_trip= subset(ba_trip,ba_trip$start_station_id %in% sf_station$station_id)

#match the trip frequency with station id
frequency=as.data.frame(table(sf_trip$start_station_id))
names(frequency) = c("station_id","freq")

#merge two files first with sf_station information, second with trip frequency information for each station
sf_station_freq = merge(frequency,sf_station, by = "station_id", is.x = T)

#creat a map
#find the loaction for center point of all station, and load the map
loc = sapply(sf_station[c("longitude","latitude")],mean)
sfmap = get_map(location = loc,zoom=14)

#put station on map and size of the dots represent size of trip frequency
ggmap(sfmap) + ggtitle("SAN FRANCISCO BIKE SHARING STATION MAP")+
  geom_point(aes(longitude,latitude,size = freq), alpha = 0.5, data=sf_station_freq) + labs(x="Longitude",y="Latitude") +
  geom_label_repel(aes(longitude,latitude,label = name, colour= "Red", fontface = "bold"),
                   label.padding = unit(0.05,"line") ,alpha = 0.8, data = sf_station_freq)



####PROBLEM-3####
#LA trip 
#function to load all five LA trip csv files and combine them into 1 file
#covert to appropriate data type, and save the one combined file as "rds" file
#received lots of help from professor Gupta for this question, especially the "lapply" part which load the files
clean_latrip_rds = function(
  path3 = "~/Documents/UCDavis/STA141A/hw/hw3/bikes/latrip",
  output3 = "~/Documents/UCDavis/STA141A/hw/hw3/bikes/la_trip_combined.rds"
){
  #get dirctory for all five files
  files=list.files(path3, full.names = TRUE)
  
  #read each file
  #use method which nick answered on piazza
  df= lapply (files, function(f) {
    x=read_csv(f)
    
    #convert "start time" to date form
    if (length(grep("-", x$start_time[1])) == 1){
      x$start_time = ymd_hms(x$start_time)
    } else {
      x$start_time = mdy_hm(x$start_time)
    }

    #convert "end time" to date form
    if (length(grep("-", x$end_time[1])) == 1){
      x$end_time = ymd_hms(x$end_time)
    } else {
      x$end_time = mdy_hm(x$end_time)
    }
    
    #change column names to match each other
    m= match(c("start_station_id","end_station_id"),names(x))
    names(x)[na.omit(m)]=c("start_station","end_station")
    
    x
  })
  
  #bind all five files
  df=do.call(rbind,df)
  
  #convert each column to appropriate data type
  df$trip_id = as.factor(df$trip_id)
  df$start_station = as.factor(df$start_station)
  df$end_station = as.factor(df$end_station)
  df$bike_id = as.integer(df$bike_id)
  df$trip_route_category = as.factor(df$trip_route_category)
  df$passholder_type = as.factor(df$passholder_type)
  
  #create a RDS file which combined all five cvs files on computer
  saveRDS(df,output3)
  
  df
}

#run the function
clean_latrip_rds()


#LA STATION 
#function to load "metro-bike-share-stations-2017-10-20.csv"
#and to covert to appropriate data type, and save the file as "rds" file
clean_lastation_rds = function(
  path4 = "~/Documents/UCDavis/STA141A/hw/hw3/bikes/metro-bike-share-stations-2017-10-20.csv",
  output4 = "~/Documents/UCDavis/STA141A/hw/hw3/bikes/metro-bike-share-stations-2017-10-20.rds"
){
  
  lastation = read_csv(path4)
  names(lastation) = tolower(names(lastation))
  lastation$go_live_date[[1]] = "7/7/2016"
  lastation$station_id = as.factor(lastation$station_id)
  lastation$go_live_date = mdy(lastation$go_live_date)
  #lastation$go_live_date[c(2:127)] = mdy(lastation$go_live_date[c(2:127)])  
  lastation$region = as.factor(lastation$region)
  lastation$status = as.factor(lastation$status)
  
  saveRDS(lastation,output4)
  
  lastation
}

#run the function
clean_lastation_rds()

#read the two RDS files previous created
la_trips_cmb = readRDS("~/Documents/UCDavis/STA141A/hw/hw3/bikes/la_trip_combined.rds")
la_station = readRDS("~/Documents/UCDavis/STA141A/hw/hw3/bikes/metro-bike-share-stations-2017-10-20.rds")




####PROBLEM-4####
#find the LA station coordinate from dataset la_trips_cmb
station_coordt = la_trips_cmb[c("start_station","start_lat","start_lon")]

#rename each column to match column name of la_station, and check for duplicate
names(station_coordt) = c("station_id","lat","lon")
duplicate1 = duplicated(station_coordt$station_id)
station_coordt = station_coordt[duplicate1==FALSE,]

#merge two files which assign station coordinate to each station
la_station_coordt = merge(la_station, station_coordt, by = "station_id", is.x = T)
#la_station=127,station_coordt=128,135

#match the frequency with station id
frequency2=as.data.frame(table(la_trips_cmb$start_station))
names(frequency2) = c("station_id","freq")
la_station_coordt = merge(frequency2,la_station_coordt, by = "station_id", is.x = T)


#plot map
#https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
#I googled coordinate location of LA downtown
#get the coordinate of la downtown, and get map
loc_ladt = c(lon = -118.2436, lat= 34.0522)
ladtmap = get_map(location = loc_ladt,zoom = 14)

#put station on map and size of the dots represent size of trip frequency
ggmap(ladtmap)+geom_point(aes(lon,lat,size= freq), alpha = 0.5, data=la_station_coordt) +
  ggtitle("LOS ANGELES DOWNTOWN BIKE SHARING STATION MAP")+ labs(x="Longitude",y="Latitude") +
  geom_label_repel(aes(lon,lat,label = station_name, colour= "red", fontface = "bold"),
                   label.padding= unit(0.05,"line") ,alpha = 0.8, data=la_station_coordt)




####PROBLEM-5####
#first part for Bay Area
#add start station coordinate to ba_trip, and remove any duplicated data
ba_trip5 = merge(ba_trip,ba_station, by = "start_station_id")
duplicate = duplicated(ba_trip5$trip_id)
ba_trip5 = ba_trip5[duplicate==FALSE,]
#remove useless columns
ba_trip5[16:19] = NULL

#assign column name to new add columns
names(ba_trip5)[14] = "start_lat"
names(ba_trip5)[15] = "start_lon"
names(ba_trip5)[8] = "end_station_id"

#add end station coordinate to ba_trip, and remove any duplicated data
ba_trip5 = merge(ba_trip5, ba_station, by ="end_station_id")
duplicate = duplicated(ba_trip5$trip_id)
ba_trip5 = ba_trip5[duplicate==FALSE,]
#remove useless columns
ba_trip5[20:23] = NULL

#assign column name to new add columns
names(ba_trip5)[18] = "end_lat"
names(ba_trip5)[19] = "end_lon"

#cut and assign labels to different time interval
ba_hour = hour(ba_trip5$start_date)
ba_trip5$interval = cut(ba_hour,breaks = c(-1,7,11,15,18,25),
                            labels = c("dawn","morning","noon","afternoon","night"))

#the following codes gives statistical information for the data set(bay area trips) we interested
#frequency of trips in different time interval
ba_fre = table(ba_trip5$interval)
ba_fre/sum(ba_fre)

#duration of trips in different time interval
aggregate(duration_sec~interval, data = ba_trip5,mean)

#distance of trips in different time interval
#find distance between start station and end station
ba_trip5$distance = distGeo(cbind(ba_trip5$start_lon,ba_trip5$start_lat),
                                cbind(ba_trip5$end_lon,ba_trip5$end_lat))

#subset only one way trips since we only have distant info for one-way trips
ba_ow = subset(ba_trip5,ba_trip5$distance != 0)
#median distance of trips in different time interval
aggregate(distance ~ interval, data = ba_ow, median)


#LA
#cut and assign labels to different time interval
la_hour = hour(la_trips_cmb$start_time)
la_trips_cmb$interval = cut(la_hour,breaks = c(-1,7,11,15,18,25),
                            labels = c("dawn","morning","noon","afternoon","night"))

#the following codes gives statistical information for the data set(Los Angeles trips) we interested
#frequency of trips in different time interval
la_fre=table(la_trips_cmb$interval)
la_fre/sum(la_fre)

#duration of trips in different time interval
aggregate(duration~interval, data = la_trips_cmb,mean)

#distance of trips in different time interval
#find distance between start station and end station
la_trips_cmb$distance = distGeo(cbind(la_trips_cmb$start_lon,la_trips_cmb$start_lat),
                    cbind(la_trips_cmb$end_lon,la_trips_cmb$end_lat))

#subset only one way trips since we only have distant info for one-way trips
la_ow = subset(la_trips_cmb,la_trips_cmb$trip_route_category == "One Way")
#median distance of trips in different time interval
aggregate(distance ~ interval, data = la_ow, median)



####PROBLEM-6####
#subset all one-way trips started at san francisco
sf_ow = subset(ba_ow,ba_ow$start_lat > 37.76 & ba_ow$start_lat<122.38)

#find the angel of each trip
sf_ow$bearing = bearing(cbind(sf_ow$start_lon,sf_ow$start_lat),
                            cbind(sf_ow$end_lon,sf_ow$end_lat))

#catagorize angel to four different directions "north, west, south, east"
sf_ow$direction = cut(sf_ow$bearing,breaks = c(-180,-135,-45,45,135,180),labels = c("south","west","north","east","south"))

#name the row and column of matrix and delete extra column
table_dirc=matrix(table(sf_ow$interval,sf_ow$direction),5,5)
table_dirc=table_dirc[,-5]
rownames(table_dirc) = c("Dawn","Morning","Noon","Afternoon","Night")
colnames(table_dirc) = c("South","West","North","East")

#plot mosaicplot to show direction of travel for different time of the day
par(mar = c(5, 3, 4, 11.5), xpd = TRUE)
mosaicplot(table_dirc, xlab = "Different Period of A Day", ylab = "Direction of Travel (One-Way)",
           main = "Direction of Trips (San Francisco)", color = c("Red","Yellow","Blue","Pink"))
color = c("Red","Yellow","Blue","Pink")
legend("topright", inset = c(-0.45,0), legend = colnames(table_dirc), fill = color )
dev.off()








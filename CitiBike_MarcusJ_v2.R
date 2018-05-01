#Marcus Joseph

##################
### Libraries ####
##################

install.packages("readr")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("googleway")
install.packages("sp")
install.packages("car")
install.packages("gmapsdistance")
install.packages("geosphere")
install.packages("leaflet")
install.packages("sm")
install.packages("scales")

library(readr) #laoding in the data
library(ggplot2) #visualizations
library(ggmap) #visualizations
library(googleway) #visualizations
library(sp) #visualizations
library(car) #visualizations
library(gmapsdistance) #mapping tools
library(geosphere) #mapping (calculating distance)
library(leaflet) #mapping routes visual
library(sm) #density visualization
library(scales) #pie chart visual

##################
#Read in the data#
##################
#All of these files are available on the Github repo but the destination may need to be adjusted
Jan = read_csv("~/Documents/GitHub/DevUp/201701-citibike-tripdata 2.csv")
Feb = read_csv("~/Documents/GitHub/DevUp/201702-citibike-tripdata 2.csv")
March = read_csv("~/Documents/GitHub/DevUp/201703-citibike-tripdata 2.csv")
April = read_csv("~/Documents/GitHub/DevUp/201704-citibike-tripdata 2.csv")
May = read_csv("~/Documents/GitHub/DevUp/201705-citibike-tripdata 2.csv")
June = read_csv("~/Documents/GitHub/DevUp/201706-citibike-tripdata 2.csv")
July = read_csv("~/Documents/GitHub/DevUp/201707-citibike-tripdata 2.csv")
Aug = read_csv("~/Documents/GitHub/DevUp/201708-citibike-tripdata 2.csv")
Sept = read_csv("~/Documents/GitHub/DevUp/201709-citibike-tripdata 2.csv")
Oct = read_csv("~/Documents/GitHub/DevUp/201710-citibike-tripdata 2.csv")
Nov = read_csv("~/Documents/GitHub/DevUp/201711-citibike-tripdata 2.csv")
Dec = read_csv("~/Documents/GitHub/DevUp/201712-citibike-tripdata 2.csv")

##################
### Merge data ###
##################
#Column Names
colnames(Jan) == colnames(April) #The column names of Jan, Feb, & March are different than the rest
colnames(Jan) = colnames(April) #This fixes Jan's column names
colnames(Feb) = colnames(April) #This fixes Feb's column names
colnames(March) = colnames(April) #This fixes March's column names

#Bind the data sets into 1 complete dataframe (all)
all = rbind(Jan, Feb, March, April, May, June, July, Aug, Sept, Oct, Nov, Dec)

#Check that all rows were added to the new dataframe
sum(c(nrow(Jan), 
      nrow(Feb), 
      nrow(March), 
      nrow(April), 
      nrow(May), 
      nrow(June), 
      nrow(July), 
      nrow(Aug), 
      nrow(Sept), 
      nrow(Oct), 
      nrow(Nov), 
      nrow(Dec))) == nrow(all) #TRUE --> the number of rows match

#Creating a smaller sample that can be used for running models
set.seed(123)
all2 = all[sample(NROW(all),16624),] #99% confidence level with +-1 confidence interval w/ random sampling

#Reviewing the merged data#
dim(all)
str(all) #This will give us more insight into the variables and their class types
summary(all) #This will just give some basic stats on the citi bike data
#From this summary, we see a trip duration max that is Max.:9735948 --> we need to remove some outliers
#Additionally, the mean is much larger than the median, indicating that our outliers are skewing the data
table(is.na(all))
#    FALSE      TRUE 
#245342701    127154 
#-We can also see that there are 127,154 NA's in our dataset

#What percentage of our data is NA?
127154/(127154+245342701) #This shows that 0.05180025% of our data is NA

#Which columns have missing data?
table(is.na(all$tripduration)) #no NA's
table(is.na(all$starttime)) #no NA's
table(is.na(all$stoptime)) #no NA's
table(is.na(all$`start station id`)) #no NA's
table(is.na(all$`start station name`)) #no NA's
table(is.na(all$`start station latitude`)) #no NA's
table(is.na(all$`start station longitude`)) #no NA's
table(is.na(all$`end station id`)) #no NA's
table(is.na(all$`end station name`)) #no NA's
table(is.na(all$`end station latitude`)) #no NA's
table(is.na(all$`end station longitude`)) #no NA's
table(is.na(all$bikeid)) #no NA's
table(is.na(all$usertype)) #15909 NA values
table(is.na(all$`birth year`)) #111245 NA values
table(is.na(all$gender)) #no NA's

#We will deal with these ouliers and NA's as we do analysis with each of these variables

##################
# Visualizations #
##################
#--------------------------------------------------------------#
#1)	Top 5 stations with the most starts (showing # of starts)
#--------------------------------------------------------------#
set.seed(123)

NROW(unique(all$`start station name`)) #there are 819 station names
NROW(unique(all$`start station id`)) #But there are only 811 station ID's

station = as.data.frame(table(all$`start station name`)) #this creates a data frame of the station names and their freq
station_2 = station[order(station$Freq, decreasing = TRUE), ] #this orders the "station" df from most freq to least freq
top_5_stations = station_2[1:5,] #This makes a variable of only the top5 stations
top_stations_all = merge(x = top_5_stations, y = all, by.x = "Var1", by.y = "start station name")
top_5_stations = top_5_stations[order(top_5_stations$Var1),] #order stations alphabetically
top_5_stations$long = unique(top_stations_all$`start station longitude`) #merge the station longitude
top_5_stations$lat = unique(top_stations_all$`start station latitude`) #merge the station latitude
colnames(top_5_stations) = c("Stations","Frequency","long","lat")
ggplot(top_5_stations, 
       aes(x = Stations, 
           y = Frequency, 
           fill=Stations)) + 
  geom_bar(stat = "identity") + 
  scale_fill_hue(c = 40) #Bar chart of top 5 stations

#Bar chart that highlights most popular station
barplot(top_5_stations$Frequency, 
        main="Start Station Frequencies", 
        horiz=FALSE,
        names.arg=top_5_stations$Stations, 
        angle = 45,
        
        col = c("dark red", 
                "gray", 
                "gray",
                "gray",
                "gray")
)
#These will add the values over each bar to show it's value for the barchart above
text(.7, 150000, "162,716")
text(1.9, 95000, "112,218")
text(3.1, 95000, "108,590")
text(4.3, 95000, "107,133")
text(5.5, 95000, "105,610")

apiKey = "AIzaSyCuKTe1ugBhLWRc157M-mOUWb-CgtQyeO4"
newYork = google_geocode(address = "New York", key = apiKey)

coords = cbind(Longitude = as.numeric(as.character(top_5_stations$long)), Latitude = as.numeric(as.character(top_5_stations$lat))) #coordinates of the top 5 stations
map.pts = SpatialPointsDataFrame(coords, top_5_stations, proj4string = CRS("+init=epsg:4326")) #make the plot points of the top 5 stations
map = qmap('New_York', zoom = 13) #make the map visual of the plotted 5 top stations

#This will show the locations of each of the bike stations
map + geom_point(data = top_5_stations, aes(x = top_5_stations$long, y = top_5_stations$lat), color="red", size=3, alpha=0.5)

#This will create a basic density map to show where the most station activity is
map + stat_density2d(
  aes(x = all2$`start station longitude`, y = all2$`start station latitude`, fill = ..level.., alpha = ..level..*3), 
  size = 3, bins = 5, data = all2, geom = "polygon") +
  scale_fill_gradient(low = "black", high = "red")

#--------------------------------------------------------------#
#2)	Trip duration by user type #-------------------------------#
#--------------------------------------------------------------#
set.seed(123)

table(is.na(all$usertype)) #As was noted above, there are 15909 NA's in the "user type" data
all_complUserType = as.data.frame(all[complete.cases(all$usertype),]) #Since there is only 2 options for the user type and there is no way to indicate what the missing values could be, we will simply remove them
table(is.na(all_complUserType$usertype)) #This shows that we have removed the NA's

all2_complUserType = all_complUserType[sample(NROW(all_complUserType),16624),] #sample of data

# Making a boxplot visual of the User Type and the duration
boxplot(tripduration~usertype,data=all2_complUserType, main="User Type's Trip Duration",
        xlab="User Type", ylab="Trip Duration (in seconds)") 
#However, we can see the massive outliers skewing the data here
##We can use a capping method to take the extreme points outside the 1.5 * IQR limits
####We cap it by replacing those observations outside the lower limit with the value of 5th percentile
####### The replace the points that lie above the upper limit, with the value of 95th percentile

#function to cap outliers
cap_outliers = function(x){
  quantiles = quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] = quantiles[1]
  x[ x > quantiles[2] ] = quantiles[2]
  x
}
# This is a new data frame with the capping method applied
all5 = as.data.frame(cap_outliers(all2_complUserType$tripduration))
all5$usertype = all2_complUserType$usertype #We add the user type column back
colnames(all5) = c("tripduration", "usertype") #fix the column names

# This is a boxplot of the usertype and trip duration with the outlier capping treatment
boxplot(tripduration~usertype,data=all5, main="User Type's Trip Duration",
        xlab="User Type", ylab="Trip Duration (in seconds)") 

hist(all5$tripduration[all5$usertype=="Customer"],main="Customer Trip Duration Histogram", xlab = "Trip Duration") #customer histogram visual
hist(all5$tripduration[all5$usertype=="Subscriber"],main="Subscriber Trip Duration Histogram", xlab = "Trip Duration") #subscriber histogram visual

cust_dens = density(all5$tripduration[all5$usertype=="Customer"]) # density visual for Customer variable
subs_dens = density(all5$tripduration[all5$usertype=="Subscriber"]) # density visual for Subscriber variable
plot(cust_dens,main="Trip Duration Density (Customer)",xlab="Trip Duration") # density visual for Customer
plot(subs_dens,main="Trip Duration Density (Subscriber)", xlab="Trip Duration")# density visual for Subscriber


#trip duration by user type data frame stats
cust_mean = as.data.frame(mean(all5$tripduration[all5$usertype=="Customer"]))
cust_mean$max = max(all5$tripduration[all5$usertype=="Customer"])
cust_mean$min = min(all5$tripduration[all5$usertype=="Customer"])
cust_mean$count = NROW(all5$tripduration[all5$usertype=="Customer"])
colnames(cust_mean) = c("mean","max","min","count")
View(cust_mean)

subs_mean = as.data.frame(mean(all5$tripduration[all5$usertype=="Subscriber"]))
subs_mean$max = max(all5$tripduration[all5$usertype=="Subscriber"])
subs_mean$min = min(all5$tripduration[all5$usertype=="Subscriber"])
subs_mean$count = NROW(all5$tripduration[all5$usertype=="Subscriber"])
colnames(subs_mean) = c("mean","max","min","count")
View(subs_mean)

#--------------------------------------------------------------#
#3)	Most popular trips based on start station and stop station)
#--------------------------------------------------------------#
set.seed(123)

#To find the most popular trips based on start station and stop station...
##We will first start by concatenation the start and stop stations into one column
all6 = as.data.frame(paste(all$`start station name`, "|", all$`end station name`))
colnames(all6) = "popular_trips"

#Next, we will order the column by its frequency
popularTrips = as.data.frame(table(all6$popular_trips))
popularTrips_2 = popularTrips[order(popularTrips$Freq, decreasing = TRUE),]
top_5_trips = popularTrips_2[1:5,]
colnames(top_5_trips) = c("Trips", "Frequency") #And we will grab pull out the top 5 most popular
top_5_trips_split = as.data.frame(do.call('rbind', strsplit(as.character(top_5_trips$Trips),'|',fixed=TRUE))) #Now we will seperate the column back into 2 sep columns
top_5_trips_split$Frequency = top_5_trips$Frequency #Add back the frequency column
colnames(top_5_trips_split) = c("Start_Station","End_Station","Frequency")

other_trips = popularTrips_2[6:NROW(popularTrips_2),]
other_trips2 = as.data.frame(sum(other_trips$Freq))
other_trips2$tripCategory = "All Others"
top_trips_combined = as.data.frame(sum(top_5_trips$Frequency))
top_trips_combined$tripCategory = "Top 5"
colnames(top_trips_combined) = c("freq","tripCategory")
colnames(other_trips2) = c("freq","tripCategory")
pieChart = rbind(other_trips2,top_trips_combined) #make the data frame for the pie chart

pieChart2 = ggplot(pieChart, aes(x="", y=freq, fill=tripCategory))+
  geom_bar(width = 1, stat = "identity")

pie = pieChart2 + coord_polar("y", start=0)

blank_theme = theme_minimal()+ #This is the theme for the pie chart
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

pie + scale_fill_brewer("Blues") + blank_theme + #This is the pie chart visual
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = freq, 
                label = c("")))+
  ggtitle("Pie Chart of Most Popular Trips vs. All Other Trips")

labels = c("All Other Trips", "Top 5 Trips")
pie(pieChart$freq, labels = labels, main="Pie Chart of Top 5 Trips vs. All others")
#------------------------------------------------------------------#
#4)	Rider performance by Gender and Age based on avg trip distance-# 
#------------------------------------------------------------------#
set.seed(123)

#This is a visualization showing the spread of ages among riders
birth_year_df = as.data.frame(as.numeric(all$`birth year`))
colnames(birth_year_df) = "birthyear"

ggplot(birth_year_df,
       aes(x = birthyear)) +
  geom_density(fill = "blue", alpha = .4) +
  geom_vline(aes(xintercept = median (birthyear, na.rm = TRUE)),
             color = "red", linetype = "dashed", lwd = 1)

111245/(111245+16253412) #.6% of birth year data is NA

gender_birth_table = as.data.frame(table(all$`birth year`,all$gender==0)) #this shows the amount of "0" genders and how they often associate with NA birth years
(sum(gender_birth_table$Freq[gender_birth_table$Var1=='NULL' & gender_birth_table$Var2==TRUE], na.rm = FALSE))/
  (sum(gender_birth_table$Freq[gender_birth_table$Var2==TRUE], na.rm = FALSE)) #This returns the % of "0" genders that also have a "NULL" for the birth year (~95.9%)
#Because most of our erroneous genders are associated with NA birth years and the overall number of erroneous data points is minimal, we will simply remove both
##Additionally, we will remove all birth years that would put the rider over the age of 100 (<1917) to deal with outliers
all8 = all[!all$`birth year`=="NULL",] #remove the NA Birth Years
all8 = all8[!all8$gender==0,] #remove the 0's from Gender
all8 = all8[!all8$`birth year`=="1916",]
all8 = all8[!all8$`birth year`=="1915",]
all8 = all8[!all8$`birth year`=="1914",]
all8 = all8[!all8$`birth year`=="1913",]
all8 = all8[!all8$`birth year`=="1912",]
all8 = all8[!all8$`birth year`=="1911",]
all8 = all8[!all8$`birth year`=="1910",]
all8 = all8[!all8$`birth year`=="1909",]
all8 = all8[!all8$`birth year`=="1908",]
all8 = all8[!all8$`birth year`=="1907",]
all8 = all8[!all8$`birth year`=="1906",]
all8 = all8[!all8$`birth year`=="1905",]
all8 = all8[!all8$`birth year`=="1904",]
all8 = all8[!all8$`birth year`=="1903",]
all8 = all8[!all8$`birth year`=="1902",]
all8 = all8[!all8$`birth year`=="1901",]
all8 = all8[!all8$`birth year`=="1900",]
all8 = all8[!all8$`birth year`=="1899",]
all8 = all8[!all8$`birth year`=="1898",]
all8 = all8[!all8$`birth year`=="1897",]
all8 = all8[!all8$`birth year`=="1896",]
all8 = all8[!all8$`birth year`=="1895",]
all8 = all8[!all8$`birth year`=="1894",]
all8 = all8[!all8$`birth year`=="1893",]
all8 = all8[!all8$`birth year`=="1892",]
all8 = all8[!all8$`birth year`=="1891",]
all8 = all8[!all8$`birth year`=="1890",]
all8 = all8[!all8$`birth year`=="1889",]
all8 = all8[!all8$`birth year`=="1888",]
all8 = all8[!all8$`birth year`=="1887",]
all8 = all8[!all8$`birth year`=="1886",]
all8 = all8[!all8$`birth year`=="1885",]
all8 = all8[!all8$`birth year`=="1884",]
all8 = all8[!all8$`birth year`=="1883",]
all8 = all8[!all8$`birth year`=="1882",]
all8 = all8[!all8$`birth year`=="1881",]
all8 = all8[!all8$`birth year`=="1880",]
all8 = all8[!all8$`birth year`=="1879",]
all8 = all8[!all8$`birth year`=="1878",]
all8 = all8[!all8$`birth year`=="1877",]
all8 = all8[!all8$`birth year`=="1876",]
all8 = all8[!all8$`birth year`=="1875",]
all8 = all8[!all8$`birth year`=="1874",]
all8 = all8[!all8$`birth year`=="1873",]
all8 = all8[!all8$`birth year`=="1872",]
all8 = all8[!all8$`birth year`=="1871",]
all8 = all8[!all8$`birth year`=="1870",]
all8 = all8[!all8$`birth year`=="1869",]
all8 = all8[!all8$`birth year`=="1868",]
all8 = all8[!all8$`birth year`=="1867",]
all8 = all8[!all8$`birth year`=="1866",]
all8 = all8[!all8$`birth year`=="1865",]
all8 = all8[!all8$`birth year`=="1864",]
all8 = all8[!all8$`birth year`=="1863",]
all8 = all8[!all8$`birth year`=="1862",]
all8 = all8[!all8$`birth year`=="1861",]
all8 = all8[!all8$`birth year`=="1860",]
all8 = all8[!all8$`birth year`=="1859",]
all8 = all8[!all8$`birth year`=="1858",]

NROW(all) #the original dataset
NROW(all8) #After removing the erroneous data points
median(birth_year_df$birthyear, na.rm = 1) #The median birth year = 1982

set.seed(123)
all3 = all8[sample(NROW(all8),16624),] #99% confidence level with +-1 confidence interval w/ random sampling
dist_start = cbind(all3$`start station longitude`,all3$`start station latitude`) #bind the start coordinates
dist_end = cbind(all3$`end station longitude`,all3$`end station latitude`) #bind the the stop coordinates
distance = as.matrix(distm(dist_start, dist_end, fun = distHaversine)) #Put the start and stop coordinates into a matrix
indicator = row(distance) - col(distance) # this creates an indicator for the matrix diagonals
Split = split(distance, indicator) # this groups on the diagonal values
all3$Distance = as.data.frame(Split[NROW(all3)]) #this returns the distances that align with our original dataframe
all3 = na.omit(as.data.frame(all3)) #remove remaining NULL rows
table(is.na(all3)) #confirm the NA's are removed

#Scatter Plot showing relationship between birth year, gender, and distance with the mean highlighted
ggplot(all3, aes(x=`birth year`, y=Distance, color = gender)) + 
  stat_summary(aes(x = `birth year`,group=1), fun.x=mean, color="red", geom="line",group=1) +
  geom_point() + 
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5)) + 
  ggtitle("Birth Year vs. Distance (with Gender highlights)")

#create speed column
all3$Speed = all3$tripduration/all3$Distance
all3 = all3[all3$Speed!="Inf",] #remove values where the rider rode a bike back to the starting station

Birth_Year_Correlation_speed = as.data.frame(cor(all3$Speed, as.numeric(all3$`birth year`))) #correlation between birth year and speed
colnames(Birth_Year_Correlation_speed) = "Birth_Year_Correlation_Speed"
Birth_Year_Correlation_speed
Gender_Correlation_speed = as.data.frame(cor(all3$Speed, all3$gender)) #correlation between gender and speed
colnames(Gender_Correlation_speed) = "Gender_Correlation_Speed"
Gender_Correlation_speed

Birth_Year_Correlation_distance = as.data.frame(cor(all3$Distance, as.numeric(all3$`birth year`))) #correlation between distance and birth year
colnames(Birth_Year_Correlation_distance) = "Birth_Year_Correlation"
Birth_Year_Correlation_distance
Gender_Correlation_distance = as.data.frame(cor(all3$Distance, all3$gender)) #correlation between distance and gender
colnames(Gender_Correlation_distance) = "Gender_Correlation"
Gender_Correlation_distance
#--------------------------------------------------------------#
#5)	What is the busiest bike in NYC in 2017? #-----------------#
#--------------------------------------------------------------#
set.seed(123)

popular_bike = as.data.frame(table(all$bikeid))#this makes a dataframe of a table showing the most popular bike IDs
popular_bike$Var1 = factor(popular_bike$Var1, levels = popular_bike$Var1[order(popular_bike$Freq)])#sort the bikes by descending freq
colnames(popular_bike) = c("Popular_Bikes_ID", "Frequency")

ggplot(popular_bike, aes(x=Popular_Bikes_ID, y=Frequency)) + 
  geom_point() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  ggtitle("Citi Bike Popularity by Bike ID") #this visual shows how the frequency of the bike usage is linear

top_5_popular_bikes = as.data.frame(popular_bike[1:5,])#This makes a dataframe of the 5 most popular bikes
Most_Pop_Bike = as.data.frame(all[all$bikeid==25738,])#Create dataframe of only the most popular bike ID
Most_Pop_Bike_Station = as.data.frame(table(Most_Pop_Bike$`start station name`))#this makes a dataframe of a table showing the stations from which the most freq used bike was checked out
Most_Pop_Bike_Station = Most_Pop_Bike_Station[order(Most_Pop_Bike_Station$Freq, decreasing = TRUE),]#sort the most popular bike's stations by descending freq
top_5_most_pop_bike_station = as.data.frame(Most_Pop_Bike_Station[1:5,])#This makes a dataframe of the 5 most popular start stations of the most popular bike
colnames(top_5_most_pop_bike_station) = c("Most_Pop_Bikes_Start_Station","Frequency")

#Total time of use for most popular bike
Pop_Bike_Time = as.data.frame(all$tripduration[all$bikeid=="25738"])
sum(Pop_Bike_Time$`all$tripduration[all$bikeid == "25738"]`) #This is the total seconds that the most popular bike was in use
#--------------------------------------------------------------#
#6)	Predicting a trip length#----------------------------------#
#--------------------------------------------------------------#
set.seed(123)

#This is the Google API key to utilize the Google Maps Distance Matrix API
set.api.key("AIzaSyC69tnhhVKh9wYyHK6c4dsDPLlPVaS68DA")
key = "AIzaSyC69tnhhVKh9wYyHK6c4dsDPLlPVaS68DA"
get.api.key() #This makes sure that it accepted our key

start_bike_station = c(40.71260, -73.96264) #Here, you enter the start station's lat/long
end_bike_station = c(40.72481, -73.94753) #Here, you enter the end station's lat/long

#This uses the googleway package to make a "route" variable (incorporating bike paths) for the mapping below
route_gmap = google_directions(origin = start_bike_station, 
                               destination = end_bike_station,
                               key = key,
                               departure_time = NULL,
                               arrival_time = NULL,
                               traffic_model = "best_guess",
                               mode = "bicycling")  

#This is the final route variable that goes into the leaflet mapping package below
df_route = decode_pl(route_gmap$routes$overview_polyline$points)

leaflet() %>%
  addTiles() %>%
  addPolylines(data = df_route, lat = ~lat, lng = ~lon) #this is the map visual of the route variable

#This calculates the length and distance of the two points above (incorporating other variables such as bike routes and traffic)
distance_gmap = google_distance(origin = start_bike_station, #A different distance api is used here than in question 4 when calculating rider performance because this Api gives a more detailed output but it could not handle the data frame from question 4
                                destination = end_bike_station,
                                key = key,
                                mode = "bicycling")

#This is a data frame of our distance calculation output
distance_gmap_df = as.data.frame(distance_gmap$rows$elements)
colnames(distance_gmap_df) = c("Distance","Duration","Status")
distance_gmap_df

#Here is a small testing sample to show the accuracy (without the perdiction yet)
set.seed(123)
testing_sample = all[sample(NROW(all),10),] 
testing_sample
# All test coordinates
start_test = cbind(testing_sample$`start station latitude`,testing_sample$`start station longitude`)
end_test = cbind(testing_sample$`end station latitude`, testing_sample$`end station longitude`)

# the start coordinates
s_row1 = start_test[1,]
s_row2 = start_test[2,]
s_row3 = start_test[3,]
s_row4 = start_test[4,]
s_row5 = start_test[5,]
s_row6 = start_test[6,]
s_row7 = start_test[7,]
s_row8 = start_test[8,]
s_row9 = start_test[9,]
s_row10 = start_test[10,]

# the end coordinates
e_row1 = end_test[1,]
e_row2 = end_test[2,]
e_row3 = end_test[3,]
e_row4 = end_test[4,]
e_row5 = end_test[5,]
e_row6 = end_test[6,]
e_row7 = end_test[7,]
e_row8 = end_test[8,]
e_row9 = end_test[9,]
e_row10 = end_test[10,]

# Putting test coordinates through the Google API
test1 = as.data.frame(google_distance(origin = s_row1, 
                                      destination = e_row1,
                                      key = key,
                                      mode = "bicycling"))
test2 = as.data.frame(google_distance(origin = s_row2, 
                                      destination = e_row2,
                                      key = key,
                                      mode = "bicycling"))
test3 = as.data.frame(google_distance(origin = s_row3, 
                                      destination = e_row3,
                                      key = key,
                                      mode = "bicycling"))
test4 = as.data.frame(google_distance(origin = s_row4, 
                                      destination = e_row4,
                                      key = key,
                                      mode = "bicycling"))
test5 = as.data.frame(google_distance(origin = s_row5, 
                                      destination = e_row5,
                                      key = key,
                                      mode = "bicycling"))
test6 = as.data.frame(google_distance(origin = s_row6, 
                                      destination = e_row6,
                                      key = key,
                                      mode = "bicycling"))
test7 = as.data.frame(google_distance(origin = s_row7, 
                                      destination = e_row7,
                                      key = key,
                                      mode = "bicycling"))
test8 = as.data.frame(google_distance(origin = s_row8, 
                                      destination = e_row8,
                                      key = key,
                                      mode = "bicycling"))
test9 = as.data.frame(google_distance(origin = s_row9, 
                                      destination = e_row9,
                                      key = key,
                                      mode = "bicycling"))
test10 = as.data.frame(google_distance(origin = s_row10, 
                                       destination = e_row10,
                                       key = key,
                                       mode = "bicycling"))

#This pulls the duration from the Google output
test1 = as.data.frame(test1$elements)
colnames(test1) = c("outputDistance","outputDuration","Pass")
test2 = as.data.frame(test2$elements)
colnames(test2) = c("outputDistance","outputDuration","Pass")
test3 = as.data.frame(test3$elements)
colnames(test3) = c("outputDistance","outputDuration","Pass")
test4 = as.data.frame(test4$elements)
colnames(test4) = c("outputDistance","outputDuration","Pass")
test5 = as.data.frame(test5$elements)
colnames(test5) = c("outputDistance","outputDuration","Pass")
test6 = as.data.frame(test6$elements)
colnames(test6) = c("outputDistance","outputDuration","Pass")
test7 = as.data.frame(test7$elements)
colnames(test7) = c("outputDistance","outputDuration","Pass")
test8 = as.data.frame(test8$elements)
colnames(test8) = c("outputDistance","outputDuration","Pass")
test9 = as.data.frame(test9$elements)
colnames(test9) = c("outputDistance","outputDuration","Pass")
test10 = as.data.frame(test10$elements)
colnames(test10) = c("outputDistance","outputDuration","Pass")

# This binds all of our dataframes together
test_bind = rbind(test1$outputDuration, test2$outputDuration)
test_bind = rbind(test_bind, test3$outputDuration)
test_bind = rbind(test_bind, test4$outputDuration)
test_bind = rbind(test_bind, test5$outputDuration)
test_bind = rbind(test_bind, test6$outputDuration)
test_bind = rbind(test_bind, test7$outputDuration)
test_bind = rbind(test_bind, test8$outputDuration)
test_bind = rbind(test_bind, test9$outputDuration)
test_bind = rbind(test_bind, test10$outputDuration)

# This binds the actual duration (from the data) to our dataframe
test_bind$actualDuration = testing_sample$tripduration

# This makes a new column to calculate the absolute difference between the API predicted duration and the actual duration
test_bind$difference = abs(test_bind$value-test_bind$actualDuration)

max_difference = max(test_bind$difference)
min_difference = min(test_bind$difference)
average_difference = sum(test_bind$difference)/NROW(test_bind)
#install.packages("tidyverse", "ggplot", "RSocrata", "tidycensus")

library(tidyverse)
library(ggplot2)
library(RSocrata)
library(tidycensus)
setwd("~/GitHub/Chicago-TNC-analysis")

##Load data ----
##Note, this will load all data available. This will be a large amount of data and take a while!!!! 
##You should probably limit your request or download the data once - then load the data from your saved file. 
##See https://github.com/Chicago/RSocrata and https://dev.socrata.com/docs/queries/ for details of how to limit the data you retrieve. 
##The benefit of using RSocrate is that it preserves the field formatting from the data portal. If you downloaded a csv - you may need to reformat the dates. 

trips <- read.socrata("https://data.cityofchicago.org/Transportation/Transportation-Network-Providers-Trips/m6dm-c72p")

##'Vehicles' and 'Drivers' files are not used in this analysis, but code provided for future analysis. 
#drivers <-read.socrata("https://data.cityofchicago.org/Transportation/Transportation-Network-Providers-Drivers/j6wf-834c")
#drivers <- as_tibble(drivers)  
#vehicles <- read.socrata("https://data.cityofchicago.org/Transportation/Transportation-Network-Providers-Vehicles/bc6b-sq4u")
#vehicles <- as_tibble(vehicles)

##Connect EDAs to CCAs ----
## This connectes Chicago Community Areas to CMAP's Economically Disconnected Areas(EDAs). 
##For more information on CMAP's EDAs see: https://datahub.cmap.illinois.gov/dataset/on-to-2050-layer-edas-disinvested-areas

ccas<-read.csv("ccas.csv", sep = ",", header = TRUE) 
eda<-as.tibble(read.csv(file = "edas.csv", sep = ",", header = TRUE)) 

##Add some columns for summaries and removes holidays from data set (a little slow)----
trips <- as_tibble(trips) %>%
  mutate(
    pickup_census_tract = as.character(pickup_census_tract),
    dropoff_census_tract = as.character(dropoff_census_tract),
    trip_minutes = trip_seconds / 60,
    start_hour = as.numeric(strftime(trip_start_timestamp, format="%H")),
    start_minute = as.numeric(strftime(trip_start_timestamp, format="%M")),
    start_time = (as.numeric(str_c(start_hour, start_minute,sep= "."))),
    day_of_week = as.factor(strftime(trip_start_timestamp, format="%a")),
    is_weekday = !day_of_week %in% c("Sat", "Sun"),
    is_shared = trips_pooled > 1,
    total_cost = fare + additional_charges) %>%
  filter(trip_start_timestamp >= "2018-11-01 00:00:00" & trip_start_timestamp <= "2018-11-18 23:59:59" |
           trip_start_timestamp >= "2018-11-26 00:00:00" & trip_start_timestamp <= "2018-12-16 23:59:59") #removes holidays
week<-c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

## Run this to save you data locally and speed up next analysis
#write_rds(trips, "trips.RData")
#trips <- read_rds("trips.RData") ##Excludes holidays

#Match trip origins to EDAs
trips["eda_start_tract"]<-eda$GEOID10[match(trips$pickup_census_tract, eda$GEOID10)]
trips["eda_cluster_start"]<-eda$cluster_name[match(trips$eda_start_tract, eda$GEOID10)]
trips["cluster_side_start"]<-eda$side[match(trips$eda_start_tract, eda$GEOID10)]

#Match trip destinations to EDAs
trips["eda_end_tract"]<-eda$GEOID10[match(trips$dropoff_census_tract, eda$GEOID10)]
trips["eda_cluster_end"]<-eda$cluster_name[match(trips$eda_end_tract, eda$GEOID10)]
trips["cluster_side_end"]<-eda$side[match(trips$eda_end_tract, eda$GEOID10)]
trips<-trips %>% mutate(is_eda = (!is.na(eda_start_tract) | !is.na(eda_end_tract)))

#Map of trips----
#Note, this section requires a Census API key. 
# You can get one here https://api.census.gov/data/key_signup.html
# census_api_key("YOUR API KEY GOES HERE")  #run this line with your key. 

Cook_tract <- get_acs(geography = "tract",  state = 17, county = "031",  variables = "B19013_001", geometry = TRUE )

trip_count <- trips %>%
  group_by(pickup_census_tract) %>%
  summarise(pu_count = n())

Cook_tract <- left_join(Cook_tract, trip_count, by = c("GEOID"= "pickup_census_tract"))

#Note this map includes tracts outside Chicago. This data should probably be removed. 
Cook_tract%>%
  ggplot(aes(fill = pu_count)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 

#Trips by Time of Day----
days1<- #averages trips for days of the week that occur five times in the dataset timeframe
  trips %>%
  filter(day_of_week %in% c("Mon", "Tue", "Wed")) %>%
  group_by(day_of_week, start_hour) %>%
  summarize(count = n()) %>%
  mutate(by_day = count /5) %>%
  ungroup(day_of_week)
days2<-  #averages trips for days of the week that occur six times in the dataset timeframe
  trips %>%
  filter(day_of_week %in% c("Thu", "Fri", "Sat", "Sun")) %>%
  group_by(day_of_week, start_hour) %>%
  summarize(count = n()) %>%
  mutate(by_day = count /6) %>%
  ungroup(day_of_week)

all_days<-full_join(days1, days2) %>%
  mutate(day_of_week = factor(day_of_week, levels = week)) %>%
  arrange(day_of_week) %>%
  mutate(order = 1:n())

ggplot(all_days) +
  geom_line(mapping = aes(x=order, y = by_day, color = day_of_week), size = 2) +
  labs(x = "", y = "trips by hour", title = "Trip counts for average weekday and weekend",
       subtitle = "Trips by hour, city of Chicago, Nov-Dec 2018 (excluding holidays)" )+
  theme(axis.text.x=element_blank())

#Percent shared by time of day for EDAs and all trips outside EDAs (weekdays only) ----
eda_trip_shared<-trips %>%
  select(cluster_side_start, cluster_side_end, is_shared, is_eda,is_weekday,start_hour) %>%
  filter(is_eda==TRUE, is_weekday == TRUE) %>%
  mutate(eda_side = paste(cluster_side_start,cluster_side_end), eda_side = gsub("NA","",eda_side),
         eda_side = gsub(" ","", eda_side),eda_side = substr(eda_side,1,4),
         eda_side_of_city = ifelse(grepl("Nort", eda_side),"North/Northwest", ifelse(grepl("West", eda_side), "West", ifelse(
           grepl("Sout", eda_side), "South/Southwest", "other")))) %>% # This takes
                                                                       #the origin EDA if the origin and destination
                                                                       #are both in EDAs.
  group_by(eda_side_of_city,is_shared, start_hour) %>%
  summarize(count = n())
trips_shared<-trips %>%
  select(is_shared, is_eda,is_weekday,start_hour) %>%
  filter(is_eda==FALSE, is_weekday == TRUE) %>%
  group_by(is_shared, start_hour) %>%
  summarize(count = n())
eda_trip_shared<-eda_trip_shared %>%
  full_join(trips_shared) %>%
  spread(key=is_shared, value = count) %>%
  select(eda_side = eda_side_of_city, start_hour, not_shared = "FALSE", shared = "TRUE") %>%
  mutate(shared_pct = shared/(not_shared + shared))

ggplot(eda_trip_shared)+
  geom_line(mapping = aes(x=start_hour, y = shared_pct, group = eda_side, color = eda_side), size = 1.5)+
  labs(x = "Time of day", y = "percent rides shared", title = "Percent trips shared by EDAs, weekdays")+
  scale_color_discrete(name = "",
                       labels = c("North/Northwest Side", "South/Southwest Side", "West Side", "Non EDA"))

#Median length by time of day for EDAs and all trips outisde EDAs (weekdays only) ----
eda_trip_length<-trips %>%
  select(cluster_side_start, cluster_side_end, trip_minutes, trip_miles, is_eda,is_weekday,start_hour) %>%
  filter(!is.na(trip_minutes), !is.na(trip_miles), is_eda==TRUE, is_weekday == TRUE) %>% #takes out records that
                                                                                         #are missing time or length
  mutate(eda_side = paste(cluster_side_start,cluster_side_end), eda_side = gsub("NA","",eda_side),
         eda_side = gsub(" ","", eda_side),eda_side = substr(eda_side,1,4),
         eda_side2 = ifelse(grepl("Nort", eda_side),"North/Northwest", ifelse(grepl("West", eda_side), "West", ifelse(
           grepl("Sout", eda_side), "South/Southwest", "other")))) %>%
  group_by(eda_side2, start_hour, is_weekday) %>%
  summarize(med_length = median(trip_miles), med_time = median(trip_minutes), count = n())

trip_length <-trips %>%
  select(trip_minutes, trip_miles, is_eda,is_weekday,start_hour) %>%
  filter(!is.na(trip_minutes), !is.na(trip_miles), is_eda==FALSE, is_weekday == TRUE) %>%
  group_by(start_hour, is_weekday) %>%
  summarize(med_length = median(trip_miles), med_time = median(trip_minutes), count = n())

eda_trip_length <- full_join(eda_trip_length, trip_length)

ggplot(eda_trip_length)+
  geom_line(mapping = aes(x = start_hour, y = med_length, group = eda_side2, color = eda_side2), size = 2)+
  labs(x = "time", y = "length (miles)", title = "Median weekday trip length for trips starting or ending in EDAs")+
  scale_color_discrete(name = "",
                       labels = c("North/Northwest Side", "South/Southwest Side", "West Side", "Non EDA"))+
  ylim(2,8)

#TNC Speed ----
speed<-trips %>%
  select(start_hour, trip_miles, trip_minutes, is_weekday) %>%
  filter(!is.na(trip_minutes), !is.na(trip_miles),is_weekday == TRUE, trip_minutes > 0, trip_miles > 0) %>%
  mutate(hour = trip_minutes/60, mph = trip_miles/hour) %>%
  group_by(start_hour) %>%
  summarize(mph = mean(mph))

ggplot(speed)+
  geom_line(mapping = aes(x = start_hour, y = mph), size = 2)+
  labs(x = "time of day", y = "MPH", title = "Average speed, weekdays")
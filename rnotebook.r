---
title: "Case Study 1"
author: "Norvin"
output:
  html_document: default
  pdf_document: default
---

install packages for project

tidyverse
janitor
lubridate
scales
#ggplot2


#Import sample datasets into R (12 months)

{r}
df1 <- read.csv("~/Desktop/BikeData/sample_01/2020_04.csv")
df2 <- read.csv("~/Desktop/BikeData/sample_01/2020_05.csv")
df3 <- read.csv("~/Desktop/BikeData/sample_01/2020_06.csv")
df4 <- read.csv("~/Desktop/BikeData/sample_01/2020_07.csv")
df5 <- read.csv("~/Desktop/BikeData/sample_01/2020_08.csv")
df6 <- read.csv("~/Desktop/BikeData/sample_01/2020_09.csv")
df7 <- read.csv("~/Desktop/BikeData/sample_01/2020_10.csv")
df8 <- read.csv("~/Desktop/BikeData/sample_01/2020_11.csv")
df9 <- read.csv("~/Desktop/BikeData/sample_01/2020_12.csv")
df10 <- read.csv("~/Desktop/BikeData/sample_01/2021_01.csv")
df11 <- read.csv("~/Desktop/BikeData/sample_01/2021_02.csv")
df12 <- read.csv("~/Desktop/BikeData/sample_01/2021_03.csv")

Merge datasets into one big dataframe

{r}

bikedata <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)


Calculate the duration of all rides to investigate customer type usage

{r}

df <- bikedata %>% 
  mutate(duration = difftime(ended_at, started_at, units = "mins"))


Explore data to investigate potential errors

{r}

str(df)

dim(df)

#row count = 3,489,748 
#header count = 14

summary(df)
# max duration is too big, negative durations needs to be removed

nrow(subset(df, duration < 0))
# removed 10552

#check for duplicates
df_v1[!duplicated(df_v1), ]

#drop negative rides
df_v1 <- df[!(df$duration < 0),]

#run check
nrow(subset(df_v1, duration < 0))



remove empty and (na) rows

{r}

df_v2 <- janitor::remove_empty(df_v1, which = c("cols"))
df_v2 <- janitor::remove_empty(df_v1, which = c("rows"))

df_v2 <- na.omit(df_v2)

# 135,200 rows removed


Format dates

{r}


#change char to date
df_v2$started_at <- lubridate::ymd_hms(df_v2$started_at)
df_v2$ended_at <- lubridate::ymd_hms(df_v2$ended_at)



#hour date format
df_v2$start_hour <- lubridate::hour(df_v2$started_at)
df_v2$end_hour <- lubridate::hour(df_v2$ended_at)

#day date format
df_v2$start_day <- lubridate::day(df_v2$started_at)
df_v2$end_day <- lubridate::day(df_v2$ended_at)

#Date format
df_v2$start_date <- as.Date(df_v2$started_at)
df_v2$end_date <- as.Date(df_v2$ended_at)

df_v2$day_of_week <- format(as.Date(all_trips_init$date), "%A")

Mean & Median Summary

{r}

aggregate(df_v2$duration ~ df_v2$member_casual, FUN = mean)
aggregate(df_v2$duration ~ df_v2$member_casual, FUN = median)



Mean ride length by customer type

casual 45.10963 mins

member 15.92224 mins

Median ride length by customer type

casual 21.48333 mins

member 11.53333 mins

Rank most popular day of the week

{r}
day_rank_member <- df_v2 %>% count(member_casual=="member", day_of_week, sort =T)

{r}


df_v2$day_of_week <- ordered(df_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(df_v2$duration ~ df_v2$member_casual + df_v2$day_of_week, FUN = mean)


Ride usage trend

In any given day of the week, casual customers ride length is longer than member customers. Both Member and Casual customers ride longer on average during the weekends

visualizations

Number of rides by rider type

{r}
#get total number of rides for each customer type
df_v2 %>% count(df_v2$member_casual, sort = T)

#member
#print(1977362/3343996*100)
#59.13%

#casual
#print(1366634/3343996*100)
#40.87%

#create a small dataframe for pie viz
customer_type <- c("Member", "Casual")
ride_count <- c(1977362, 1366634)
percentage <- c(59.13, 40.87)

customer_type_count <- data.frame(customer_type, ride_count, percentage)

#visualize

ggplot(customer_type_count, aes(x="", y=ride_count, fill=customer_type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust=0.5)) +
  labs(title="Total rides by customer", x = NULL, y = paste("Total number of rides: ", comma(sum(ride_count)))) +
  theme_classic() +
  theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
  scale_fill_manual(values = c("Casual" = "#9C9EFE", "Member" = "#A66CFF"))  


# there are ways to do this without creating extra dataframes, still learning R, wish we were using Python in this instance. 


Average ride length by rider type

{r}

#creat df fron output
aggregate(df_v2$duration ~ df_v2$member_casual, FUN = mean)

customer <- c("Casual", "Member")
avg_minutes <- c(45.10, 15.92)

customer_avg_ride_length <- data.frame(customer, avg_length)

ggplot(data = customer_avg_ride_length, aes(x=customer, y=avg_minutes)) + geom_bar(stat = "identity", aes(fill=customer)) + scale_fill_manual(values = c("Casual" = "#9C9EFE", "Member" = "#A66CFF")) +theme(
    axis.title.y = element_text(vjust = +3.25),
    axis.title.x = element_text(vjust = -0.75)
  ) + labs(title = "Customer average ride length (minutes) ")


Number of rides by bike and rider type

{r}
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
df_v2 %>% count(member_casual, rideable_type, sort = T)




df_v2 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")  + scale_fill_brewer(palette = "Oranges")  + theme_minimal() +
  labs(title = "Total Rides by Customer & Bike Type") + 
  ylab("Number of Rides") + 
  xlab("Bike Type")




Number of rides by day and bike type

{r}

#df_v2 %>% count(rideable_type, day_of_week, sort=T)


df_v2 %>% 
  group_by(rideable_type, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(rideable_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge")  + scale_fill_brewer(palette = "Purples")  + theme_classic() + scale_y_continuous(label = comma) +
  labs(title = "Bike type by day of week") + 
  ylab("Number of Rides") + 
  xlab("Bike Type")


Average duration by bike type

{r}

aggregate(df_v2$duration ~ df_v2$rideable_type, FUN = mean)
avg_minutes <- c(31.5, 16.8, 15.9)

bikes <- c("Docked Bike", "Classic Bike", "Electric Bike")


duration_bike_type <- data.frame(bike_type, avg_minutes)


ggplot(data = duration_bike_type, aes(x=reorder(bikes, -avg_minutes), y=avg_minutes)) + geom_bar(stat = "identity", aes(fill=bikes)) + scale_fill_manual(values = c("Classic Bike" = "#9C9EFE", "Docked Bike" = "#A66CFF", "Electric Bike" = "#C689C6")) +theme(
    axis.title.y = element_text(vjust = +3),
    axis.title.x = element_text(vjust = -0.75)
  ) + ylab("Minutes") + xlab("Bike Types") + labs(title = "Bike type avg ride length (minutes) ")



Number of rides by month and rider type

{r}

df_v2 %>% 
  group_by(member_casual, months) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(member_casual, months)  %>% 
  ggplot(aes(x = ordered(months), y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) + 
  geom_point()  + scale_y_continuous(label = comma) + scale_fill_brewer(palette = "Oranges") +
  labs(title = "Number rides by month & customer") + 
  ylab("Total Rides") + 
  xlab("Month")




popular bike stations

{r}

popular_stations <- df_v2%>% count(df_v2$start_station_name, sort=T)

print(popular_stations)

total stations = 705, omit stations with names
sum of rides = 3343996
sd = 5976.404

sd = 17.87 or 18%
average = 4737

sample size = 226.79

print(705*.20)


We take a sample of the most popular stations (30) and plot them on a map. These 30 stations account for about 684,897 or 20% of rides. What is see is a tight cluster around the business center of Chicago (NE). This area accounts for places that tourists frequent such as museums, parks,Cloud Gate, colleges, and businesses. I noticed that none of this top stations are closer towards the coast. The beach areas are great areas for stations for causal riders taking a scenic route.


Tests

{r}

ggplot(df_v2$months, aes(df_v2$months, fill = n)) +  # Create ggplot2 histogram with default colors
  geom_histogram()
       


ASk

Prepare

Analyze

Share

Act

Findings

Ride average is about 20.34 minutes. Casual riders tend to ride longer at an average of 45 minutes and member riders ride about  16 minutes

 Saturday is the most popular day of the Week for both casual and member riders

Rides start low on Monday, then peaks on Saturday, then it drops off on Sunday.

Member riders have a consistent pattern of usage, while casual riders usage peaks during weekends. needs graph

On any week day, casual riders usage is longer than member riders

For each of the three bike products (Classic bike, Docked bike, Electric Bike) Docked bikes are the most popular among both causal and member riders

in the course of 12 months, ride usage peaks during the summer time and falls during winter (Dec, Jan, Feb, Apr)

Act 

{r}

print(ride_average)

Objectives

The main objective of this project was to explore and investigate how customers use the product.

Product: Bicycle Ride Sharing (Docked Bikes, Classic Bikes, Electric Bikes)

Customer: Members (monthly/Annual membership), Casual (pay as you go)

Data Sources

We are provided a list of datasets that show


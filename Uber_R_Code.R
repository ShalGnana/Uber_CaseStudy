#load the uber datafile into R and find out the structure
uber<-read.csv("Uber_assignment.csv",header = TRUE, na.strings="NA")
str(uber)


#Data cleaning: install lubridate package to format date to R format of yyyy-mm-dd and append seconds in time format
install.packages("lubridate")
library(lubridate)
uber$Request.timestamp<-parse_date_time(uber$Request.timestamp, c("dmy_HMS", "dmy_HM"))
uber$Drop.timestamp<-parse_date_time(uber$Drop.timestamp, c("dmy_HMS", "dmy_HM"))
str(uber)


#1.Visually identify the most pressing problems for Uber.
#Hint: Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; 
#identify the most problematic types of requests (city to airport / airport to city etc.) 
#and the time slots (early mornings, late evenings etc.) using plots
#check number of pickups from airport and city
summary(uber$Pickup.point)

# result shows that pickups from airport=3238 and pickups from city=3507

#check number of cancellations or 'no cars available' status
length(which(is.na(uber$Drop.timestamp)))
#3914 times when there have been cancellations or no cars available

#check when driver has cancelled bookings
length(which(is.na(uber$Driver.id)))
#there are 2650 cancellations by drivers

#make different timeslots: upto 5am=early mornings, 5am-9.59am=rush hour mornings
#10am-4.59pm=day time, 5pm-9.59pm=evening rush hour, 10pm onwards=late night
uber$reqhour<-hour(uber$Request.timestamp)
uber$time_slot=ifelse (uber$reqhour<5,"Early Morning", ifelse (uber$reqhour<10, "Morning RushHour", ifelse(uber$reqhour<17, "Daytime", ifelse(uber$reqhour<22, "Evening Rush", "Late Night"))))

#add another column that shows the journey
uber$Airport_to_City=ifelse(uber$Pickup.point=="Airport", 1,0)
str(uber)

summary(as.factor(uber$time_slot))
summary(as.factor(uber$Airport_to_City))
#Morning and evening rush hour are indeed the ones with higher bookings; evening rush hour being the most booked timeslot
#in other words, highest demand is in evening rush hour timing
#city to airport has the higher record compared to airport to city

#plot 1: Plot the Frequency of Cab Request status based on aesthetics chosen as Status
library(ggplot2)
ggplot(uber, mapping=aes(x=Status))+ geom_histogram(stat="count") + labs(title="Frequency of cab request Status", subtitle="Frequency of booking status", x="Booking request in a day(Hours)")
#This shows that frequency of cancelled or no cars available is higher than trips completed.

#plot 2: Aesthetics is chosen as time of day to understand the status of booking requests on an hourly basis 

req_status<-ggplot(uber, aes(x=uber$time_slot))+geom_bar(aes(fill=Status), stat="count", position=position_dodge())+ggtitle("Bookings throughout the day")+labs(y="Demands", x=" ")
req_status
#It is evident that the trips completed across the day is almost equal.
#The evening rush hour has the most non-availability of cars 
#The highest cancellations are in the morning rush hour
#The next step is to check the route ie airport to city or vice-versa.

#plot3: The next step is to check the route ie airport to city or vice-versa and see the status of bookings
#Find out the gap between supply and demand and show the same using plots.

#Find the time slots when the highest gap exists
#Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
req_pickup_point<-ggplot(uber, aes(x=uber$time_slot, fill=Pickup.point)) + geom_bar()+labs("Uber Booking Status based on pickup point") 
req_pickup_point


#show the graphs of status and pickup time against timeslots side by side.
install.packages("gridExtra")
library(gridExtra)
grid.arrange(req_status,req_pickup_point,ncol=2)
#This shows that evening rush hour has the highest demand for airport to city cabs
#It also shows that city to airport requests are the highest in the morning rush hour
#Driver cancellations are the highest in morning rush-hour timings
#non-availability of cabs is highest in evening rush hour

#find the journey duration in a weekday 
uber$journey_time_minutes=uber$Drop.timestamp-uber$Request.timestamp
uber$journey_time_minutes

#plot the journey duration and 
ggplot(uber, aes(y=uber$journey_time_minutes, x=uber$time_slot, fill=Pickup.point))+geom_boxplot()+labs(Title="Duration of the journey in a weekday", x="Booking requested", y="Journey duration")
mean(uber$journey_time_minutes,na.rm=TRUE)

#on an average a Time difference of 52.41152 mins 
#time taken from city to airport in the morning rush hour is high compared to the rest of the day
#This explains the driver cancellations being highest in the morning





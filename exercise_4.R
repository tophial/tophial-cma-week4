#exercise 4
#introduction to functions
testfun <- function(){}
class(testfun)
testfun <- function(){print("this function does nothing")}

testfun()
testfun <- function(sometext){print(sometext)}

testfun(sometext = "this function does slightly more, but still not much")
## [1] "this function does slightly more, but still not much"

my_age <- function(birthday, units){
  difftime(Sys.time(),birthday, units = units)
}

my_age(birthday = "1995-09-27", units = "days")
#If we want any of our parameters to have default value, we can assign an initial value to the parameter when declaring the variables within the round brackets.

my_age <- function(birthday, units = "days"){
  difftime(Sys.time(),birthday, units = units)
}

#task1----------------

#sqrt((x-lead(x,1))^2+(y-lead(y,1))^2)

euclidean_distance <- function(x,y){
  sqrt((x-lead(x,1))^2+(y-lead(y,1))^2)
}

#task 2 - prepare analysis
library(readr)        
library(dplyr)        
library(ggplot2)      
library(lubridate)

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",")

#make subset with sabi, rosa for time between 01.04.2015 - 15.04.2015

subset_wildschwein <- wildschwein_BE %>% 
  filter(TierName == "Sabi" | TierName =="Rosa") %>% 
  filter((DatetimeUTC >= as.Date('2015-04-01 00:00:00') & DatetimeUTC <= as.Date('2015-04-15 00:00:00')))

#task3-Create Join Key--------------
#match the two animals temporally.

#use datetimeround
subset_wildschwein <-subset_wildschwein %>% 
  mutate(datetimeround = round_date(
    DatetimeUTC,
    unit = "15 mins"
  ))


#Task 4: Measuring distance at concurrent locations--------------


#1 Split the wildschwein_filter object into one data.frame per animal
subset_Sabi <- subset_wildschwein %>% 
  filter(TierName== "Sabi")
subset_Rosa <- subset_wildschwein %>% 
  filter(TierName== "Rosa")

#2 Join* these datasets by the new Datetime column created in the last task. The joined observations are temporally close.
joinset <- full_join(subset_Rosa, subset_Sabi, by= "datetimeround", copy=FALSE, suffix = c("_rosa", "_sabi"))
#3 in the joined dataset, calculate Euclidean distances between concurrent (gleichzeitigen) observations and store the values in a new column
joinset<-joinset %>% 
  mutate(eucli_rosa = euclidean_distance(E_rosa, N_rosa),
         eucli_sabi = euclidean_distance(E_sabi, N_sabi))
#4 Use a reasonable threshold on distance to determine if the animals are also spatially close enough to constitute a meet (we use 100 meters). Store this Boolean information (TRUE/FALSE) in a new column


joinset<-joinset %>% 
  mutate(difference = abs(eucli_rosa- eucli_sabi), 
        threshold = (difference <= 100))

#Task 5: Visualize data

joinset_meets <- joinset %>% 
  group_by(threshold) %>% 
  filter(threshold==TRUE)


  ggplot()  +
  geom_point(subset_Rosa, mapping= aes(E, N, colour= "blue", alpha=0.01)) +
  geom_point(subset_Sabi, mapping = aes(E, N, colour= "red", alpha=0.01)) +
  xlim(c(2570000,2571000))+
  ylim(c(1204500,1205500))+
  theme(legend.position = "none")
  
  
  #Task 6 (optional): Visualize data as timecube with plotly

  library(plotly)

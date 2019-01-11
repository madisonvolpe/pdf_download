#### Analysis #### 

library(plyr)
library(tidyverse)
library(lubridate)

data <- read_csv("RelevantBookings.csv") # just relevant bookings 
main_data <- read_csv("bookings_clean.csv") #this includes all datapoints 

## Convert to Date 

data$Last.Booking.Date <- mdy(data$Last.Booking.Date)
data$Last.Release.Date <- mdy(data$Last.Release.Date)

main_data$Last.Booking.Date <- mdy(main_data$Last.Booking.Date)
main_data$Last.Release.Date <- mdy(main_data$Last.Release.Date)

range(data$Last.Booking.Date) # one year of data: 2017-12-31, 2019-01-01
range(main_data$Last.Booking.Date) # one year of data: 2017-12-31, 2019-01-01

## Create a short form of relevant data i.e: (warrants not unlisted)
main_data$uid <- 1:nrow(main_data)
data_short <- main_data %>% filter(uid %in% data$uid)

# check
nrow(data_short) == length(unique(data$uid))

#we can see that the duplicates arise from recommits so we should remove them from both datasets 
data_short <- data_short %>% filter(Booking.Type != "RECOMMIT")
data <- data %>% filter(Booking.Type != "RECOMMIT")

#final check 
data_dups <- data_short[c("Full.Name", "Last.Booking.Date")]
data_dups <- data_dups[duplicated(data_dups),]
nrow(data_dups) #equal to 0 so no longer duplicates... 
rm(data_dups)

## One Charge Analysis 

  # cleaning

  data_short$counts2 <- gsub("[^0-9,]","",data_short$counts)
  
  OneCharge <- data_short %>%
    filter(grepl("^\\d{1}$", data_short$counts2)) #choose those with only one charge
  
  OneCharge$charges_clean <- gsub(",", " ", OneCharge$charges_clean)
  
  OneCharge <- OneCharge %>%
    filter(!grepl("STATE", OneCharge$charges_clean))  #remove out of state warrants
                                                      #record - kept in state (out of county warrants)
  BRRR::skrrrahh(26)
  
  #rectifying w. alternative way 
  OneCharge2 <- data %>% 
    group_by(uid) %>%
    count(uid) %>%
    filter(n==1 | n == 2) %>%
    select(uid)
  
  Diff <- OneCharge[!is.element(OneCharge$uid, OneCharge2$uid),10]
  
  Diff <- data_short %>%
    filter(data_short$uid %in% Diff$uid)
  
  
  
  #
  
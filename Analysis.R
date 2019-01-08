#### Analysis ####
library(tidyverse)
data <- read_csv("bookings_clean.csv")

# create ID variable  
data$uid <- paste(data$FULL.NAME, data$LAST.BOOKING.DATE,sep = ",")

# string split columns by comma into new rows
data_analysis <- data %>% 
  select(FULL.NAME, LAST.BOOKING.DATE, LAST.RELEASE.DATE, BOOKING.TYPE, IN.JAIL., counts,
         agency, charges_clean) %>%
  mutate(all_charges = strsplit(as.character(charges_clean), ",")) %>% 
  unnest(all_charges)

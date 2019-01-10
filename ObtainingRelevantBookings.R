library(plyr)
library(tidyverse)
data <- read_csv("bookings_clean.csv")

# create ID variable  
data$uid <- 1:nrow(data)

# string split columns by comma into new rows

data_analysis <- data %>% 
  select(Full.Name, Last.Booking.Date, Last.Release.Date, Booking.Type, In.Jail, counts,
         agency, Charges, uid, charges_clean) %>%
  mutate(all_charges = strsplit(as.character(charges_clean), ",")) %>% 
  unnest(all_charges)

# Regex for License Suspensions - from patterns we noticed that ALMOST ALL of the charges
# have "LIC" in their charge description 

  ## Obtain IDs where grepl("LIC", data_analysis$charges_clean) == TRUE

  # LIC_ids <- data_analysis %>%
  #   filter(grepl("LIC", charges_clean)) %>%
  #   select(uid) %>%
  #   distinct()
  # 
  # LIC_Data <- data_analysis %>%
  #   filter(uid %in% LIC_ids$uid)

  ## obtains IDs where (grepl "DWLS", data_analysis$charges_clean) == TRUE
  
  # DWLS_ids <- data_analysis %>%
  #   filter(grepl("DWLS", charges_clean)) %>%
  #   select(uid) %>%
  #   distinct()
  # 
  # DWLS_Data <- data_analysis %>%
  #   filter(uid %in% DWLS_ids$uid)
  
# Go through LIC_Data and weed out not really driving with license suspended people 
  # LIC_Data_Clean <- LIC_Data %>%
  #   filter(!uid %in% c(23,51,55,90,168,214,262,284,304,355,377,405,
  #                      475,512,513,526,545,555,559,573,592,597,602,
  #                      699, 763, 777, 783, 819, 863, 935, 984, 1023,
  #                      1025, 1111, 1124, 1140, 1141, 1143, 1178, 1205, 
  #                      1281,1298, 1301, 1324, 1361, 1367, 1377, 1386,
  #                      1418, 1457, 1461, 1476, 1513, 1526, 1533, 1546, 1559, 
  #                      1567, 1572, 1575, 1599, 1633, 1645, 1660, 1692, 1693,
  #                      1698, 1728, 1730, 1766, 1788, 1822, 1845, 1851, 1869,
  #                      1870, 1886, 1923, 1956, 1957, 1961, 1969, 1970, 1984,
  #                      1999, 2061, 2076, 2108, 2151, 2180, 2225, 2274, 2327,
  #                      2331, 2332, 2380, 2392, 2442, 2448, 2455, 2460, 2470,
  #                      2495, 2507, 2513, 2578, 2586, 2629, 2776, 2800, 2809, 
  #                      2851, 2886, 2900, 2909, 2928, 2937, 2941, 3004, 3029,
  #                      3041, 3043, 3071, 9870, 9832, 9776, 9733, 9720, 9712,
  #                      9652, 9614, 9588, 9578, 9559, 9554, 9530, 9527, 9507,
  #                      9492, 9466, 9454, 9436, 9430, 9420, 9379, 9353, 9345,
  #                      9338, 9326, 9310, 9281, 9267, 9243, 9146, 9141, 9138,
  #                      9131, 9130, 9125, 9111, 9105, 9085, 9075, 9062, 9000,
  #                      8997, 8991, 8930, 8905, 8900, 8796, 8760, 8749, 8731,
  #                      8711, 8645, 8586, 8568, 8549, 8556, 8509, 8472, 8470,
  #                      8425, 8402, 8391, 8382))

# Or alternative way
  
  ## Obtain IDs where grepl("(?=.*LIC)(?=.*DRIV)", data_analysis$charges_clean) == TRUE
  # (?=.*match this expression)(?=.*match this too) - non consuming regular expression
  
  LIC_ids_2 <- data_analysis %>%
    filter(grepl("(?=.*LIC)(?=.*DRIV)", charges_clean, perl = T)) %>%
    select(uid) %>%
    distinct()
  
  LIC_Data_2 <- data_analysis %>%
    filter(uid %in% LIC_ids_2$uid)
  
  #DRIVE WITH EXPIRED LICENSE, COUNTERFEITED DRIVERS LICENSE, POSSESS FORGED DRIVERS LICENSE OR ID, POSSESS STOLEN PROP DRIVERS LICENSE
  # FTA/DRIVING WHILE LICENSE SUSPENDED OR REVOKED CAUSING DEATH, 
   # 555, 592, 783, 1124,1140, 1178, 1281,1361, 1386, 1476, 1575, 1583, 1645, 1698, 1845, 1869, 1870, 1961, 1969, 
  # 2380, 2392, 2776, 3171, 3402, 3548, 3835, 3899, 3902, 4337, 4338,4398, 4436, 4984, 5352, 5529, 5640, 5731, 5885, 6066, 6225,
  # 6310, 6338, 6499, 7319, 8023, 8176, 8186, 8190, 8391, 8509, 8645, 9000, 9125, 9243, 9326, 9420, 9466, 9492, 9870, 
  
  #Remove these IDS - they are associated with FRAUD, and underage drinking 
  
  LIC_Data_Clean <- LIC_Data_2 %>%
    filter(!uid %in% c(555,592,783,1124,1140,1178,1281,1361,1386,1476,1575,1583,1645, 1698, 1845, 1869, 1870, 1961, 1969,
                       2380, 2392, 2776, 3171, 3402, 3548, 3835, 3899, 3902, 4337, 4338,4398, 4436, 4984, 5352, 5529, 
                       5640, 5731, 5885, 6066, 6225,6310, 6338, 6499, 7319, 8023, 8176, 8186, 8190,8228,8391, 8509, 8645, 
                       9000, 9125, 9243, 9326, 9420, 9466, 9492, 9870))
  
  #Find those with MOVING TRAFFIC VIOL OPERATE MOTOR VEHICLE WO VALID LICENSE charge, it might have not been picked up in the last 
  #regex 
  
  Other1 <- data_analysis %>%
    filter(grepl(pattern = "MOVING TRAFFIC VIOL OPERATE MOTOR", charges_clean)) %>%
    select(uid) %>%
    distinct()
  
  Other1data <- data_analysis %>%
    filter(uid %in% Other1$uid)
  
  #Find those with DWLS(R) charge, it was not picked up in the last regex 
  
  Other2 <- data_analysis %>%
    filter(grepl(pattern = "DWLS", charges_clean)) %>%
    select(uid) %>%
    distinct()
  
  Other2data <- data_analysis %>%
    filter(uid %in% Other2$uid)

# Compare IDs of all three and only keep unique values. 
  
  ids1 <- unique(LIC_Data_Clean$uid)
  ids2 <- unique(Other1data$uid)
  ids3 <- unique(Other2data$uid)

  ids2 <- ids2[!ids2 %in% ids1]
  ids3 <- ids3[!ids3 %in% ids1]
  
  FinalIDS <- c(ids1, ids2, ids3)

# Final data set 
  
  FinalData <- data_analysis %>%
    filter(uid %in% FinalIDS)

# Write csv of dataset 
  
  write_csv(FinalData, "RelevantBookings.csv")
  
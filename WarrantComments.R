#### Obtaining All Warrant Comments/ Charges ####

library(tabulizer)
library(plyr)
library(tidyverse)
library(data.table)

## directory 

base <- "H:/Public/Justice Program/Reports and Pubs/Crim/Arnold Foundation Fees and Fines (Phase II)/Data/Cost Data/Florida/Leon_County/JailCosts/bookings_pdfs"
setwd(base)

## obtain pdfs from directory 

pdfs <- list.files(base)

## read pdfs and extract charges

charges <- list(NA)

for(i in 1:length(pdfs)){
  charges[[i]] <- extract_tables(pdfs[i], area = list(c(84, 352, 750, 470)),
                                    guess = F, output = "data.frame")
}

## Convert lists of characters within the main list to one dataframe per entry 
charges2 <- list(NA)

for(i in 1:length(charges)){
  charges2[[i]] <- ldply (charges[[i]], data.frame)
}

## convert list of dataframes to one df 
charges_final <- do.call(rbind.data.frame,charges2)


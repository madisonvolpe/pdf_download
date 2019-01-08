##### Read PDFs and Extract Tables #####
library(tabulizer)
library(plyr)
library(tidyverse)
library(data.table)

## directory 

base <- "H:/Public/Justice Program/Reports and Pubs/Crim/Arnold Foundation Fees and Fines (Phase II)/Data/Cost Data/Florida/Leon_County/JailCosts/bookings_pdfs"
setwd(base)

## obtain pdfs from directory 

pdfs <- list.files(base)

## read pdfs and extract tables 

pdf_tables <- list(NA)

for(i in 1:length(pdfs)){
  pdf_tables[[i]] <- extract_tables(pdfs[i])
}

## Convert lists of characters within the main list to one dataframe per entry 
pdf_tables2 <- list(NA)

for(i in 1:length(pdf_tables)){
 pdf_tables2[[i]] <- ldply (pdf_tables[[i]], data.frame)
}

## convert list of dataframes to one df 
bookings <- do.call(rbind.data.frame,pdf_tables2)

## drop first and last two columns 
bookings <- bookings[,-c(1,8:9)]

## give headers 
bookings[, ] <- lapply(bookings[, ], as.character)
names(bookings) <- bookings[1,]

## remove header columns dispersed through data 
for(i in 1:nrow(bookings)){
  if(grepl("^FULL", bookings$`FULL NAME`[i])==TRUE){
    bookings[i,] <- NA
  }
}

## remove rows with all blanks 
bookings <- bookings %>% drop_na(`FULL NAME`)

## Clean 
source("CleaningCharges.R")
bookings<-cleaningcharges(bookings)

for(i in 1:nrow(bookings)){
  bookings$counts[i] <- paste(bookings$counts[[i]],collapse=",")
  bookings$agency[i] <- paste(bookings$agency[[i]],collapse=",")
}

bookings$counts <- unlist(bookings$counts)
bookings$agency <- unlist(bookings$agency)

bookings$charges_clean <- gsub(",$", "", bookings$charges_clean)
bookings$charges_clean <- gsub(",,", ",", bookings$charges_clean)

## save a clean version 
write_csv(bookings, "bookings_clean.csv")

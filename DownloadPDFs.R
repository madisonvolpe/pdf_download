##### LEON COUNTY SHERIFF'S OFFICE: DAILY BOOKING REPORT ##### 

library(purrr)

## Download from 

dates <- data.frame(dates.seq = seq(as.Date("2018/1/1"), as.Date("2019/1/1"), "day"))
dates$weekday <- weekdays(dates$dates.seq)

## Reformat dates to match pdf 
dates$new <- strptime(as.character(dates$dates.seq), format = "%Y-%m-%d")
dates$new <- format(as.Date(dates$new, format = "%Y-%m-%d"), "%m-%d-%y")

## Remove preceding 0s 
dates$new <- gsub("^0","",dates$new)

for(i in 1:nrow(dates)){
  if(grepl("0\\d", dates$new[i])==TRUE){
    dates$new[i] <- gsub("0", "",dates$new[i])
  }
}

# Create URLS  
baseURL<- "http://media.graytvinc.com/documents/Booking+Report+"
URLs <- data.frame(URL = paste("http://media.graytvinc.com/documents/Booking+Report+",dates$new, ".pdf",sep=""))
URLs$ID <- dates$new
URLs$URL <- as.character(URLs$URL)

# Download folder 
download.folder = '~/Desktop/LeonCountyBookings/'

# Function to Download Data

get_data <- function(x, y){
  download.file(x, y, method = 'auto', quiet = FALSE, mode = "w",
                cacheOK = TRUE, extra = getOption("download.file.extra"))
}

get_data2 <- possibly(get_data, otherwise = NA)

# Wrap in purr possibly because it breaks 

for(i in 1:nrow(URLs)){
    pdf.name <- paste(download.folder, URLs$ID[i], '.pdf', sep = "")
    get_data2(URLs$URL[i], pdf.name)
}






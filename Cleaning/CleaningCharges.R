library(tidyverse)

cleaningcharges <- function(df){
  
  # extract counts
  df <- mutate(df, counts = str_extract_all(pattern = "[0-9]{1}+[L,F,T]", df[,'Charges']))
  
  # extract arrest agencies 
  agencies <- paste(c("LEON COUNTY SHERIFF", "FLORIDA STATE UNIVERSITY", "STATE HIGHWAY PATROL",
                      "FLORIDA A and M", "DOC PAROLE and PROBATION", "FLORIDA HIGHWAY PATROL", 
                      "TALLAHASSEE POLICE"), collapse = "|")

  df <- mutate(df, agency = str_extract_all(pattern = agencies, df[,'Charges']))
  
  # remove charge counts and agencies from charges section
  
  #charge counts 
  df <- mutate(df, charges_clean= gsub("([0-9]{1})+([L,F,T])", "\\2",df[,'Charges']))
  
  # agencies and stop words 
  agencies_stopwords<- paste(c("LEON COUNTY SHERIFF", "FLORIDA STATE UNIVERSITY", "STATE HIGHWAY PATROL",
                               "FLORIDA A and M", "DOC PAROLE and PROBATION", "FLORIDA HIGHWAY PATROL", 
                               "TALLAHASSEE POLICE", "TROOP H", "TALLAHASSEE", "DEPARTMENT", "POLICE",
                               "UNIVERSITY"),collapse = "|")
  
  df <- mutate(df, charges_clean = str_remove_all(pattern=agencies_stopwords, df[,'charges_clean']))
  
  # replace \n with , 
  
  df <- mutate(df, charges_clean =gsub(pattern = "\r", replacement = ",", df[,'charges_clean']))

  return(df)
}













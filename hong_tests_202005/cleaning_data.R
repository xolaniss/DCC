#Preliminaries
library(ccgarch)
library(readxl)
library(stringr)
setwd("/Users/xolanisibande/Desktop/hong_tests_202005")
rm(list=ls())
Sys.setenv(TZ="Africa/Johannesburg")


#Importing data

  #Sheet Import Function ---
Sheets_import<- function(path){
  data<- path %>%
    excel_sheets() %>%
    purrr::set_names() %>%
    map(read_excel,  col_names = F, path=path, skip = 1)
}


    # Importing Sheets ----
Sheets <-Sheets_import("NewData_GARCHIncluded.xlsx")

# Selecting relevant data
monthly_data <- Sheets[[1]][c(1,2,3,8,9)]
daily_data <- Sheets[[2]][c(1,2,3,13,14)]

#Creating Dates
monthly_data[,1] <- seq(as.Date("1791/09/1"), as.Date("2019/9/1"), "month")
daily_data[,1] <- daily_data[,1] %>% map(as.Date)


#Cleaning dates

  #Names
names(monthly_data) <- c( "Date",
                          "Dollar-Pound Returns",	
                          "SP500 Returns", 
                          "Dollar-Pound Volatility: Squared Residuals",
                          "SP500 Volatility: Squared Residuals")

names(daily_data) <- c( "Date",
                        "Dollar-Pound Returns"	,
                       "DJIA Returns",
                       "Dollar-Pound Volatility: Squared Residuals",	
                       "SP500 Volatility: Squared Residuals")


# Exporting Data

write.csv(monthly_data, file="monthly_data.csv", row.names = F)
write.csv(daily_data, file="daily_data.csv", row.names = F)






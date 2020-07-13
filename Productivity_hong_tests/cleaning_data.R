#Preliminaries
options("scipen" = 100, "digits" = "4")
library(ccgarch)
library(readxl)
library(stringr)
library(tidyverse)
library(lubridate)
setwd("/Users/xolanisibande/DCC/Productivity_hong_tests")
rm(list=ls())
Sys.setenv(TZ="Africa/Johannesburg")


#Importing data

  #Sheet Import Function ---
Sheets_import<- function(path){
  data<- path %>%
    excel_sheets() %>%
    purrr::set_names() %>%
    map_df(read_excel,  col_names = F, path=path, skip = 1)
}


    # Importing Sheets ----
Sheets <-Sheets_import("Productivity_DCC.xlsx")
names(Sheets) <- c("Date", "GI1", "LPG")
summary(Sheets)
Sheets <- Sheets %>% mutate(Date = parse_date_time(Date, orders = "Yq"))


# Exporting Data

write.csv(Sheets, file="Productivity.csv", row.names = F)






#Preliminaries
options("scipen" = 100, "digits" = "4")
library(ccgarch)
library(readxl)
library(stringr)
library(tidyverse)
library(lubridate)
setwd("/Users/xolanisibande/DCC/LCE")
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
Sheets <-Sheets_import("LSC_EMVID.xlsx")
names(Sheets) <- c("Date", "CURVATURE",	"LEVEL","SLOPE",	"EMVID")
str(Sheets)
summary(Sheets)
Sheets <- Sheets %>% mutate(Date = parse_date_time(Date, orders = "m/d/Y"))


# Exporting Data

write.csv(Sheets, file="LCE.csv", row.names = F)






#Preliminaries
options("scipen" = 100, "digits" = "4")
library(ccgarch)
library(readxl)
library(stringr)
library(tidyverse)
setwd("/Users/xolanisibande/DCC/CME_hong_tests")
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
Sheets <-Sheets_import("CME_balanced.xlsx")

Sheets$sentiment$...1 <- as.Date(Sheets$sentiment$...1, "%m/%d/%Y")
names(Sheets$sentiment) <- c("date", "cme_returns",	"news_sentiment",	"cme_variance")
Sheets$sentiment <- na.omit(Sheets$sentiment)

Sheets$infections$...1 <- as.Date(Sheets$ infections$...1, "%m/%d/%Y")
Sheets$infections[,5] <- NULL
Sheets$infections <- na.omit(Sheets$infections)
names(Sheets$infections) <- c("date", "cme_returns",	"daily_infect_emv_index",	"cme_variance")


# Exporting Data

write.csv(Sheets$sentiment, file="sentiment.csv", row.names = F)
write.csv(Sheets$infections, file="infections.csv", row.names = F)






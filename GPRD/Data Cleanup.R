# Description -------------------------------------------------------------
# Data cleaning for GPRD hong tests on 20 May 2020 - Xolani Sibande

# Packages ----------------------------------------------------------------
library("tidyverse")
library("readr")
library("readxl")
library("here")
library("lubridate")

# Import ------------------------------------------------------------------
Data <- read_excel(here("Balanced_lsc_gpr.xlsx"))

# Cleaning ----------------------------------------------------------------
Data <- Data %>% mutate(Date = parse_date_time(Date, "%m/%d/%Y"))

# Export ------------------------------------------------------------------
Data <- write.csv(Data, here("Clean_Data.csv"), row.names = FALSE)


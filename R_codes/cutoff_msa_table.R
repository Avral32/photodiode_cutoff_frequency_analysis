# Date:            2022-09-06 12:53:45
# R-version:       4.0.5 (2021.03.31)
# RStudio-version: 2022.2.2.485
# Platform:        x86_64-w64-mingw32
# CRAN Snapshot:   https://cran.microsoft.com/snapshot/2021-06-01/
# User:            TE517241

# Description:     Add an description here

# global options -----------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  # Deactivate auto usage of factors
Sys.setenv(LANGUAGE = 'en')        # Activate english error messages
# load packages ------------------------------------------------------------------------------------
library(magrittr)
library(purrr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(stringr)


data <- read.csv2("data/raw/table-MSA.csv", header = TRUE) %>% janitor::clean_names() %>% 
  dplyr::rename(RunOrder = i_run_order)



parts_ubr <- data.frame(parts = c(1:10),
                        device = c("GA2199", "GA2200", "GA0995", "GA0996",
                                   "2189_01", "2189_02", "2205_02", "2205_04", "2390_04", "2390_21"),
                        ubr_V = c(204, 193, 119, 118, 147, 149, 149, 163, 162, 159 ))
      
data <- data %>% 
  dplyr::left_join(x = data,
                   y = parts_ubr,
                   by = c("parts" = "parts"))  
  
data <- data %>% 
  dplyr::rename_with(str_to_title) 

data["Done"] <- ""   
  
write.table(data, file = "output/cutoff_msa.csv", sep = " ")

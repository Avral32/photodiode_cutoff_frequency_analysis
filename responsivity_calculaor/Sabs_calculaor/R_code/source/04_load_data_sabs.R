

# Description:     all functions which we need to load the measurement files

# global options -----------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  # Deaktivate the auto usage of factors
Sys.setenv(LANGUAGE = 'en')        # Activate english error messages

# load packages ------------------------------------------------------------------------------------
library(magrittr)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

# main functions -----------------------------------------------------------------------------------

# einlesen meherer Sabs datein
read_sabs_files <- function(paths) {
  header <- purrr::map2(paths, 1:length(paths), function(path, n) {
    import <- read_sabs_file(path)
    header <- import$header %>% 
      dplyr::mutate(header_id = n)
    return(header)
  }) %>% 
    dplyr::bind_rows()
  
  recipe <- purrr::map2(paths, 1:length(paths), function(path, n) {
    import <- read_sabs_file(path)
    data <- import$recipe %>% 
      dplyr::mutate(header_id = n)
    return(data)
  }) %>% 
    dplyr::bind_rows()
  
  data <- purrr::map2(paths, 1:length(paths), function(path, n) {
    import <- read_sabs_file(path)
    data <- import$data %>% 
      dplyr::mutate(header_id = n)
    return(data)
  }) %>% 
    dplyr::bind_rows()
  
  list(header = header,
       recipe = recipe,
       data   = data) %>%  
    return()
  
}


# einlesen der Sabs kurve
read_sabs_file <- function(path){
  try({
    # browser()
    cat(basename(path), "\n")
    file_daten <- scan(file   = path, 
                       what   = "character", 
                       sep    = "\n", 
                       quiet  = TRUE)
    
    flag_header <- find_flag(file_daten[1:40], "\\[Allgemein\\]")
    flag_rcp    <- find_flag(file_daten[1:40], "\\[Einstellungen Messung\\]")
    #flag_source <- find_flag(file_daten[1:40], "\\[Einstellung Quelle\\]")
    flag_data   <- find_flag(file_daten[1:40], "\\[Messwerte\\]")
    
    #sabs_f_name <- parse_filename(path)
    sabs_header <- read_header(file_daten, flag_header, flag_rcp)
    sabs_rcp    <- read_rcp(file_daten, flag_rcp, flag_data)
    sabs_data   <- read_data(file_daten, flag_data)
    
    list(header   = sabs_header, 
         recipe   = sabs_rcp, 
         #filename = sabs_f_name, 
         data     = sabs_data) %>% 
      return()
  })
}

read_zuordnung_file <- function(path) {
  readr::read_csv2(file = path,
                   col_types = cols(
                     fa_nr        = col_character(),
                     wafer_charge = col_character(),
                     wafer_nr     = col_integer(),
                     facility     = col_character()
                   )) %>% 
    return()
}

# support functions --------------------------------------------------------------------------------

# Zeilennummer mit Flag suchen
find_flag <- function(stream, string) {
  stream %>%  
    str_which(pattern = string) 
}

# Information hinter suchsting ausgeben
parse_info <- function(stream, pattern) {
  pattern <- as.character(pattern)
  stream %>% 
    str_subset(pattern = pattern) %>% 
    str_split(pattern = ";") %>% 
    unlist() %>%
    .[str_which(.,pattern = pattern) + 1] %>% 
    return()
}

# Header lesen 
read_header <- function(stream, start_header, end_header){
  # browser()
  tmp <- stream[start_header:(end_header - start_header)]
  
  date        <- parse_info(tmp, pattern = "Datum:") 
  #material_nr   <- parse_info(tmp, pattern = "Artikelnr:")
  article_name <- parse_info(tmp, pattern = "Bauteil:")
  fa_nr        <- parse_info(tmp, pattern = "Charge:")
  wafer_batch <- parse_info(tmp, pattern = "Wafer:")
  device_nr <- parse_info(tmp, pattern = "Bauteil-Nr.:")
  temperature   <- parse_info(tmp, pattern = "Temperatur:")
  
  data.frame(date, article_name, fa_nr, wafer_batch, device_nr, temperature) %>%
    return()
}

# Rezept lesen
read_rcp <- function(stream, start_rcp, end_rcp){
  # browser()
  tmp <- stream[start_rcp:(end_rcp + 1)]
  
  #measurement_device <- parse_info(tmp, pattern = "Messgerät:") # issue with "ä"
  filter_counts <- parse_info(tmp, pattern = "Filter Counts:")
  delay <- parse_info(tmp, pattern = "Delay:")
  nplc <- parse_info(tmp, pattern = "NPLC:")
  voltage   <- parse_info(tmp, pattern = "Spannung:")
  channel_b <- parse_info(tmp, pattern = "Kanal B:")
  dark_current <- parse_info(tmp, pattern = "Id Kanal A:")
  
  data.frame(filter_counts, delay, nplc, voltage, channel_b, dark_current) %>%
    return()
}

# Dateinnamen Parsen -> kommentar und Bauteil Nr.
# parse_filename <- function(path) {
#   file_name <- path %>% basename()
#   
#   flag <- file_name %>% str_count(pattern = "#") == 4
#   tmp  <- file_name %>% str_split(pattern = "#") %>% unlist()
#   
#   bt_nr     <- tmp[2]
#   kommentar <- ifelse(flag, tmp[3], "")
#   data.frame(kommentar,bt_nr) %>% 
#     return()
#   
# }

# Messdaten lesen
read_data <- function(stream, start_data){
  # browser()
  suppressWarnings({
    daten <- stream[(start_data + 2 ):length(stream)] %>% 
      read_csv2(locale = locale(decimal_mark = ",")) %>% # delim = ";"
      select(-matches("^X\\d+$"))
  })
  names(daten) <- names(daten) %>%
    str_remove(pattern = "\\[.*\\]$") %>% 
    str_trim()
  daten <- daten %>% janitor::clean_names()
  return(daten)
}


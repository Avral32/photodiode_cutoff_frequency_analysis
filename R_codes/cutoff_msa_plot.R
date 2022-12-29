# Date:            2022-07-21 09:05:49
# R-version:       4.0.5 (2021.03.31)
# RStudio-version: 2022.2.2.485
# Platform:        x86_64-w64-mingw32
# CRAN Snapshot:   https://cran.rstudio.com/
# User:            TE517241

# Description:     Add an description here


# global options -----------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  # Deactivate auto usage of factors
options(dplyr.summarise.inform = FALSE) # disable the message "you can override using the .groups"
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


dir_path <- "data/raw/spectra"

file_path <- list.files(path = dir_path,
                        pattern = "*.CSV",
                        full.names = TRUE)

csv <- dplyr::tibble(path = file_path, names = basename(file_path)) %>% 
       dplyr::mutate(data = purrr::map(path, read.csv2, nrows = 1000, header = TRUE, skip = 2)) %>%  
       tidyr::separate(col = names, into = c("oprs", "ro","part","mess"), sep = "_") %>% 
       dplyr::select(oprs, ro, part, mess, data) 
       
csv$mess <- str_sub(csv$mess, 3,3)

# storage device names and mess numbers to use later
device_mess_names <- csv %>% 
  dplyr::select(oprs, part,ro)
  


raw <- csv %>%
  tidyr::unnest(cols = data) %>%
  janitor::clean_names() %>% 
  dplyr::select(-x) %>% 
  dplyr::rename(f = trace, a = x1) %>% 
  # convert Hz to Mhz
  dplyr::mutate(f  = f / 1e6) %>% 
  dplyr::mutate_if(is.numeric, ~round(., 5))  %>% 
  ###
  dplyr::group_by(oprs, ro, part, f) %>% 
  dplyr::summarise(a = mean(a))

# plot to check the curves for each mess


plot <- raw %>% 
  dplyr::group_by(oprs, ro, part) %>% 
  dplyr::filter(part == c("part6") & oprs == c("NMUT"))
  

theme_set(theme_bw(base_size = 12))

ggplot(plot, aes(x = f, y = a, group = interaction(part, ro), color = as.factor(ro))) +
  geom_line(size = 2) + 
  scale_x_continuous(trans = 'log10') +
  # x axis
  annotation_logticks(sides = "b",
                      alpha = 0.12,
                      size = 1,
                      short = unit(30,"cm"),
                      #mid = unit(3,"mm"),
                      long = unit(30,"cm")
  ) +
  
  coord_cartesian(xlim = c(1,1e3), ylim = c(-20,10)) +
  theme_bw() +
  labs(title    = "MSA-cutoff_part6",
       y        = "Amplitude [dB]",
       x = "Frequency [MHz]")


#FSOBase::save_my_last_plot()



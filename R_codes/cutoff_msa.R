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
  dplyr::select(oprs, part,ro)  %>% 
  dplyr::group_by(oprs, part,ro) %>% 
  dplyr::distinct(oprs, part,ro)
  

#device_mess_names <- unique(device_mess_names)   
  


raw <- csv %>%
  tidyr::unnest(cols = data) %>%
  janitor::clean_names() %>% 
  dplyr::select(-x) %>% 
  dplyr::rename(f = trace, a = x1) %>% 
  # convert Hz to Mhz
  dplyr::mutate(f  = f / 1e6) %>% 
  dplyr::mutate_if(is.numeric, ~round(., 5))  %>% 
  dplyr::group_by(part)



# The range of frequency where the plateau occurs
frq_min <- 10
frq_max <- 25

    

# function that can calculate the cut off freqeuency of a single dataframe 
cut_off_freq <- function(dframe, minm = frq_min, maxm =frq_max){
  
  min <- which.min(abs(dframe$f - minm))
  max <- which.min(abs(dframe$f - maxm)) 
  media <- median(dframe$a[min:max])
  idx <- which(dframe$a == media)
  #cat("Median in the plateau:", media, "\n")
  #cat("Index of the median value in the plateau:", idx)   
  
  # index of cutoff frequency
  idx <- which.min(abs(3-abs(dframe$a[minm:length(dframe$f)]-media)))
  idx_real <- min + idx
  cutoff_freq <- dframe$f[idx_real]
  
  return(list(media, cutoff_freq))
}



cutoff <- raw %>% 
    #dplyr::group_by(oprs, ro, part,mess) %>% 
    ###
  dplyr::group_by(oprs, ro, part, f) %>% 
  # take the average of the 5 measurements
  dplyr::summarise(a = mean(a)) %>% 
  group_map(~ cut_off_freq(.x)) %>%
  transpose() %>% 
  tidyr::as_tibble(.name_repair = ~ c("median", "cutoff")) %>% 
  dplyr::mutate( cutoff = cutoff %>% as.numeric()) %>% 
  dplyr::mutate(median = median %>% as.numeric()) %>% 
  dplyr::mutate(risetime  = 0.35 /cutoff*1e+6) # ps
  

cut_off_bind <- cbind(cutoff,device_mess_names) 
cut_off_bind$part <- str_extract_all(cut_off_bind$part, "\\d+") 

cut_off <- cut_off %>%
dplyr::mutate(part = part %>% as.character())



parts <- data.frame(part = as.character(c(1:10)),
                        device = c("GA2199", "GA2200", "GA0995", "GA0996",
                                   "2189_01", "2189_02", "2205_02", "2205_04", "2390_04", "2390_21"))

cut_off <- cut_off %>% 
  dplyr::left_join(x = cut_off,
                   y = parts,
                   by = c("part" = "part")) 

cut_off <- cut_off %>% 
  dplyr::relocate(any_of(c("device", "oprs", "part", "ro")), .before = median)

cut_off_mean <- cut_off %>% 
  #dplyr::group_by(device, oprs, ro, part) %>% 
  #get the mean values for number of measurements., mess
  #dplyr::summarise(across(c(cutoff, risetime), mean)) %>% 
  dplyr::mutate(part = part %>%  as.numeric()) %>% 
  dplyr::arrange(-desc(part)) %>% 
  dplyr::rename(cutoff_MHz = cutoff,
                risetime_ps = risetime)  
  #write.csv2(file = "output/cutoff_freq_msa.csv")



# plot results

plot_data <- cut_off_mean 
   
# variation of risetime vs parts in general
theme_set(theme_bw(base_size = 18))             
ggplot(plot_data, aes(x = as.factor(part), y = risetime_ps), group = device) +
  geom_point(size = 4, alpha = 0.9, shape = 19, aes(colour = factor(device)))  +
  theme_bw() +
  labs(title = "Risetime determined with bandwidth MSA - parts",
       subtitle = "Variation of risetime with parts in general",
       x = "Parts",
       y = "Risetime [ps]") + 
  coord_cartesian(ylim = c(500, 1300))+
  geom_hline(yintercept = 600, linetype = "dashed", size = 0.4, color = "red") +
  geom_hline(yintercept = 880, linetype = "dashed", size = 0.4, color = "orange") +
  geom_hline(yintercept = 1100, linetype = "dashed", size = 0.4, color = "blue") +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 15),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        legend.position = "none") 
    #legend.title = element_blank())
    #,legend.position = "none"
    #guides(col = guide_legend("Devices"))
#FSOBase::save_my_last_plot()

# Variation of risetime vs parts with operators
theme_set(theme_bw(base_size = 18))             
ggplot(plot_data, aes(x = as.factor(part), y = risetime_ps, colour = factor(oprs))) +
  geom_point(size = 4, alpha = 0.9, shape = 19)  +
  theme_bw() +
  scale_fill_manual(values=c("#56ddc5", "#ff3db7")) +
  labs(title = "Risetime determined with bandwidth MSA - operators",
       subtitle = "Variation of risetime with operstors",
       x = "Parts",
       y = "Risetime [ps]") + 
  coord_cartesian(ylim = c(500, 1300))+
  geom_hline(yintercept = 600, linetype = "dashed", size = 0.4, color = "red") +
  geom_hline(yintercept = 880, linetype = "dashed", size = 0.4, color = "orange") +
  geom_hline(yintercept = 1100, linetype = "dashed", size = 0.4, color = "blue") +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 15),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        legend.text = element_text(size = 10, color = "black")) + 
  guides(col = guide_legend("Operators"))

#FSOBase::save_my_last_plot()

# Operator to operator

# Variation of risetime vs parts with operators
plot_data <- plot_data %>% 
  dplyr::group_by(oprs)
theme_set(theme_bw(base_size = 18))             
ggplot(plot_data, aes(x = as.factor(part), y = risetime_ps, color = factor(part))) +
  geom_point(size = 5, alpha = 0.9, shape = 19)  +
  facet_wrap(oprs~., ncol = 2) +
  theme_bw() +
  scale_fill_manual(values=c("#56ddc5", "#ff3db7")) +d
  labs(title = "Variation fo risetime from operator to operator",
       subtitle = "",
       x = "Parts",
       y = "Risetime [ps]") + 
  coord_cartesian(ylim = c(500, 1300))+
  geom_hline(yintercept = 600, linetype = "dashed", size = 0.4, color = "red") +
  geom_hline(yintercept = 880, linetype = "dashed", size = 0.4, color = "orange") +
  geom_hline(yintercept = 1100, linetype = "dashed", size = 0.4, color = "blue") +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 15),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        legend.position = "none")  
  #guides(col = guide_legend("Operators"))

#FSOBase::save_my_last_plot()




# Description:     Add an description here

# global options -----------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  # Deactivate auto usage of factors
Sys.setenv(LANGUAGE = 'en')        # Activate english error messages

# load packages ------------------------------------------------------------------------------------
library(magrittr)
library(purrr)
library(tidyr)
library(dplyr)
library(tibble)
library(sitools)
library(stringr)
library(ggplot2)

# source files -------------------------------------------------------------------------------------
source("R_code/source/calc_reflexion.R")
source("R_code/source/support_functions.R")

# setup --------------------------------------------------------------------------------------------

# Wellenlängenbereichen [nm]
lambda    <- seq(800,1000,10)

# Wafer- / Epi-Dicke
target_Si    <- 65e-6
tolerance_Si <- 0

# Dicke Oxied
target_SiO2    <- 15e-9
tolerance_SiO2 <- 5e-9

# Dicke Nitrid
target_Si3N4    <- 98e-9
tolerance_Si3N4 <- 10-9


#Brechzahl der Umgebung: Luft
n_env           <- 1

# Einfallswinkel in [Deg]
angle_of_inc_deg <- c(0,15,30)

# calculate data -----------------------------------------------------------------------------------

thickness_Si    <- calc_parameter(target_Si,    tolerance_Si)
thickness_SiO2  <- calc_parameter(target_SiO2,  tolerance_SiO2)
thickness_Si3N4 <- calc_parameter(target_Si3N4, tolerance_Si3N4)

# setup the parameter of ARC and build all combinantions of them

data <- tidyr::crossing(thickness_Si, 
                        thickness_SiO2 ,
                        thickness_Si3N4, 
                        lambda,
                        n_env, 
                        angle_of_inc_deg) %>% 
  tibble::rownames_to_column(.,var = "id") %>%
  dplyr::group_by(id) %>%
  tidyr::nest() %>%
  dplyr::rename(parameter = data)


# calculate the responsivity for all arc parameter 
data_sabs_raw <- data %>% 
  dplyr::mutate(sabs = purrr::map(.x = parameter,
                                  .f = wrap_calc_sabs))


# unnest data
data_sabs_raw <- data_sabs_raw %>% 
  tidyr::unnest(parameter) %>% 
  dplyr::select(-lambda) %>% 
  tidyr::unnest(sabs)


# get min and max value for each wavelenght
data_sabs_min_max <- data_sabs_raw %>% 
  dplyr::group_by(angle_of_inc_deg) %>% 
  dplyr::summarise(min = min(sabs),
                   max = max(sabs))


# calculate sabs target without tolerances 
data_sabs_main <- data_sabs_raw %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(thickness_Si == target_Si, 
                thickness_SiO2 == target_SiO2, 
                thickness_Si3N4 == target_Si3N4)


# bind columns
data_sabs <- dplyr::right_join(data_sabs_main, data_sabs_min_max, by = "angle_of_inc_deg")


# plot data ----------------------------------------------------------------------------------------

str_sub_title <- bquote(paste("Parameter: Si = ", .(format_si(target_Si,"m")),   " ± ", .(format_si(tolerance_Si,"m")),", ", 
                              SiO[2], " = ", .(format_si(target_SiO2,"m")), " ± ", .(format_si(tolerance_SiO2,"m")), ", ", 
                              Si[3], N[4] , " = ", .(format_si(target_Si3N4,"m"))," ± ", .(format_si(tolerance_Si3N4,"m")), 
                              ", Lambda = ", .(lambda), "nm"))


# data_sabs %>% 
#   ggplot(aes(x = angle_of_inc_deg)) +
#   geom_ribbon(aes(ymin = min, ymax = max), fill = "gray95", alpha = 0.5) + 
#   geom_line(aes(y = max ), color = "darkgray") + 
#   geom_line(aes(y = min ), color = "darkgray") +
#   geom_line(aes(y = sabs), color = "red") +
#   # geom_point(aes(y = sabs), color = "red") +
#   geom_vline(xintercept = asin(c(0.1,0.25,0.45,0.5))*(180/pi), color = "darkgray", linetype = 2) +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(0,90,10), minor_breaks = seq(0,90,5)) +
#   scale_y_continuous(breaks = seq(0,0.8,0.1), minor_breaks = seq(0,0.8,0.05)) +
#   labs(
#     title = "Tolerance Anlayses of ARC vs. Angle",
#     subtitle = str_sub_title,
#     y = "Spectral Responsivity in A/W",
#     x = "Angle in Degree") +
#   annotate(geom = "text", 
#            x = asin(c(0.1,0.25,0.45,0.5))*(180/pi), 
#            y = 0.01, 
#            label = paste("Optics", c("5x", "10x","20x", "50x")),
#            angle = 90,
#            hjust = 0,
#            vjust = -0.5,
#            color = "darkgray",
#            size = 3)



data_sabs %>% 
  ggplot(aes(x = lambda, y = sabs, color = angle_of_inc_deg %>% as.factor())) +
  geom_line()+
  theme_bw()




# global options -----------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  # Deaktivate the auto usage of factors
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
library(ggpmisc)

# source files -------------------------------------------------------------------------------------
source("R_code/source/calc_reflexion.R")
source("R_code/source/support_functions.R")

# setup --------------------------------------------------------------------------------------------

# Wellenlängenbereichen [nm]
lambda <- seq(400,1100,2)

# Wafer- / Epi-Dicke
target_Si    <- 380e-6
tolerance_Si <- 0
sd_Si        <- 0

# Dicke Oxied
target_SiO2    <- 30e-9
tolerance_SiO2 <- 5e-9
sd_SiO2        <- 0

# Dicke Nitrid
target_Si3N4    <- 95e-9
tolerance_Si3N4 <- 10e-9
sd_Si3N4        <- 0

#Brechzahl der Umgebung: Luft
n_env           <- 1

# Einfallswinkel in [Deg] 
angle_of_inc_deg <- 0


# calculate data -----------------------------------------------------------------------------------

thickness_Si <- calc_parameter(target_Si,tolerance_Si, sd_Si)
thickness_SiO2 <- calc_parameter(target_SiO2, tolerance_SiO2, sd_SiO2)
thickness_Si3N4 <- calc_parameter(target_Si3N4,tolerance_Si3N4, sd_Si3N4)



# setup the parameter of ARC and build all combinantions of them
data <- 
  tidyr::crossing(thickness_Si, 
                  thickness_SiO2,
                  thickness_Si3N4, 
                  lambda) %>% 
  dplyr::group_by(thickness_Si, 
                  thickness_SiO2,
                  thickness_Si3N4) %>% 
  tidyr::nest() %>% 
  dplyr::rename(lambda = data) %>% 
  dplyr::mutate(n_env            = n_env, 
                angle_of_inc_deg = angle_of_inc_deg) %>% 
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


# get min and max value for each wavelength
data_arc_min_max <- data_sabs_raw %>% 
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::between(thickness_Si,    target_Si    - tolerance_Si,   target_Si    + tolerance_Si) ,
                dplyr::between(thickness_SiO2,  target_SiO2  - tolerance_SiO2, target_SiO2  + tolerance_SiO2),
                dplyr::between(thickness_Si3N4, target_Si3N4 - tolerance_Si3N4,target_Si3N4 + tolerance_Si3N4)) %>% 
  dplyr::group_by(lambda) %>% 
  dplyr::summarise(min = min(r),
                   max = max(r))



# get min and max  value 1xSD-Range for each wavelength
data_arc_sd_1_min_max <- data_sabs_raw %>% 
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::between(thickness_Si,    target_Si    - sd_Si,   target_Si    + sd_Si) ,
                dplyr::between(thickness_SiO2,  target_SiO2  - sd_SiO2, target_SiO2  + sd_SiO2),
                dplyr::between(thickness_Si3N4, target_Si3N4 - sd_Si3N4,target_Si3N4 + sd_Si3N4)) %>% 
  dplyr::group_by(lambda) %>% 
  dplyr::summarise(sd_1_min = min(r),
                   sd_1_max = max(r))

# get min and max  value 2xSD-Range for each wavelength
data_arc_sd_2_min_max <- data_sabs_raw %>% 
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::between(thickness_Si,    target_Si    - 2*sd_Si,   target_Si    + 2*sd_Si) ,
                dplyr::between(thickness_SiO2,  target_SiO2  - 2*sd_SiO2, target_SiO2  + 2*sd_SiO2),
                dplyr::between(thickness_Si3N4, target_Si3N4 - 2*sd_Si3N4,target_Si3N4 + 2*sd_Si3N4)) %>% 
  dplyr::group_by(lambda) %>% 
  dplyr::summarise(sd_2_min = min(r),
                   sd_2_max = max(r))

# get min and max  value 3xSD-Range for each wavelength
data_arc_sd_3_min_max <- data_sabs_raw %>% 
  dplyr::ungroup() %>%
  dplyr::filter(dplyr::between(thickness_Si,    target_Si    - 3*sd_Si,   target_Si    + 3*sd_Si) ,
                dplyr::between(thickness_SiO2,  target_SiO2  - 3*sd_SiO2, target_SiO2  + 3*sd_SiO2),
                dplyr::between(thickness_Si3N4, target_Si3N4 - 3*sd_Si3N4,target_Si3N4 + 3*sd_Si3N4)) %>% 
  dplyr::group_by(lambda) %>% 
  dplyr::summarise(sd_3_min = min(r),
                   sd_3_max = max(r))

# # calculate sabs target without tolerances 
data_arc_main <-
  data_sabs_raw %>% 
  dplyr::ungroup() %>%
  dplyr::filter(thickness_Si    == target_Si,
                thickness_SiO2  == target_SiO2,
                thickness_Si3N4 == target_Si3N4) %>% 
  dplyr::select(lambda,r)

# bind columns
data_arc <- dplyr::left_join(data_arc_main, data_arc_min_max, by = "lambda")


# plot data ----------------------------------------------------------------------------------------

str_sub_title <- bquote(paste("Parameter:", 
                              SiO[2], " = ", .(format_si(target_SiO2,"m")), " ± ", .(format_si(tolerance_SiO2,"m")), ", ", 
                               Si[3], N[4] , " = ", .(format_si(target_Si3N4,"m"))," ± ", .(format_si(tolerance_Si3N4,"m"))))

data_tab <- data_arc[data_arc$lambda == 1064,] %>% 
  dplyr::rename(target = r) %>% 
  round(digits = 4) %>% 
  dplyr::mutate_all( ~ format(.,scientific = FALSE))
  


data_arc %>% 
  ggplot(aes(x = lambda))+
  geom_ribbon(aes(ymin = min, ymax = max), fill = "gray99", alpha = 0.5) + 
  
  geom_ribbon(data = data_arc_sd_3_min_max, aes(ymin = sd_3_min, ymax = sd_3_max), fill = "gray90", alpha = 0.5) + 
  geom_line(data = data_arc_sd_3_min_max,aes(y = sd_3_max ), color = "gray85", size = 0.1, linetype = 1) + 
  geom_line(data = data_arc_sd_3_min_max,aes(y = sd_3_min ), color = "gray85", size = 0.1, linetype = 1) + 
  
  geom_ribbon(data = data_arc_sd_2_min_max, aes(ymin = sd_2_min, ymax = sd_2_max), fill = "gray85", alpha = 0.5) +
  geom_line(data = data_arc_sd_2_min_max,aes(y = sd_2_max ), color = "gray80", size = 0.1, linetype = 1) + 
  geom_line(data = data_arc_sd_2_min_max,aes(y = sd_2_min ), color = "gray80", size = 0.1, linetype = 1) + 
  
  geom_ribbon(data = data_arc_sd_1_min_max, aes(ymin = sd_1_min, ymax = sd_1_max), fill = "gray80", alpha = 0.5) + 
  geom_line(data = data_arc_sd_1_min_max,aes(y = sd_1_max ), color = "gray75", size = 0.1, linetype = 1) + 
  geom_line(data = data_arc_sd_1_min_max,aes(y = sd_1_min ), color = "gray75", size = 0.1, linetype = 1) + 

  geom_line(aes(y = max ), color = "darkgray", size = 0.1, linetype = 1) + 
  geom_line(aes(y = min ), color = "darkgray", size = 0.1, linetype = 1) +
  geom_line(aes(y = r), color = "red") + 
  # geom_line(aes(y = quantum_eff), color = "blue", linetype = 2) +
  theme_bw() +
  coord_cartesian(xlim = c(400,1100), ylim = c(0,0.6)) +
  scale_x_continuous(breaks = seq(400,1100,100), minor_breaks = seq(400,1100,50)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.05)) +
  annotate(geom = "table", x = 1100, y = 0.6, label = list(data_tab), vjust = 1, hjust = 1) +
  labs(
    title = "Tolerance Anlayses of ARC",
    subtitle = str_sub_title,
    y = "Reflection",
    x = "Wavelength in nm")




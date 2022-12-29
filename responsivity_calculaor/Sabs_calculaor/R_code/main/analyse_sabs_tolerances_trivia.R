

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
library(patchwork)

# source files -------------------------------------------------------------------------------------
source("R_code/source/calc_reflexion.R")
source("R_code/source/support_functions.R")
source("R_code/source/04_load_data_sabs.R")   #NMUT Skript zum Einlesen der Sabs Daten, übernommen load_antest

# setup --------------------------------------------------------------------------------------------

# Wellenlängenbereichen [nm]
lambda <- seq(400,1100,1)

# Wafer- / Epi-Dicke
target_Si    <- 10e-6
tolerance_Si <- 1.5e-6

# Dicke Oxied
target_SiO2    <- 15e-9
tolerance_SiO2 <- 5e-9

# Dicke Nitrid
target_Si3N4    <- 60e-9
tolerance_Si3N4 <- 5e-9

#Brechzahl der Umgebung: Luft
n_env           <- 1

# Einfallswinkel in [Deg] 
angle_of_inc_deg <- 0


# calculate data -----------------------------------------------------------------------------------


calc_parameter <- function(target, tolerance = 0, sd = 0) {
  values <- c(target,seq(target - tolerance,target + tolerance, length.out = 9))
  values <- unique(values)
  return(values)
}


thickness_Si    <- calc_parameter(target_Si, tolerance_Si)
thickness_SiO2  <- calc_parameter(target_SiO2, tolerance_SiO2)
thickness_Si3N4 <- calc_parameter(target_Si3N4, tolerance_Si3N4)



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

system.time({
# calculate the responsivity for all arc parameter 
data_sabs_raw <- data %>% 
  dplyr::mutate(sabs = purrr::map(.x = parameter,
                                  .f = wrap_calc_sabs))
})

# data_sabs_raw %>% saveRDS("data/derived/monte_carlo_simulation.rds")

# unnest data
data_sabs_raw <- data_sabs_raw %>% 
  tidyr::unnest(parameter) %>% 
  dplyr::select(-lambda) %>% 
  tidyr::unnest(sabs)



# get min and max value for each wavelenght
data_sabs_all_min_max <- data_sabs_raw %>% 
  # dplyr::select(-parameter) %>% 
  # tidyr::unnest(sabs) %>% 
  dplyr::group_by(lambda) %>% 
  dplyr::summarise(min = min(sabs),
            max = max(sabs))


# # calculate sabs target without tolerances 
data_sabs_main <-
  data_sabs_raw %>% 
  dplyr::ungroup() %>%
  dplyr::filter(thickness_Si    == target_Si,
                thickness_SiO2  == target_SiO2,
                thickness_Si3N4 == target_Si3N4) %>% 
  dplyr::select(lambda,sabs)

# get min and max value for each wavelength only Si varies 
data_sabs_si_min_max <-  data_sabs_raw %>% 
  dplyr::ungroup() %>%
  dplyr::filter(thickness_SiO2  == target_SiO2,
                thickness_Si3N4 == target_Si3N4) %>% 
  dplyr::select(lambda,sabs) %>% 
  # dplyr::select(-parameter) %>% 
  # tidyr::unnest(sabs) %>% 
  dplyr::group_by(lambda) %>% 
  dplyr::summarise(min = min(sabs),
                   max = max(sabs))

# get min and max value for each wavelength only SiO2 varies
data_sabs_sio2_min_max <-  data_sabs_raw %>% 
  dplyr::ungroup() %>%
  dplyr::filter(thickness_Si    == target_Si,
                thickness_Si3N4 == target_Si3N4) %>% 
  dplyr::select(lambda,sabs) %>% 
  # dplyr::select(-parameter) %>% 
  # tidyr::unnest(sabs) %>% 
  dplyr::group_by(lambda) %>% 
  dplyr::summarise(min = min(sabs),
                   max = max(sabs))

# get min and max value for each wavelength only Si3N4 varies
data_sabs_si3n4_min_max <-  data_sabs_raw %>% 
  dplyr::ungroup() %>%
  dplyr::filter(thickness_Si    == target_Si,
                thickness_SiO2  == target_SiO2) %>% 
  dplyr::select(lambda,sabs) %>% 
  # dplyr::select(-parameter) %>% 
  # tidyr::unnest(sabs) %>% 
  dplyr::group_by(lambda) %>% 
  dplyr::summarise(min = min(sabs),
                   max = max(sabs))


# bind columns
data_sabs <- dplyr::left_join(data_sabs_main, data_sabs_all_min_max, by = "lambda")


# saveRDS(object = data_sabs, file = "data/derived/data_sabs_raw.RDS")

# plot data ----------------------------------------------------------------------------------------

# str_sub_title <- bquote(paste("Parameter: Si = ", .(format_si(target_Si,"m")),   " ± ", .(format_si(tolerance_Si,"m")),", ", 
#                                    SiO[2], " = ", .(format_si(target_SiO2,"m")), " ± ", .(format_si(tolerance_SiO2,"m")), ", ", 
#                              Si[3], N[4] , " = ", .(format_si(target_Si3N4,"m"))," ± ", .(format_si(tolerance_Si3N4,"m"))))
# 
# 
# 
# data_sabs %>% 
#   ggplot(aes(x = lambda))+
#   geom_ribbon(aes(ymin = min, ymax = max), fill = "gray95", alpha = 0.5) + 
#   geom_line(aes(y = max ), color = "darkgray") + 
#   geom_line(aes(y = min ), color = "darkgray") +
#   geom_line(aes(y = sabs), color = "red") + 
#   geom_point(x = 850, y = 0.5, color = "blue") +
#   theme_bw() +
#   coord_cartesian(xlim = c(400,1100), ylim = c(0.1,0.7)) +
#   scale_x_continuous(breaks = seq(400,1100,100), minor_breaks = seq(400,1100,50)) +
#   scale_y_continuous(breaks = seq(0,0.8,0.1), minor_breaks = seq(0,0.8,0.05)) +
#   labs(
#     title = "Tolerance Anlayses of ARC",
#     subtitle = str_sub_title,
#     y = "Spectral Responsivity in A/W",
#     x = "Wavelength in nm")
# 
# 
# data_sabs_raw %>% 
#   filter(lambda == 850) %>% 
#   ggplot(aes(x = sabs))+
#   geom_density() +
#   geom_vline(xintercept = 0.5)



# NMUT: load measured Sabs data --------------------------------------------------------------------

measured_raw_sabs <- read_sabs_files(paths = Sys.glob(file.path("data/raw/sabs_data", "*.csv")))


measured_sabs <- measured_raw_sabs$data %>%
  dplyr::left_join(measured_raw_sabs$header %>% select(article_name, fa_nr, device_nr, header_id), by = "header_id")

# plot patchwork -----------------------------------------------------------------------------------

y_min <- 0.0
y_max <- 0.5
x_min <- 400
x_max <- 1100


# all parameter varies 
plot_all <- ggplot(data = data_sabs_all_min_max, aes(x = lambda)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "gray95", alpha = 0.5) + 
  geom_line(aes(y = max ), color = "darkgray") + 
  geom_line(aes(y = min ), color = "darkgray") +
  geom_line(data = data_sabs_main, aes(y = sabs), color = "red") + 
  geom_point(x = 905, y = 0.58, color = "blue") +
  theme_bw() +
  coord_cartesian(xlim = c(x_min,x_max), ylim = c(y_min,y_max)) +
  scale_x_continuous(breaks = seq(x_min,x_max,100), minor_breaks = seq(x_min,x_max,50)) +
  scale_y_continuous(breaks = seq(y_min,y_max,0.1), minor_breaks = seq(y_min,y_max,0.05)) +
  annotate(geom = "text", 
           x = x_min, 
           y = y_max, 
           vjust = 0.5, 
           hjust = 0,
           label = "all parameter varies") + 
  labs(y = "Spectral Responsivity in A/W",
       x = "Wavelength in nm")


# only Si3N4 varies
plot_Si3N4 <- ggplot(data = data_sabs_si3n4_min_max, aes(x = lambda)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "gray95", alpha = 0.5) + 
  geom_line(aes(y = max ), color = "darkgray") + 
  geom_line(aes(y = min ), color = "darkgray") +
  geom_line(data = data_sabs_main, aes(y = sabs), color = "red") + 
  # geom_point(x = 850, y = 0.5, color = "blue") +
  theme_bw() +
  coord_cartesian(xlim = c(x_min,x_max), ylim = c(y_min,y_max)) +
  scale_x_continuous(breaks = seq(x_min,x_max,100), minor_breaks = seq(x_min,x_max,50)) +
  scale_y_continuous(breaks = seq(y_min,y_max,0.1), minor_breaks = seq(y_min,y_max,0.05)) +
  annotate(geom = "text", 
           x = x_min, 
           y = y_max, 
           vjust = 0.5, 
           hjust = 0,
           label = "only Si3N4 varies") + 
  labs(y = "Spectral Responsivity in A/W",
       x = "Wavelength in nm")

# only Si varies 
plot_Si <- ggplot(data = data_sabs_si_min_max, aes(x = lambda)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "gray95", alpha = 0.5) + 
  geom_line(aes(y = max ), color = "darkgray") + 
  geom_line(aes(y = min ), color = "darkgray") +
  geom_line(data = data_sabs_main, aes(y = sabs), color = "red") + 
  # geom_point(x = 850, y = 0.5, color = "blue") +
  theme_bw() +
  coord_cartesian(xlim = c(x_min,x_max), ylim = c(y_min,y_max)) +
  scale_x_continuous(breaks = seq(x_min,x_max,100), minor_breaks = seq(x_min,x_max,50)) +
  scale_y_continuous(breaks = seq(y_min,y_max,0.1), minor_breaks = seq(y_min,y_max,0.05)) +
  annotate(geom = "text", 
           x = x_min, 
           y = y_max, 
           vjust = 0.5, 
           hjust = 0,
           label = "only Si varies") + 
  labs(y = "Spectral Responsivity in A/W",
       x = "Wavelength in nm")

# only SiO2 varies
plot_SiO2 <- ggplot(data = data_sabs_sio2_min_max, aes(x = lambda)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "gray95", alpha = 0.5) + 
  geom_line(aes(y = max ), color = "darkgray") + 
  geom_line(aes(y = min ), color = "darkgray") +
  geom_line(data = data_sabs_main, aes(y = sabs), color = "red") + 
  # geom_point(x = 850, y = 0.5, color = "blue") +
  theme_bw() +
  coord_cartesian(xlim = c(x_min,x_max), ylim = c(y_min,y_max)) +
  scale_x_continuous(breaks = seq(x_min,x_max,100), minor_breaks = seq(x_min,x_max,50)) +
  scale_y_continuous(breaks = seq(y_min,y_max,0.1), minor_breaks = seq(y_min,y_max,0.05)) +
  annotate(geom = "text", 
           x = x_min, 
           y = y_max, 
           vjust = 0.5, 
           hjust = 0,
           label = "only SiO2 varies") + 
  labs(y = "Spectral Responsivity in A/W",
       x = "Wavelength in nm")


str_sub_title <- paste("Parameter: Si = ", format_si(target_Si,"m"),   "±", format_si(tolerance_Si,"m"),", ", 
                                "SiO2 = ", format_si(target_SiO2,"m"), "±", format_si(tolerance_SiO2,"m"), ", ", 
                               "Si3N4 = ", format_si(target_Si3N4,"m"),"±", format_si(tolerance_Si3N4,"m"))




(plot_all + plot_Si + plot_Si3N4 + plot_SiO2) *
  # geom_line(data = measured_sabs, aes(x = wellenlange, y = empfindlichkeit), colour = "blue") +
  plot_annotation(title = "Tolerance Analysis of ARC",
                  subtitle = str_sub_title)



# peak values --------------------------------------------------------------------------------------

data_sabs_raw %>% 
  group_by(thickness_Si, thickness_SiO2, thickness_Si3N4) %>% 
  slice(which.max(sabs)) %>%
  arrange(lambda) %>% 
  summary()

data_sabs_raw %>% 
  group_by(thickness_Si, thickness_SiO2, thickness_Si3N4) %>% 
  slice(which.max(sabs)) %>%
  arrange(lambda) %>% 
  ggplot(aes(x = lambda)) +
  geom_density() +
  theme_bw()


# 729 

y_min <- 0
y_max <- 1
x_min <- 620
x_max <- 730

data_sabs_raw %>% 
  group_by(thickness_Si, thickness_SiO2, thickness_Si3N4) %>% 
  slice(which.max(sabs)) %>%
  arrange(lambda) %>% 
  ggplot(aes(x = lambda)) +
  stat_ecdf(geom = "line", color = "gray")+
  stat_ecdf(geom = "point") +
  geom_vline(xintercept = c(630,670), color = "red")+
  theme_bw() +
  coord_cartesian(xlim = c(x_min,x_max), ylim = c(y_min,y_max)) +
  scale_x_continuous(breaks = seq(x_min,x_max,10), minor_breaks = seq(x_min,x_max,50)) +
  scale_y_continuous(breaks = seq(y_min,y_max,0.1), minor_breaks = seq(y_min,y_max,0.05)) +
  labs(title = "Empirical Cumulative Distribution of Peak Sensitivity",
       subtitle = str_sub_title,
       y = "",
       x = "Lambda in nm")

FSOBase::save_my_last_plot()


data_sabs_raw %>% 
  group_by(thickness_Si, thickness_SiO2, thickness_Si3N4) %>% 
  slice(which.max(sabs)) %>%
  arrange(lambda) %>% 
  ggplot(aes(x = lambda, y = sabs)) +
  geom_density_2d_filled(alpha = 0.9)+
  geom_density_2d() +
  theme_bw()+
  labs(title = "Densety of Peak Sensitivity",
       subtitle = str_sub_title)
FSOBase::save_my_last_plot()



data_sabs_raw %>% 
  ggplot(aes(x = lambda, y = sabs)) +
  geom_density_2d_filled() +
  geom_line(data = data_sabs_main, aes(y = sabs), color = "red") + 
  theme_bw()

data_sabs_raw %>% 
  ggplot(aes(x = lambda, y = sabs, group = lambda)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_line(data = data_sabs_main, aes(y = sabs), color = "red") + 
  theme_bw()


data_sabs_raw %>% 
  ggplot(aes(x = lambda, y = sabs,  )) +
  geom_line(aes(group = id), color = "gray", alpha = 0.7) +
  geom_line(data = data_sabs_main, aes(y = sabs), color = "red") + 
  theme_bw()


data_sabs_raw %>% 
  group_by(lambda) %>% 
  summarise(q_0 = quantile(sabs,0),
            q_10 = quantile(sabs,0.1),
            q_20 = quantile(sabs,0.2),
            q_30 = quantile(sabs,0.3),
            q_40 = quantile(sabs,0.4),
            q_50 = quantile(sabs,0.5),
            q_60 = quantile(sabs,0.6),
            q_70 = quantile(sabs,0.7),
            q_80 = quantile(sabs,0.8),
            q_90 = quantile(sabs,0.9),
            q_100 = quantile(sabs,1)) %>% 
  tidyr::pivot_longer(-lambda,names_to = "quantile", values_to = "sabs") %>% 
  ggplot(aes(x = lambda, y = sabs)) +
  geom_line(aes(group = quantile), color = "gray") +
  geom_line(data = data_sabs_main, aes(y = sabs), color = "red") + 
  theme_bw()



data_sabs_raw %>% 
  group_by(lambda) %>% 
  summarise(q_0 = quantile(sabs,0),
            q_10 = quantile(sabs,0.1),
            q_20 = quantile(sabs,0.2),
            q_30 = quantile(sabs,0.3),
            q_40 = quantile(sabs,0.4),
            q_50 = quantile(sabs,0.5),
            q_60 = quantile(sabs,0.6),
            q_70 = quantile(sabs,0.7),
            q_80 = quantile(sabs,0.8),
            q_90 = quantile(sabs,0.9),
            q_100 = quantile(sabs,1),
            sd_sabs = sd(sabs)) %>% 
  ggplot(aes(x = lambda)) +
  geom_ribbon(aes(ymin = q_0, ymax = q_100), fill = "gray95", alpha = 0.5) + 
  # geom_line(aes(y = q_0 ), color = "darkgray") + 
  # geom_line(aes(y = q_100 ), color = "darkgray") +
  geom_ribbon(aes(ymin = q_10, ymax = q_90), fill = "gray95", alpha = 0.5) + 
  geom_ribbon(aes(ymin = q_20, ymax = q_80), fill = "gray90", alpha = 0.5) + 
  geom_ribbon(aes(ymin = q_30, ymax = q_70), fill = "gray80", alpha = 0.5) + 
  geom_ribbon(aes(ymin = q_40, ymax = q_60), fill = "gray75", alpha = 0.5) + 
  geom_line(data = data_sabs_main, aes(y = sabs), color = "red") +  
  theme_bw()

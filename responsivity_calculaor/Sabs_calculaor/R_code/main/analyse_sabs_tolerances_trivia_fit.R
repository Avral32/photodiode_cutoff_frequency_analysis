

# global options -----------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  # Deaktivate the auto usage of factors
Sys.setenv(LANGUAGE = 'en')        # Activate english error messages

# load packages ------------------------------------------------------------------------------------
library(magrittr)
library(purrr)
library(tidyr)
library(dplyr)


library(ggplot2)


# source files -------------------------------------------------------------------------------------
source("R_code/source/calc_reflexion.R")
source("R_code/source/support_functions.R")
source("R_code/source/04_load_data_sabs.R")   #NMUT Skript zum Einlesen der Sabs Daten, übernommen load_antest


# load measured Sabs data --------------------------------------------------------------------------
measured_raw_sabs <- read_sabs_files(paths = "data/raw/sabs_data/AD22-12_Ch_400400010633-OR_0_C0013-009.csv")


measured_sabs <- measured_raw_sabs$data %>%
  dplyr::left_join(measured_raw_sabs$header %>% dplyr::select(article_name, fa_nr, device_nr, header_id), by = "header_id")

# calc fit -----------------------------------------------------------------------------------------


# calc_sabs(lambda, thickness_Si, thickness_SiO2, thickness_Si3N4, n_env, angle_of_inc_deg)

model <- nls(empfindlichkeit ~ calc_sabs(wellenlange, 
                               thickness_Si, 
                               thickness_SiO2, 
                               thickness_Si3N4,
                               n_env = 1,
                               angle_of_inc_deg = 0) %>% dplyr::pull("sabs"),
    data = measured_sabs[c("wellenlange","empfindlichkeit")],
    start = list(thickness_Si    = 10e-6, 
                 thickness_SiO2  = 15e-9, 
                 thickness_Si3N4 = 70e-9)#,
    # lower = list(thickness_Si    = 10e-6 - 1.5e-6,
    #              thickness_SiO2  = 15e-9 - 5e-9,
    #              thickness_Si3N4 = 70e-9 - 5e-9),
    # upper = list(thickness_Si    = 10e-6 + 1.5e-6,
    #              thickness_SiO2  = 15e-9 + 5e-9,
    #              thickness_Si3N4 = 70e-9 + 5e+9)
    )
summary(model)
coef(model)

# plot fit -----------------------------------------------------------------------------------------
plot_data <- measured_sabs[c("wellenlange","empfindlichkeit")]
plot_data["model"] <- predict(model)

plot_data %>% 
  ggplot(aes(x = wellenlange)) +
  geom_point(aes(y = empfindlichkeit), color = "darkgray") +
  geom_line(aes(y = model), color = "red") +
  theme_bw()


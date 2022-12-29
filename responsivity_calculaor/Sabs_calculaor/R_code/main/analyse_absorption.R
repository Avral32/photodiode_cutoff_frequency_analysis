

# Description:     Add an description here

# global options -----------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  # Deaktivate the auto usage of factors
Sys.setenv(LANGUAGE = 'en')        # Activate english error messages

# load packages ------------------------------------------------------------------------------------
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# setup --------------------------------------------------------------------------------------------

# Wellenlängenbereichen [nm]
lambda       <- seq(400,1100,1)

#
thickness_Si <- seq(1,10,0.1) %o% 10^(-3:-6) %>% as.vector() %>% unique()

# source files -------------------------------------------------------------------------------------
source("R_code/source/calc_reflexion.R")
source("R_code/source/99_utilities.R")

# support function ---------------------------------------------------------------------------------

wrap_calc_absorption <- function(thickness_Si) {
  calc_absorption(lambda, daten_Si, thickness_Si)
}

# prepare data -------------------------------------------------------------------------------------

absorption <- data.frame(thickness_Si) %>% 
  dplyr::mutate(absorbtion = purrr::map(thickness_Si, wrap_calc_absorption)) %>% 
  tidyr::unnest(cols = absorbtion)


# plot data ----------------------------------------------------------------------------------------


absorption %>% 
  dplyr::filter(lambda %in% c(660,905, 940,1064)) %>% 
  ggplot(aes(x = thickness_Si, y = absorption, linetype = lambda %>% as.factor()))+
  geom_line() +
  # geom_point() + 
  scale_x_log10(minor_breaks = log10_minor_break(), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1,.1))+
  geom_vline(xintercept = c(10e-6,20e-6, 65e-6,380e-6,380e-6*2), 
             linetype = 1, 
             color = "darkgray") +
  annotate(geom = "text", 
           x = c(3.35e-6, 3e-5, 4.7e-5, 9e-4), 
           y = 0.6 , 
           label = c("660 nm","905 nm","940 nm","1064 nm"), 
           angle = 65,
           hjust = 0,
           vjust = -0.1,
           size  = 3,) +
  annotate(geom = "text", 
           x = c(10e-6,20e-6, 65e-6,380e-6,380e-6*2), 
           y = 0 , 
           label = c("Technology: ±12 (10µm)",
                     "Technology: ±6 (20µm)",
                     "Technology: ±9 (65µm)",
                     "Technology: ±Q, ±10 (380µm)",
                     "Technology: ±Q, ±10 mirror (760µm)"), 
           angle = 90,
           hjust = 0,
           vjust = -0.5,
           size  = 3,
           color = "darkgray") +
  theme_bw() +
  guides(linetype = "none") +
  labs(title    = "Absorbtion in Si",
       linetype = "Wavelength",
       x        = "Si Thickness",
       y        = "Absorbtion")




absorption %>% 
  dplyr::filter(lambda  == 905) %>% 
  ggplot(aes(x = thickness_Si, y = absorption, linetype = lambda %>% as.factor()))+
  geom_line(color = "red") +
  # geom_point() + 
  scale_x_log10(minor_breaks = log10_minor_break(), expand = c(0,0)) +
  coord_cartesian(xlim = c(1e-6,1e-3)) +
  scale_y_continuous(breaks = seq(0,1,.1))+
  geom_vline(xintercept = c(10e-6,20e-6, 65e-6,380e-6,380e-6*2), 
             linetype = 1, 
             color = "darkgray") +
  annotate(geom = "text", 
           x = c(10e-6,20e-6, 65e-6,380e-6,380e-6*2), 
           y = 0 , 
           label = c("Technology: ±12 (10µm)",
                     "Technology: ±6 (20µm)",
                     "Technology: ±9 (65µm)",
                     "Technology: ±Q, ±10 (380µm)",
                     "Technology: ±Q, ±10 mirror (760µm)"), 
           angle = 90,
           hjust = 0,
           vjust = -0.5,
           size  = 3,
           color = "darkgray") +
  theme_bw() +
  guides(linetype = "none") +
  labs(title    = "Absorbtion in Si for 905nm",
       x        = "Si Thickness",
       y        = "Absorbtion")



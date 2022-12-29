

# Info:            einlesen der Materialdaten für Si, SiO2 und Si3N4 aus den CSV-Files 
#                  formatieren und interpolieren fehlender Daten
#                  Quelle: https://www.pvlighthouse.com.au/refractive-index-library


# global options -----------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  # Deaktivate the auto usage of factors
Sys.setenv(LANGUAGE = 'en')        # Activate english error messages

# load packages ------------------------------------------------------------------------------------
library(magrittr)
library(tibble)
library(readr)

# support function ---------------------------------------------------------------------------------

# Materialdaten einlesen und unötieg Spalten entfernen
load_material_data <- function(path) {
  daten <- read_csv(file      = path,
                    skip      = 1,
                    col_names = c("lambda", "n", "lambda_k", "k", "alpha",  "reference", "details"),
                    col_types = list(
                      lambda    = col_double(),
                      n         = col_double(),
                      lambda_k  = col_skip(),
                      k         = col_double(),
                      alpha     = col_double(),
                      reference = col_skip(),
                      details   = col_skip())
  )
  return(daten)
}

# Daten so interpolieren das zu jede Wellelänge Daten vorhanden sind
interpolate_daten <- function(daten, lambda){
  n <- spline(x    = daten$lambda,
              y    = daten$n,
              xout = lambda) %>% extract2(2)
  k <- spline(x    = daten$lambda,
              y    = daten$k,
              xout = lambda) %>% extract2(2)
  a <- spline(x    = daten$lambda,
              y    = daten$alpha,
              xout = lambda) %>% extract2(2)
  tibble(lambda, n, k, alpha = a) %>% 
    return()
}

# calculate data frame with refractive index for selected wavelengths
calc_refractive_index <- function(lambda) {
  
  data_ref_in <- list(
    interpolate_daten(daten_Si3N4, lambda) %>% 
      select(c(lambda, n_Si3N4 = n, k_Si3N4 = k)),
    interpolate_daten(daten_SiO2,  lambda) %>% 
      select(c(lambda, n_SiO2  = n, k_SiO2  = k)),
    interpolate_daten(daten_Si,    lambda) %>% 
      select(c(lambda, n_Si    = n, k_Si    = k))
  ) %>% 
    reduce(left_join, by = "lambda") 
  return(data_ref_in)
}

# import data --------------------------------------------------------------------------------------

# Daten aus CSV einlesen
daten_Si    <- load_material_data("data/raw/Si (Crystalline, 300 K [Gre08]).csv")
daten_SiO2  <- load_material_data("data/raw/SiO2 [Rao19].csv")
daten_Si3N4 <- load_material_data("data/raw/Si3N4 (LPCVD [McI14]).csv")





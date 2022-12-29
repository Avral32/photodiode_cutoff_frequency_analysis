
# global options -----------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  # Deaktivate the auto usage of factors
Sys.setenv(LANGUAGE = 'en')        # Activate english error messages

# load packages ------------------------------------------------------------------------------------
library(magrittr)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)

# source files -------------------------------------------------------------------------------------
source("R_code/source/data_preparation.R")

# constants ----------------------------------------------------------------------------------------

# Plancksche Wirkungsquantum [J s]
const_h <- 6.62606896E-34

# Lichtgeschwindigkeit [m/s]
const_c <- 299792458

# Elementarladung [C] 
const_e <- 1.602176487E-19

# support functions --------------------------------------------------------------------------------

#calculate Sabs
calc_sabs <- function(lambda, thickness_Si, thickness_SiO2, thickness_Si3N4, n_env, angle_of_inc_deg) {
  # browser()
  # angle in Rad
  angle_of_inc <- angle_of_inc_deg*pi/180
  
  # Tabelle der Brechzahlen
  data_arc <- calc_refractive_index(lambda) %>% 
    rowwise() %>% 
    mutate(r_te = calc_reflexion_te(n_env, n_Si3N4, k_Si3N4, n_SiO2, k_SiO2, n_Si, k_Si, lambda, angle_of_inc,thickness_Si, thickness_SiO2, thickness_Si3N4),
           r_tm = calc_reflexion_tm(n_env, n_Si3N4, k_Si3N4, n_SiO2, k_SiO2, n_Si, k_Si, lambda, angle_of_inc,thickness_Si, thickness_SiO2, thickness_Si3N4),
           r    = (r_te + r_tm)/2,
           t_te = 1 - r_te,
           t_tm = 1 - r_tm,
           t    = 1 - r) %>% 
    select(c(lambda, r, t,t_te,t_tm))
  
  data_sabs <- list(
    calc_absorption(lambda, daten_Si, thickness_Si),
    calc_quantum_eff(lambda),
    data_arc) %>% 
    reduce(left_join, by = "lambda") %>% 
    mutate(sabs = absorption*quantum_eff * t)
  
  return(data_sabs)
}

# Absorption berechnen nach Lambert-Beerschen Gesetz 
calc_absorption <- function(lambda, daten, thickness){
  # Werte für Wellenlängen interpolieren
  daten  <- interpolate_daten(daten, lambda)
  # Absorption aus Absorbtionskoefizenten [1/cm] und Dicke berechnen
  absorption <- 1 - exp(-daten$alpha * 100 * thickness)
  tibble(lambda, absorption) %>% 
    return()
}

# Quanteneffizients
calc_quantum_eff <- function(lambda){
  quantum_eff <- (const_e * lambda * 10^-9) / (const_h * const_c)
  tibble(lambda, quantum_eff) %>% 
    return()
}

# Y-Parameter TE aus Brechzahl und einfallswinkel berechnen
calc_y_par_te <- function(n, k, winkel) {
  y_par <- (complex(real = n, imaginary = k)  / const_c) * cos(winkel)
  return(y_par)
}

# Y-Parameter TM aus Brechzahl und einfallswinkel berechnen
calc_y_par_tm <- function(n, k, winkel) {
  y_par <- complex(real = n, imaginary = k)  / (const_c * cos(winkel))  
  return(y_par)
}

# Phasenverscheibung berechnen
calc_phase <- function(n ,thickness, winkel, lambda) {
  # browser()
  lambda <- lambda * 1e-9
  phase <- ((2 * pi) / lambda ) * n * thickness * cos(winkel)
  return(phase) 
}

# Liste der Winkel in jeder Schicht
calc_winkel <- function(lambda, n_env, n_Si3N4, n_SiO2, n_Si, angle_of_inc) { 
  
  w_Si3N4 <- asin(n_env   * sin(angle_of_inc) / n_Si3N4)
  w_SiO2  <- asin(n_Si3N4 * sin(w_Si3N4)      / n_SiO2)
  w_Si    <- asin(n_SiO2  * sin(w_SiO2)       / n_Si)
  
  tibble(lambda, angle_of_inc, w_Si3N4, w_SiO2, w_Si) %>% 
    return()
}

# Transfer-Matrix
calc_transfer_m <- function(y_par, phase) {
  # browser()
  m <- matrix(nrow = 2, ncol = 2)
  m[1,1] <- cos(phase)
  m[1,2] <- complex(imaginary = 1 ) * sin(phase) / y_par
  m[2,1] <- complex(imaginary = 1 ) * sin(phase) * y_par
  m[2,2] <- cos(phase)
  return(m)
}

# Reflexion berechnen TE-Fall
calc_reflexion_te <- function(n_env,n_Si3N4,k_Si3N4,n_SiO2,k_SiO2,n_Si, k_Si, lambda, winkel,thickness_Si, thickness_SiO2, thickness_Si3N4) {
  # browser()
  # Winkel je schiht mus hier berechnet und dann jeweils übergeben werden!
  winkel <- calc_winkel(lambda, n_env, n_Si3N4, n_SiO2, n_Si, winkel)
  y_0   <- calc_y_par_te(n_env,0,winkel$angle_of_inc)
  y_1   <- calc_y_par_te(n_Si3N4,k_Si3N4 ,winkel$w_Si3N4)
  p_1   <- calc_phase(n_Si3N4,thickness_Si3N4,winkel$w_Si3N4,lambda)
  m_1   <- calc_transfer_m(y_1, p_1)
  y_2   <- calc_y_par_te(n_SiO2,k_SiO2,winkel$w_SiO2)
  p_2   <- calc_phase(n_SiO2,thickness_SiO2,winkel$w_SiO2,lambda)
  m_2   <- calc_transfer_m(y_2, p_2)
  m     <- m_1 %*% m_2
  y_s   <- calc_y_par_te(n_Si,k_Si,winkel$w_Si)
  
  r = (y_0 * m[1,1] + y_0 * y_s * m[1,2] - m[2,1] - y_s * m[2,2]) / 
    (y_0 * m[1,1] + y_0 * y_s * m[1,2] + m[2,1] + y_s * m[2,2])
  return(Re(r*Conj(r)))
}

# Reflexion berechnen TM-Fall
calc_reflexion_tm <- function(n_env,n_Si3N4,k_Si3N4,n_SiO2,k_SiO2,n_Si, k_Si, lambda, winkel,thickness_Si, thickness_SiO2, thickness_Si3N4) {
  # Winkel je schiht mus hier berechnet und dann jeweils übergeben werden!
  winkel <- calc_winkel(lambda, n_env, n_Si3N4, n_SiO2, n_Si, winkel)
  y_0   <- calc_y_par_tm(n_env,0,winkel$angle_of_inc)
  y_1   <- calc_y_par_tm(n_Si3N4,k_Si3N4 ,winkel$w_Si3N4)
  p_1   <- calc_phase(n_Si3N4,thickness_Si3N4,winkel$w_Si3N4,lambda)
  m_1   <- calc_transfer_m(y_1, p_1)
  y_2   <- calc_y_par_tm(n_SiO2,k_SiO2,winkel$w_SiO2)
  p_2   <- calc_phase(n_SiO2,thickness_SiO2,winkel$w_SiO2,lambda)
  m_2   <- calc_transfer_m(y_2, p_2)
  m     <- m_1 %*% m_2
  y_s   <- calc_y_par_tm(n_Si,k_Si,winkel$w_Si)
  
  r = (y_0 * m[1,1] + y_0 * y_s * m[1,2] - m[2,1] - y_s * m[2,2]) / 
    (y_0 * m[1,1] + y_0 * y_s * m[1,2] + m[2,1] + y_s * m[2,2])
  return(Re(r*Conj(r)))
}
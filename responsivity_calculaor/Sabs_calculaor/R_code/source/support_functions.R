

# Description:     Add an description here

# global options -----------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  # Deactivate auto usage of factors
Sys.setenv(LANGUAGE = 'en')        # Activate english error messages

# load packages ------------------------------------------------------------------------------------
library(magrittr)

# support functions---------------------------------------------------------------------------------

wrap_calc_sabs <- name <- function(parameter) {
  # browser()
  lambda           <- parameter$lambda            %>% unlist() %>% unname()
  thickness_Si     <- parameter$thickness_Si     # 
  thickness_SiO2   <- parameter$thickness_SiO2   # 
  thickness_Si3N4  <- parameter$thickness_Si3N4  # 
  n_env            <- parameter$n_env            # 
  angle_of_inc_deg <- parameter$angle_of_inc_deg # 
  daten            <- calc_sabs(lambda, thickness_Si, thickness_SiO2, thickness_Si3N4, n_env, angle_of_inc_deg)
  
  # daten %>% 
  #   select(lambda,sabs) %>% 
  return(daten[c("lambda","sabs","r")])
}

calc_parameter <- function(target, tolerance = 0, sd = 0) {
  values <- c(target,
              target - tolerance,
              target + tolerance)
  #             target - sd,
  #             target + sd,
  #             target - 2 * sd,
  #             target + 2 * sd,
  #             target - 3 * sd,
  #             target + 3 * sd)
  # values <- c(target,seq(target - tolerance,target + tolerance, length.out = 9))
  
  values <- unique(values)
  return(values)
}

format_eng <- function(x) {
  s <- as.numeric(strsplit(format(x, scientific = T), "e")[[1]])
  return(paste(s[1] * 10 ^ (s[2] %% 3), 
               as.integer(s[2] - (s[2] %% 3)), 
               sep = "e"))
}


# sitools::f2si ohne leehzeichen 
format_si <- function(number, unit = "") {
  sistring <- sitools::f2si(number, unit)
  sistring <- stringr::str_remove_all(string = sistring, pattern = "\\s")
  return(sistring)
}

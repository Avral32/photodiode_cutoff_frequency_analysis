

# Description:     Add an description here

# global options -----------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)  # Deaktivate the auto usage of factors
Sys.setenv(LANGUAGE = 'en')        # Activate english error messages

# support functions --------------------------------------------------------------------------------

# logarimische Hilfslinien in ggplot mit 'scale_x_log10(minor_breaks=log10_minor_break())'
log10_minor_break <- function(...){
  function(x){
    minx         <- floor(min(log10(x), na.rm = T)) - 1
    maxx         <- ceiling(max(log10(x), na.rm = T)) + 1
    n_major      <- maxx - minx + 1
    major_breaks <- seq(minx, maxx, by = 1)
    minor_breaks <- rep(log10(seq(1, 9, by = 1)), times = n_major) +
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}

library(tidyverse)
library(ggplot2)


dir <- "data/raw/spectra/"
file <- "NMUT_RO85_part6_001.CSV"

csv <- read.csv2(paste0(dir, file), header = TRUE, skip = 2, nrow =1000)  %>% 
       select(-X)  %>% 
       mutate(Trace = Trace / 1e6, X1 = X1)  %>%  #-2.94552
       mutate_if(is.numeric , ~round(., 5))

#rang of frquency to find the plateau in MHz
freq_min <- 7
freq_max <- 15


cut_off_freq <- function(dframe, minm, maxm){
    
    min <- which.min(abs(dframe[,1] - minm))
    max <- which.min(abs(dframe[,1] - maxm)) 
    media <- median(dframe[,2][min:max])
    idx <- which(dframe[,2] == media)
    cat("\nMedian in the plateau:", media)
    #cat("Index of the median value in the plateau:", idx)   
    
    # index of cutoff frequency
    idx <- which.min(abs(3-abs(dframe[,2][minm:length(dframe[,1])]-media)))
    idx_real <- min + idx
    cutoff_freq <- dframe[,1][idx_real]
    #cat("\nIndex: ",idx_real)
    cat("\nCut-off frequency: ", cutoff_freq ,"MHz" )
    
    cat("\nRisetime: ", 0.35/cutoff_freq*1e6,"ps" )
    return(list(min, max, media, cutoff_freq))
}

cutoff <- cut_off_freq(csv, freq_min, freq_max)
# change the list to numeric value
cutoff <- as.numeric(unlist(cutoff))
min <- cutoff[1]
max <- cutoff[2]

# # range dataframe to inficate the plateau range
range_y <- csv[,2][min:max]
range_x <- csv[,1][min:max]
range <- data.frame(x = range_x,
                  y = range_y)



theme_set(theme_bw(base_size = 12))

ggplot(data = csv, aes(x = Trace, y = X1)) +
  geom_point(size = 3, alpha = 0.8 , shape = 1) + 
  geom_point(data = range, aes(x=x, y = y), size = 2, color = "blue") +
 scale_x_continuous(trans = 'log10') +
 # x axis
  annotation_logticks(sides = "b",
      alpha = 0.12,
      size = 1,
      short = unit(30,"cm"),
      #mid = unit(3,"mm"),
      long = unit(30,"cm")
      ) +
  
  coord_cartesian(xlim = c(1,1e3), ylim = c(-20,10)) +
  geom_hline(yintercept = cutoff[3], linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = cutoff[4] , linetype = "dashed", color = "black", size = 1) +

  theme_bw() +
  labs(title    = "Series GA0995 - GA1006",
       y        = "Amplitude [dB]",
       x = "Frequency [MHz]") +

theme(plot.title = element_text(color = "black", size = 18, face = "bold"),
         plot.subtitle = element_text(color = "black", size = 15),
         axis.text = element_text(size = 18, color = "black", face="bold"),
        axis.title = element_text(size = 20, face="bold"),
       legend.position = "none" ) +

annotate("text", x = 200, y= -18, label = "-3 dB Cut-off\nFrequency",
           color = "black", size = 6, fontface =2) +

annotate("text", x = 10, y= -10, label = paste0("cutoff_freq: ",cutoff[3], " MHz"),
         color = "black", size = 6, fontface =2) 

#ggsave("GA0995_119V_002.jpg",

#       width = 6.5,
#       height = 5,
#)


# File:   
# 

# INSTALL AND LOAD PACKAGES ################################
#sessionInfo()
#Sys.getlocale()
#git remote add origin https://github.com/l1m40/medals.git
#git branch -M main
#git push -u origin main
#

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
# Packages I load every time; uses "pacman"
pacman::p_load(pacman,tidyverse,rio
               #dplyr,rio,lubridate,
               #ggplot2,grid,gridExtra,           #ggview, # grid.arrange
               #glue,reshape2,tidyverse,  data.table,
               #tidyquant,tseries,    foreach,doParallel
               #,gt
               #, GGally, ggthemes, 
               #ggvis, httr, lubridate, plotly, rmarkdown, shiny, 
               #stringr, tidyr
               #,quantmod,PerformanceAnalytics,lubridate
) 










# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 
# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base
# Clear plots
dev.off()  # But only if there IS a plot
# Clear console
cat("\014")  # ctrl+L

# Clear mind :)


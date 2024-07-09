# Script file:    Plt_TAFE_2024_V.2.3.R
# Title:          Process Eddy Covariance, Sap Flow, and Meteorological Data of a Vineyard Station
# Author:         Bastos Campos, F.
# Date:           06/12/2022
# Updates:        08/03/2023, 30/06/2023, 04/07/2023, 08/07/2024
# Version:        2.3
#
# Description:    
# This script processes the Plantaditsch vineyard station data, including organizing Eddy Covariance, 
# Sap Flow, and Meteorological data to study fluxes dynamics, including water use efficiency,
# and their correlations with meteorological during a period of dry days in 2022.
#
#
# READ ME ------------------------------------------------
# Notes:
# - Define Directories in (1) that correspond to your machine
# - Create the following directories and place the in input_data_dir the data from the repository:
# 
# Bastos Campos, F., Callesen, T., Alberti, G., Montagnani, L., Zanotelli, D., & Tagliavini, M. (2024). 
# Dataset for "Meteorological drivers of vineyard water vapour loss and water use efficiency during dry days" 
# [corrected version] [Data set]. Zenodo. https://doi.org/10.5281/zenodo.12688311
#
# input_data_dir: where the two input data files are (Plt_HW_2022_h.rds and Plt_HW_irrig_2022_d.rds);
# derived_data_dir: where the derived data files will be exported to;
# plots_dir: where the plots will be save.


# 0) Install Required Libraries ------------------------------------------------

cran_libraries <- c("tools", "tibble", "magrittr", "ggplot2", "lubridate", 
                    "dplyr", "utils", "tidyr", "scales", "egg", "corrplot", "RColorBrewer")

install_cran_if_missing <- function(lib) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
    library(lib, character.only = TRUE)
  }
}

invisible(lapply(cran_libraries, install_cran_if_missing))


# 1) Define Directories ---------------------------------------------------------

# Define here all working directories in your system
# input_data_dir: where the two input data files are (Plt_HW_2022_h.rds and Plt_HW_irrig_2022_d.rds);
# derived_data_dir: where the derived data files will be exported to;
# plots_dir: where the plots will be save.

input_data_dir <- "/home/bastosca/Desktop/TAFE_2024/input_data"
derived_data_dir <- "/home/bastosca/Desktop/TAFE_2024/derived_data"
plots_dir <- "/home/bastosca/Desktop/TAFE_2024/plots"


# Set the working directory to input data directory
setwd(input_data_dir)


# 2) Load Libraries ------------------------------------------------------------

library(tools)        # For file_path_sans_ext
library(tibble)       # For tibble
library(magrittr)     # For the piping operator %>%
library(lubridate)    # For date manipulation
library(dplyr)        # For data manipulation
library(utils)        # For utility functions
library(tidyr)        # For data tidying
library(ggplot2)      # For data visualization
library(scales)       # For scaling functions
library(egg)          # For additional ggplot2 functions
library(corrplot)     # For correlation plots
library(RColorBrewer) # For color palettes


# 3) Read and Save Hourly Data --------------------------------------------------------

# Read the data
data_h <- readRDS("Plt_HW_2022_h.rds")

# Set area per vine (square meters)
Ap <- 1.85

# Save processed data
setwd(derived_data_dir)
saveRDS(data_h, file = "Plt_HW_2022_h.rds")
write.csv2(data_h, file = "Plt_HW_2022_h.csv")


# 4) Obtain Daily Data and Include Irrigation --------------------------------------------------------

# Load the data
setwd(input_data_dir)
data_h <- readRDS("Plt_HW_2022_h.rds")

# Compute WUE (Water Use Efficiency)
data_h <- data_h %>% mutate(
  ET_6h_18h = ET,
  GPP_6h_18h = GPP
)

data_h <- data_h %>% mutate_at(
  c("ET_6h_18h", "GPP_6h_18h"), 
  ~ ifelse((hour >= 6 & hour <= 18), ., NA)
)

# Compute daily values
data_h <- data_h %>% mutate(
  Year = year(date),
  month = month(date),
  day = day(date),
  hour = hour(date),
  min = minute(date)
)

data_d <- data_h %>% 
  group_by(Year, month, day) %>% 
  summarise(
    ET = sum(ET, na.rm = TRUE),
    GPP = sum(GPP, na.rm = TRUE),
    Rn = sum(Rn, na.rm = TRUE),
    Precip = sum(Precip, na.rm = TRUE),
    Tair = mean(Tair, na.rm = TRUE),
    VPD = mean(VPD, na.rm = TRUE),
    usoil_10 = mean(usoil_10, na.rm = TRUE),
    ws = mean(ws, na.rm = TRUE),
    
    ET_6h_18h = sum(ET_6h_18h, na.rm = TRUE),
    GPP_6h_18h = sum(GPP_6h_18h, na.rm = TRUE),
    
    T_A = sum(T_A, na.rm = TRUE),
    T_B = sum(T_B, na.rm = TRUE),
    T_C = sum(T_C, na.rm = TRUE),
    T_D = sum(T_D, na.rm = TRUE),
    T_E = sum(T_E, na.rm = TRUE),
    T_F = sum(T_F, na.rm = TRUE),
    T_irr = sum(T_irr, na.rm = TRUE),
    T_rf = sum(T_F, na.rm = TRUE),
    T_vines = sum(T_F, na.rm = TRUE),
    T_A_L = sum(T_A_L, na.rm = TRUE),
    T_B_L = sum(T_B_L, na.rm = TRUE),
    T_C_L = sum(T_C_L, na.rm = TRUE),
    T_D_L = sum(T_D_L, na.rm = TRUE),
    T_E_L = sum(T_E_L, na.rm = TRUE),
    T_F_L = sum(T_F_L, na.rm = TRUE),
    T_irr_L = sum(T_irr_L, na.rm = TRUE),
    T_rf_L = sum(T_rf_L, na.rm = TRUE),
    T_vines_L = sum(T_vines_L, na.rm = TRUE)
  ) %>% as_tibble()

# Compute WUE (Water Use Efficiency)
data_d <- data_d %>% mutate(
  WUE_ET = GPP_6h_18h / ET_6h_18h,    # Beer et al. (2009), Nelson et al. (2018)
  WUE_Tv = GPP_6h_18h / T_irr
)

data_d <- data_d %>% select(-GPP_6h_18h, -ET_6h_18h)

# Rename columns
colnames(data_d) <- c("Year", "month", "day", 
                      "ET", "GPP", "Rn", "Precip", "Tair", "VPD", "usoil_10", "ws",
                      "T_A", "T_B", "T_C", "T_D", "T_E", "T_F",
                      "T_irr", "T_rf", "T_vines",
                      "T_A_L", "T_B_L", "T_C_L", "T_D_L", "T_E_L", "T_F_L",
                      "T_irr_L", "T_rf_L", "T_vines_L", "WUE_ET", "WUE_Tv")

# Replace zero values with NA
list_names <- names(data_d)[!(names(data_d) %in% c("Precip"))]
data_d <- data_d %>% mutate_at(list_names, ~ ifelse(. == 0, NA, .))

# Add date and Day of Year (DoY)
data_d <- data_d %>% mutate(
  date_char = paste((paste(Year, month, day, sep = "-")), (paste("00", "00", "00", sep = ":")), sep = " "),
  date = as.POSIXct(date_char, format = "%Y-%m-%d %H:%M:%S", tz = "CET"),
  DoY = yday(date)
)

# Add irrigation data to the dataset
setwd(input_data_dir)
data_irrig_d <- readRDS("Plt_HW_irrig_2022_d.rds")

# Merge irrigation information
data_d <- data_d %>% left_join(data_irrig_d[, c("DoY", "irrig")], by = "DoY")

# Compute irrigation in mm/day
data_d <- data_d %>% mutate(
  irrig_L = irrig,
  irrig = irrig_L / Ap,
  Precip_L = Ap * Precip
)

# Save the complete dataset
setwd(derived_data_dir)
saveRDS(data_d, file = "Plt_HW_2022_d.rds")
write.csv2(data_d, file = "Plt_HW_2022_d.csv")


# 5) Plots --------------------------------------------------------

# ..... d ..... --------------------------------------------------------

# 1) meteo 1 (in mm/d) --------------------------------------------------------

# Load the HW_dataset
setwd(derived_data_dir)
data_d <- readRDS("Plt_HW_2022_d.rds")

# Rescale variables for plotting
data_d <- data_d %>%
  mutate(
    irrig = irrig / 2,     # Scale for right Y-axis, mm/d
    Precip = Precip / 2,   # Scale for right Y-axis, mm/d
    VPD = (VPD * 10) / 2,  # VPD in hPa, scale for right Y-axis, mm/d
    Tair = Tair / 2        # Scale for right Y-axis, °C
  )

data_d_D <- data_d %>%
  select(date, T_irr, Tair, VPD, Rn, ET, Precip, irrig)

Plt_Tirr_Trf_indT_Tair_irrig <- data_d_D %>%
  pivot_longer(cols = c(Rn, Tair, VPD), names_to = "Variable", values_to = "Value")

lt <- c("solid", "solid", "dotted")
sz <- c(0.60, 0.40, 0.60)

# Create a sequence of vertical lines for midnight times
vertical_lines <- seq(from = as.POSIXct("2022-05-31 00:00:00", tz = "CET"),
                      to = as.POSIXct("2022-08-31 23:59:59", tz = "CET"),
                      by = "2 weeks")

# Bar plot data
Plt_Precip_irrig_d <- data_d %>%
  select(date, Precip, irrig) %>%
  pivot_longer(cols = c(Precip, irrig), names_to = "Variable", values_to = "Value")

my_colors <- c(Precip = "#bebebe", irrig = "blue")
my_labels <- c(expression(Irrigation), expression(Rainfall))

# Create the plot
Plt_meteo_d <- ggplot(data = Plt_Tirr_Trf_indT_Tair_irrig) +
  # Add bars
  geom_bar(data = Plt_Precip_irrig_d, aes(x = date, y = Value, fill = Variable),
           stat = "identity", alpha = 1.0) +
  scale_fill_manual(values = my_colors, labels = my_labels) +
  
  # Add lines
  geom_line(aes(date, Value, color = Variable, size = Variable, linetype = Variable)) +
  scale_color_manual(values = c("darkorange", "darkred", "black")) +
  scale_size_manual(values = sz) +
  scale_linetype_manual(values = lt) +
  
  # Add vertical dashed lines every 2 weeks
  geom_vline(xintercept= as.numeric(vertical_lines), linetype= "dashed", color= "grey")+
  
  scale_y_continuous(expression("Rn"~(M~J~m^-~2~d^-~1)),
                     breaks= seq(0.0, 20.0, 5.0),
                     limits= c(0.0, 23.0),
                     sec.axis= sec_axis(~. * 2,        # Adjust here for scale factor
                                        name= expression(atop("Tair (°C), VPD (hPa),",
                                                              "irr. and rain (mm d"^-~1~")")),
                                        breaks= seq(0.0, 40.0, 10.0)))+
  
  scale_x_datetime(expand = c(0, 0),
                   labels = date_format("%b %d", tz = "CET"),
                   breaks = vertical_lines,       # date_breaks= ("2 week"),
                   limits = as.POSIXct(c(
                     "2022-05-31 12:00:00", "2022-08-31 23:59:00"), 
                     tz = "CET")) +
  
  geom_text(x = as.POSIXct("2022-06-05 04:00:00", tz = "CET"), 
            y = 22.5, size = 7.0, label = "A", parse = FALSE) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5) +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 1, color = "white"),
    axis.text.x = element_text(size = 1, color = "white"),
    axis.text.y = element_text(size = 10),
    axis.line.y.right = element_line(color = "black"),
    axis.line.y.left = element_line(color = "black"),
    legend.title = element_text(color = "white", size = 9.0),
    legend.position = c(0.82, 0.92),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.text = element_text(size = 9.0, colour = "black"),
    legend.key.height = unit(0.65, "line"),
    legend.key.width = unit(1.0, "line"),
    legend.box = "horizontal"
  )

# Plt_meteo_d

# Save the plot
setwd(plots_dir)
ggsave("Plt_2022_Tv_meteo_d.png", plot = Plt_meteo_d, width = 15.0, height = 8.0, units = "cm")
ggsave("Plt_2022_Tv_meteo_d.tiff", plot = Plt_meteo_d, width = 15, height = 8.0, units = "cm", dpi = 350)


# 2) meteo 2 (in mm/d) --------------------------------------------------------

# date, meteo (mm/d)

# load HW_dataset
setwd(derived_data_dir)
data_d <- readRDS("Plt_HW_2022_d.rds")

data_d_D <- data_d[,c("date", "T_irr", "Tair", "VPD", "Rn", "ET", "Precip", "irrig", "usoil_10", "ws")]

# re-scaling irrig for 2nd y axis
data_d_D$ws <- (data_d_D$ws)*2    # scale on the right Y axis, mm/d

Plt_Tirr_Trf_indT_Tair_irrig <- data_d_D %>%
  pivot_longer(c(`ws`, `usoil_10`),
               names_to= "Variable", values_to= "Value")

lt <- c("dashed", "solid")
sz <- c(0.70, 0.70)
my_labels <- c(expression("ws"[EC]),
               expression("usoil"[10]))

# Create a sequence of vertical lines for midnight times
vertical_lines <- seq(from = as.POSIXct("2022-05-31 00:00:00", tz = "CET"),
                      to = as.POSIXct("2022-08-31 23:59:59", tz = "CET"),
                      by = "2 weeks")

# the plot
Plt_meteo_d_2 <- ggplot(data=Plt_Tirr_Trf_indT_Tair_irrig)+
  
  # add lines
  geom_line(aes(date, Value,
                color= Variable, size= Variable, linetype= Variable),
            color= "black")+
  
  #  scale_color_manual(values= c("darkred", "black"))+
  scale_size_manual(values= sz)+
  scale_linetype_manual(values= lt,
                        labels = c(expression(usoil[10]), expression(ws)))+
  
  # Add vertical dashed lines every 2 weeks
  geom_vline(xintercept= as.numeric(vertical_lines), linetype= "dashed", color= "grey")+
  
  scale_y_continuous(expression(usoil[10]~(cm^3~cm^-~3)),
                     breaks=seq(0.0, 20.0, 5.0),
                     limits= c(0.0, 23.0),
                     sec.axis= sec_axis(~(./(2.0)),     # adjust here
                                        name= expression(ws~(m~s^-~1)),
                                        breaks= seq(0.0, 10.0, 2.5)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   breaks = vertical_lines,       # date_breaks= ("2 week"),
                   labels= date_format("%b %d", tz="CET"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2022-05-31 12:00:00", format="%Y-%m-%d %H:%M:%S", tz="CET"), 
                     as.POSIXct("2022-08-31 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="CET"))))+
  
  geom_text(x=as.POSIXct("2022-06-05 04:00:00", tz="CET"), 
            y=22.3, size=7.0, label= "B", parse= F)+
  
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size= 10, color="black"),
        axis.text.x= element_text(size= 1, color = "white"),
        axis.text.y= element_text(size= 10),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.title= element_text(color= "white", size= 9.0),
        legend.position= c(0.85, 0.95),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 10.0, colour= "black"),
        legend.key.height= unit(0.65,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.box="horizontal")+
  guides(linetype= guide_legend(order= 1), size= "none", color= "none")

# Plt_meteo_d_2

# Save the plot
# Plt_meteo_d_save <- ggarrange(Plt_meteo_d, nrow=1, ncol=1)
setwd(plots_dir)
ggsave("Plt_2022_Tv_ws_usoil10_d.png", plot= Plt_meteo_d_2, width= 15.0, height= 8.0, units= "cm")
ggsave("Plt_2022_Tv_ws_usoil10_d.tiff", plot= Plt_meteo_d_2, width= 15.0, height= 8.0, units= "cm", dpi=350)


# 3) WUE --------------------------------------------------------

# load HW_dataset
setwd(derived_data_dir)
data_d <- readRDS("Plt_HW_2022_d.rds")

# correct names
data_d_D <- data_d[,c("date", "T_irr", "Tair", "VPD", "Rn", "ET", "Precip", "WUE_ET", "GPP")]

colnames(data_d_D) <- c("date", "Tv", "Tair", "VPD", "Rn", "ET", "Precip", "WUE", "GPP")

# export for Damiano
data_d_D$"Tv/ET" <- (data_d_D$Tv)/(data_d_D$ET)

data_d_D_export <- data_d_D[,c("date", "Tv/ET", "Tv", "ET", "WUE", "VPD", "Tair", "Precip")]

# re-scaling irrig for 2nd y axis
data_d_D$GPP <- (data_d_D$GPP)*1    # scale on the right Y axis, mm/d

Plt_wue <- data_d_D %>%
  pivot_longer(c(`WUE`, `GPP`),
               names_to= "Variable", values_to= "Value")

lt <- c("solid", "solid")
sz <- c(0.70, 0.70)

# Create a sequence of vertical lines for midnight times
vertical_lines <- seq(from = as.POSIXct("2022-05-31 00:00:00", tz = "CET"),
                      to = as.POSIXct("2022-08-31 23:59:59", tz = "CET"),
                      by = "2 weeks")

# Create a sequence of vertical lines for midnight times
midnight_times <- seq(from = as.POSIXct("2022-05-31 00:00:00", tz = "CET"),
                      to = as.POSIXct("2022-08-31 23:59:59", tz = "CET"),
                      by = "week")

# the plot
Plt_wue <- ggplot(data=Plt_wue)+
  
  # add lines
  geom_line(aes(date, Value,
                color= Variable, size= Variable, linetype= Variable))+
  scale_color_manual(values= c("red", "darkblue"))+
  scale_size_manual(values= sz)+
  scale_linetype_manual(values= lt)+
  
  # Add vertical dashed lines every 2 weeks
  geom_vline(xintercept= as.numeric(vertical_lines), linetype= "dashed", color= "grey")+
  
  scale_y_continuous(expression(WUE~(gC~(kg[H20])^-~1)),
                     breaks=seq(0.0, 20.0, 5.0),
                     limits= c(0.0, 23.0),
                     sec.axis= sec_axis(~(./(1.0)),     # adjust here
                                        name= expression(GPP~(gC~m^-~2~d^-~1)),
                                        breaks= seq(0.0, 20.0, 5.0)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   breaks = vertical_lines,       # date_breaks= ("2 week"),
                   labels= date_format("%b %d", tz="CET"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2022-05-31 12:00:00", format="%Y-%m-%d %H:%M:%S", tz="CET"), 
                     as.POSIXct("2022-08-31 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="CET"))))+
  
  geom_text(x=as.POSIXct("2022-06-05 04:00:00", tz="CET"), 
            y=22.5, size=7.0, label= "C", parse= F)+
  
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size = 10, face = "bold"),
        axis.text.x= element_text(size= 1, color = "white"),
        axis.text.y= element_text(size= 10),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.title= element_text(color= "white", size= 9.0),
        legend.position= c(0.89, 0.94),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 10.0, colour= "black"),
        legend.key.height= unit(0.65,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.box="horizontal")

# Plt_wue

# Save the plot
setwd(plots_dir)
ggsave("Plt_2022_WUE.png", plot= Plt_wue, width= 15.0, height= 8.0, units= "cm")
ggsave("Plt_2022_WUE.tiff", plot= Plt_wue, width= 15.0, height= 8.0, units= "cm", dpi=350)


# 4) plot  (in mm/d) --------------------------------------------------------

# date, T_irr, ET (mm/d)

# load HW_dataset
setwd(derived_data_dir)
data_d <- readRDS("Plt_HW_2022_d.rds")

# re-scaling irrig for 2nd y axis
data_d$irrig <- (data_d$irrig)/6    # scale on the right Y axis, mm/d
data_d$Precip <- (data_d$Precip)/6  # scale on the right Y axis, mm/d
data_d$VPD <- (data_d$VPD)*10       # VPD in hPa
data_d$VPD <- (data_d$VPD)/6        # scale on the right Y axis, mm/d
data_d$Tair <- (data_d$Tair)/6      # scale on the right Y axis, °C
data_d$GPP <- (data_d$GPP)/4.25     # scale on the right Y axis, gC/m²d


data_d_D <- data_d[,c("date", "T_irr", "Tair", "VPD", "ET", "GPP", "Precip", "irrig")]

colnames(data_d_D) <- c("date", "Tv", "Tair", "VPD", "ET", "GPP", "Precip", "irrig")

Plt_Tirr_Trf_indT_Tair_irrig <- data_d_D %>%
  pivot_longer(c(`Tv`, `ET`),
               names_to= "Variable", values_to= "Value")

lt <- c("solid", "solid")
sz <- c(0.60, 0.60)

# Create a sequence of vertical lines for midnight times
vertical_lines <- seq(from = as.POSIXct("2022-05-31 00:00:00", tz = "CET"),
                      to = as.POSIXct("2022-08-31 23:59:59", tz = "CET"),
                      by = "2 weeks")

# the plot
Plt_Tv_ET_d <- ggplot(data=Plt_Tirr_Trf_indT_Tair_irrig)+
  
  # add lines
  geom_line(aes(date, Value,
                color= Variable, size= Variable, linetype= Variable))+
  scale_color_manual(values= c("blue", "darkgreen"))+
  scale_size_manual(values= sz)+
  scale_linetype_manual(values= lt)+
  
  # Add vertical dashed lines every 2 weeks
  geom_vline(xintercept= as.numeric(vertical_lines), linetype= "dashed", color= "grey")+
  
  scale_y_continuous(expression("Tv or  ET"~(mm~d^-~1)),
                     breaks=seq(0.0, 6.0, 2.0),
                     limits= c(0.0, 6.1),
                     sec.axis= sec_axis(~(./(1/10)),     # adjust here
                                        name= expression(),
                                        breaks= seq(-1000.0, 1000, 5000)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   breaks = vertical_lines,       # date_breaks= ("2 week"),
                   labels= date_format("%b %d", tz="CET"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2022-05-31 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="CET"), 
                     as.POSIXct("2022-08-31 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="CET"))))+
  
  geom_text(x=as.POSIXct("2022-06-05 04:00:00", tz="CET"), 
            y=6.0, size=7.0, label= "D", parse= F)+
  
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size= 10, angle= 45, hjust= 1, color="black"),
        axis.text.x= element_text(size= 10, angle= 45, hjust= 1),
        axis.text.y= element_text(size= 10),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.title= element_text(color= "white", size= 9.0),
        legend.position= c(0.90, 0.95),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 10.0, colour= "black"),
        legend.key.height= unit(0.65,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.box="horizontal")

# Plt_Tv_ET_d

# Save the plot
setwd(plots_dir)
ggsave("Plt_2022_Tv_ET_d.png", plot= Plt_Tv_ET_d, width= 15.0, height= 8.0, units= "cm")
ggsave("Plt_2022_Tv_ET_d.tiff", plot= Plt_Tv_ET_d, width= 15, height= 8.0, units= "cm", dpi=350)


# Arrange the panel --------------------------------------------------------

Plt_Tv_ET_meteo_d <- ggarrange(Plt_meteo_d, Plt_meteo_d_2, Plt_wue, Plt_Tv_ET_d,
                               heights= c(0.5, 0.5, 0.5, 0.5), 
                               widths= c(1.0),
                               nrow=4, ncol=1)

# Save the plot
setwd(plots_dir)
ggsave("Plt_2022_Tv_ET_GPP_meteo_d.png", plot= Plt_Tv_ET_meteo_d, width= 17.0, height= 20.0, units= "cm")
ggsave("Plt_2022_Tv_ET_GPP_meteo_d.tiff", plot= Plt_Tv_ET_meteo_d, width= 17, height= 20.0, units= "cm", dpi=350)


# ..... h ..... --------------------------------------------------------

# 5) plot v.5 (in mm/h) --------------------------------------------------------

# date, meteo (mm/h)

# load HW_dataset
setwd(derived_data_dir)
data_h <- readRDS("Plt_HW_2022_h.rds")

data_h$VPD <- (data_h$VPD)*10        # VPD in hPa
data_h$VPD <- (data_h$VPD)/10        # scale on the right Y axis, mm/d
data_h$Tair <- (data_h$Tair)/10      # scale on the right Y axis, °C

data_h_D <- data_h[,c("date", "T_irr", "Tair", "VPD", "Rn", "ET", "Precip")]

data_h_D <- data_h_D %>%
  pivot_longer(c(`Rn`, `Tair`, `VPD`),
               names_to= "Variable", values_to= "Value")

lt <- c("solid", "solid", "dotted")
sz <- c(0.60, 0.40, 0.60)

# for the barplot
Plt_Precip_irrig_h <- data_h[,c("date", "Precip")] %>%
  pivot_longer(c(`Precip`), names_to= "Variable", values_to= "Value")

my_colors <- c(Precip= "blue")
my_labels <- c(expression(Rainfall))

# Create a sequence of vertical lines for midnight times
midnight_times <- seq(from = as.POSIXct("2022-07-15 00:00:00", tz = "CET"),
                      to = as.POSIXct("2022-07-24 23:59:59", tz = "CET"),
                      by = "day")

# the plot
Plt_meteo_h <- ggplot(data=data_h_D)+
  # add bars
  geom_bar(data= Plt_Precip_irrig_h, aes(x=date, y= Value, fill= Variable),
           stat="identity", alpha=1.0)+
  scale_fill_manual(values= my_colors, labels= my_labels)+
  
  # add lines
  geom_line(aes(date, Value,
                color= Variable, size= Variable, linetype= Variable))+
  scale_color_manual(values= c("darkorange", "darkred", "black"))+
  scale_size_manual(values= sz)+
  scale_linetype_manual(values= lt)+
  
  # Add vertical dashed lines at midnight
  geom_vline(xintercept= as.numeric(midnight_times), linetype= "dashed", color= "grey")+
  
  scale_y_continuous(expression("Rn"~(MJ~m^-~2~h^-~1)),
                     breaks=seq(0.0, 8.0, 2.0),
                     limits= c(-0.34, 5.5),
                     sec.axis= sec_axis(~(./(1/10)),        # adjust here for scale factor
                                        name= expression(paste(atop("Tair (°C), VPD (hPa),", 
                                                                    "Rainfall (mm h"~-~1~")"))),    # is ok???
                                        breaks= seq(0.0, 55.0, 20.0)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("1 day"),
                   labels= date_format("%b %d", tz="CET"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2022-07-15 01:00:00", format="%Y-%m-%d %H:%M:%S", tz="CET"), 
                     as.POSIXct("2022-07-24 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="CET"))))+
  
  geom_text(x=as.POSIXct("2022-07-15 12:00:00", tz="CET"), 
            y=5.3, size=7.0, label= "A", parse= F)+
  
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size= 10, color="black"),
        axis.text.x= element_text(size= 1, color = "white"),
        axis.text.y= element_text(size= 10),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.title= element_text(color= "white", size= 9.0),
        legend.position= c(0.58, 0.94),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 10.0, colour= "black"),
        legend.key.height= unit(0.65,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.direction = "horizontal",
        legend.box="horizontal")

# Plt_meteo_h

# Save the plot
setwd(plots_dir)
ggsave("Plt_2022_Tv_meteo_h.png", plot= Plt_meteo_h, width= 15.0, height= 8.0, units= "cm")
ggsave("Plt_2022_Tv_meteo_h.tiff", plot= Plt_meteo_h, width= 15, height= 8.0, units= "cm", dpi=350)


# 6) plot (ws and usoil) --------------------------------------------------------

# date, meteo 2 

# load HW_dataset
setwd(derived_data_dir)
data_h <- readRDS("Plt_HW_2022_h.rds")

data_h$ws <- (data_h$ws)*2      # scale on the right Y axis

# rename usoil_10
names(data_h)[names(data_h)=="usoil_10"] <- "usoil10"

data_h_D <- data_h[,c("date", "T_irr", "Tair", "ws", "usoil10", "ET", "Precip")]

data_h_D <- data_h_D %>%
  pivot_longer(c(`ws`, `usoil10`),
               names_to= "Variable", values_to= "Value")

lt <- c("dashed", "solid")
sz <- c(0.60, 0.50)

# Create a sequence of vertical lines for midnight times
midnight_times <- seq(from = as.POSIXct("2022-07-15 00:00:00", tz = "CET"),
                      to = as.POSIXct("2022-07-24 23:59:59", tz = "CET"),
                      by = "day")

# the plot
Plt_meteo_h_2 <- ggplot(data=data_h_D)+
  
  # add lines
  geom_line(aes(date, Value,
                size= Variable, linetype= Variable),
            color= "black")+
  # scale_color_manual(values= c("darkred", "black"))+
  scale_size_manual(values= sz)+
  scale_linetype_manual(values= lt,
                        labels = c(expression(usoil[10]), expression(ws)))+
  
  # Add vertical dashed lines at midnight
  geom_vline(xintercept= as.numeric(midnight_times), linetype= "dashed", color= "grey")+
  
  scale_y_continuous(expression(usoil[10]~("% vol.")),
                     breaks=seq(0.0, 30.0, 10.0),
                     limits= c(0.0, 30.0),
                     sec.axis= sec_axis(~(./(2.0)),      # adjust here for scale factor
                                        name= expression(ws~(m~s^-~1)),
                                        breaks= seq(0.0, 15.0, 5.0)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("1 day"),
                   labels= date_format("%b %d", tz="CET"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2022-07-15 01:00:00", format="%Y-%m-%d %H:%M:%S", tz="CET"), 
                     as.POSIXct("2022-07-24 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="CET"))))+
  
  geom_text(x=as.POSIXct("2022-07-15 12:00:00", tz="CET"), 
            y=29.0, size=7.0, label= "B", parse= F)+
  
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size= 10, color="black"),
        axis.text.x= element_text(size= 1, color = "white"),
        axis.text.y= element_text(size= 10),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.title= element_text(color= "white", size= 9.0),
        legend.position= c(0.87, 0.96),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 10.0, colour= "black"),
        legend.key.height= unit(0.65,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.box="horizontal")+
  guides(linetype= guide_legend(order= 1), size= "none", color= "none")


# Plt_meteo_h_2

# Save the plot
setwd(plots_dir)
ggsave("Plt_2022_ws_usoil10_h.png", plot= Plt_meteo_h_2, width= 15.0, height= 8.0, units= "cm")
ggsave("Plt_2022_ws_usoil10_h.tiff", plot= Plt_meteo_h_2, width= 15.0, height= 8.0, units= "cm", dpi=350)


# 7)  plot (in mm/h) --------------------------------------------------------

# date, T_irr, ET (mm/h)

# load HW_dataset
setwd(derived_data_dir)
data_h <- readRDS("Plt_HW_2022_h.rds")

data_h$VPD <- (data_h$VPD)*10        # VPD in hPa
data_h$VPD <- (data_h$VPD)/10        # scale on the right Y axis, mm/d
data_h$Tair <- (data_h$Tair)/10      # scale on the right Y axis, °C

data_h$"Tv/ET" <- (data_h$T_irr)/(data_h$ET)

data_h_D <- data_h[,c("date", "Tv/ET", "Tair", "VPD", "ET", "Precip")]

Plt_Tv_ET_h <- data_h_D %>%
  pivot_longer(c(`Tv/ET`),
               names_to= "Variable", values_to= "Value")

lt <- c("solid")
sz <- c(0.60)

# Create a sequence of vertical lines for midnight times
midnight_times <- seq(from = as.POSIXct("2022-07-15 00:00:00", tz = "CET"),
                      to = as.POSIXct("2022-07-24 23:59:59", tz = "CET"),
                      by = "day")

# the plot
Plt_Tv_ET_ratio <- ggplot(data=Plt_Tv_ET_h)+
  
  # add lines
  geom_line(aes(date, Value,
                color= Variable, size= Variable, linetype= Variable))+
  scale_color_manual(values= c("darkblue"))+
  scale_size_manual(values= sz)+
  scale_linetype_manual(values= lt)+
  
  # Add vertical dashed lines at midnight
  geom_vline(xintercept= as.numeric(midnight_times), linetype= "dashed", color= "grey")+
  
  scale_y_continuous(expression("Tv / ET"),
                     breaks=seq(0.0, 1.0, 0.25),
                     limits= c(0.0, 1.05),
                     sec.axis= sec_axis(~(./(1/10)),     # adjust here
                                        name= expression(),
                                        breaks= seq(-1000.0, 1000, 5000)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("day"),  # ("month"),
                   labels= date_format("%b %d", tz="CET"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2022-07-15 01:00:00", format="%Y-%m-%d %H:%M:%S", tz="CET"), 
                     as.POSIXct("2022-07-24 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="CET"))))+
  
  geom_text(x=as.POSIXct("2022-07-15 12:00:00", tz="CET"), 
            y=1.0, size=7.0, label= "C", parse= F)+
  
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size= 10, color="black"),
        axis.text.x= element_text(size= 1, color = "white"),
        axis.text.y= element_text(size= 10),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.title= element_text(color= "white", size= 9.0),
        legend.position= c(0.90, 0.99),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 10.0, colour= "black"),
        legend.key.height= unit(0.65,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.box="horizontal")

# Plt_Tv_ET_ratio

# Save the plot
setwd(plots_dir)
ggsave("Plt_2022_Tv_ET_ratio_h.png", plot= Plt_Tv_ET_ratio, width= 15.0, height= 8.0, units= "cm")
ggsave("Plt_2022_Tv_ET_ratio_h.tiff", plot= Plt_Tv_ET_ratio, width= 15, height= 8.0, units= "cm", dpi=350)


# 8) WUE (in gC kg−1 H2O) --------------------------------------------------------

# load HW_dataset
setwd(derived_data_dir)
data_h <- readRDS("Plt_HW_2022_h.rds")
data_h <- data_h %>% mutate(hour=hour(date))          

data_h_D <- data_h[,c("date", "T_irr", "Tair", "VPD", "ET", "WUE_ET", "GPP")]

colnames(data_h_D) <- c("date", "Tv", "Tair", "VPD", "ET", "WUE", "GPP")

# substitute -Inf/Inf values by NA
is.na(data_h_D) <- sapply(data_h_D, is.infinite)

# re-scale for plotting
data_h_D$GPP <- (data_h_D$GPP)*3       # scale on the right Y axis, gC/m²d

Plt_Tv_ET_h <- data_h_D %>%
  pivot_longer(c(`WUE`, `GPP`),
               names_to= "Variable", values_to= "Value")

lt <- c("solid", "solid")
sz <- c(0.60, 0.60)

# Create a sequence of vertical lines for midnight times
midnight_times <- seq(from = as.POSIXct("2022-07-15 00:00:00", tz = "CET"),
                      to = as.POSIXct("2022-07-24 23:59:59", tz = "CET"),
                      by = "day")

# the plot
Plt_WUE_h <- ggplot(data=Plt_Tv_ET_h)+
  
  # add lines
  geom_line(aes(date, Value,
                color= Variable, size= Variable, linetype= Variable))+
  scale_color_manual(values= c("red", "darkblue"))+
  scale_size_manual(values= sz)+
  scale_linetype_manual(values= lt)+
  
  # Add vertical dashed lines at midnight
  geom_vline(xintercept= as.numeric(midnight_times), linetype= "dashed", color= "grey")+
  
  scale_y_continuous(expression(WUE~(gC~(kg[H20])^-~1)),
                     breaks=seq(0.0, 8.0, 2.5),
                     limits= c(-2.5, 8.0),
                     sec.axis= sec_axis(~(./(3)),        # adjust here for the scale factor
                                        name= expression(GPP~(gC~m^-~2~d^-~1)),
                                        breaks= seq(-0.5, 2.5, 1.0)))+
  
  scale_x_datetime(expand= c(0, 0),
                   expression(),
                   date_breaks= ("day"),
                   labels= date_format("%b %d", tz="CET"),
                   limits= as.POSIXct(c(
                     as.POSIXct("2022-07-15 01:00:00", format="%Y-%m-%d %H:%M:%S", tz="CET"), 
                     as.POSIXct("2022-07-24 23:59:00", format="%Y-%m-%d %H:%M:%S", tz="CET"))))+
  
  geom_text(x=as.POSIXct("2022-07-15 12:00:00", tz="CET"), 
            y=7.7, size=7.0, label= "D", parse= F)+
  
  geom_hline(yintercept=0, linetype="dashed", color= "darkgrey", size= 0.5)+
  theme(plot.background= element_rect(fill= "white"),
        panel.background= element_rect(fill= "white"),
        axis.title= element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size= 10, color="black"),
        axis.text.x= element_text(size= 1, color = "white"),
        axis.text.y= element_text(size= 10),
        axis.line.y.right= element_line(color= "black"),
        axis.line.y.left= element_line(color= "black"),
        legend.title= element_text(color= "white", size= 9.0),
        legend.position= c(0.89, 0.98),
        legend.key= element_rect(fill= "transparent", colour= "transparent"),
        legend.text= element_text(size= 10.0, colour= "black"),
        legend.key.height= unit(0.65,"line"),
        legend.key.width= unit(1.0,"line"),
        legend.box="horizontal")

# Plt_WUE_h

# Save the Plot
setwd(plots_dir)
ggsave("Plt_2022_WUE_h.png", plot= Plt_WUE_h, width= 15.0, height= 8.0, units= "cm")
ggsave("Plt_2022_WUE_h.tiff", plot= Plt_WUE_h, width= 15, height= 8.0, units= "cm", dpi=350)


# 9) plot (in mm/h) --------------------------------------------------------

# date, T_irr, ET (mm/h)

# Load HW_dataset
setwd(derived_data_dir)
data_h <- readRDS("Plt_HW_2022_h.rds")

data_h$VPD <- (data_h$VPD) * 10       # VPD in hPa
data_h$VPD <- (data_h$VPD) / 10       # scale on the right Y axis, mm/d
data_h$Tair <- (data_h$Tair) / 10     # scale on the right Y axis, °C
data_h$GPP <- (data_h$GPP) / 4        # scale on the right Y axis, gC/m²d

data_h_D <- data_h[, c("date", "T_irr", "Tair", "VPD", "ET", "Precip", "GPP")]
colnames(data_h_D) <- c("date", "Tv", "Tair", "VPD", "ET", "Precip", "GPP")

Plt_Tv_ET_h <- data_h_D %>%
  pivot_longer(c(`Tv`, `ET`),
               names_to = "Variable", values_to = "Value")

lt <- c("solid", "solid")
sz <- c(0.60, 0.60)

# Create a sequence of vertical lines for midnight times
midnight_times <- seq(from = as.POSIXct("2022-07-15 00:00:00", tz = "CET"),
                      to = as.POSIXct("2022-07-24 23:59:59", tz = "CET"),
                      by = "day")

# The plot
Plt_Tv_ET_h <- ggplot(data = Plt_Tv_ET_h) +
  
  # Add lines
  geom_line(aes(date, Value, color = Variable, size = Variable, linetype = Variable)) +
  scale_color_manual(values = c("blue", "darkgreen")) +
  scale_size_manual(values = sz) +
  scale_linetype_manual(values = lt) +
  
  # Add vertical dashed lines at midnight
  geom_vline(xintercept= as.numeric(midnight_times), linetype= "dashed", color= "grey")+
  
  scale_y_continuous(expression("Tv or ET" ~ (mm ~ h^-~1)),
                     breaks = seq(0.0, 0.65, 0.2),
                     limits = c(0.0, 0.70),
                     sec.axis = sec_axis(~(./(1/10)),
                                         name = expression(),
                                         breaks = seq(-1000.0, 1000, 5000))) +
  
  scale_x_datetime(expand = c(0, 0),
                   expression(),
                   date_breaks = "day",
                   labels = date_format("%b %d", tz = "CET"),
                   limits = as.POSIXct(c(
                     as.POSIXct("2022-07-15 01:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "CET"), 
                     as.POSIXct("2022-07-24 23:59:00", format = "%Y-%m-%d %H:%M:%S", tz = "CET")))) +
  
  geom_text(x = as.POSIXct("2022-07-15 12:00:00", tz = "CET"), 
            y = 0.65, size = 7.0, label = "E", parse = FALSE) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", size = 0.5) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.title= element_text(size = 10, face = "bold"),
        axis.title.x= element_text(size = 12, color = "black"),
        axis.text.x= element_text(size= 10, angle= 45, hjust= 1),
        axis.text.y= element_text(size = 10),
        axis.line.y.right= element_line(color = "black"),
        axis.line.y.left= element_line(color = "black"),
        legend.title= element_text(color = "white", size = 10),
        legend.position = c(0.89, 0.97),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text = element_text(size = 10, colour = "black"),
        legend.key.height = unit(0.65, "line"),
        legend.key.width = unit(1.0, "line"),
        legend.box = "horizontal")

# Plt_Tv_ET_h

# Save the Plot
setwd(plots_dir)
ggsave("Plt_2022_Tv_ET_h.png", plot= Plt_Tv_ET_h, width= 15.0, height= 8.0, units= "cm")
ggsave("Plt_2022_Tv_ET_h.tiff", plot= Plt_Tv_ET_h, width= 15, height= 8.0, units= "cm", dpi=350)


# Arrange the panel --------------------------------------------------------

Plt_Tv_ET_meteo_ratio_h <- ggarrange(Plt_meteo_h, Plt_meteo_h_2,
                                     Plt_Tv_ET_ratio, 
                                     Plt_WUE_h, Plt_Tv_ET_h,
                                     heights= c(0.5, 0.5, 0.5, 0.5, 0.5), 
                                     widths= c(1.0),
                                     nrow=5, ncol=1)

# Save the plot
setwd(plots_dir)
ggsave("Plt_2022_Tv_ET_WUE_meteo_h.png", plot= Plt_Tv_ET_meteo_ratio_h, width= 18.0, height= 23.5, units= "cm")
ggsave("Plt_2022_Tv_ET_WUE_meteo_h.tiff", plot= Plt_Tv_ET_meteo_ratio_h, width= 18.0, height= 23.5, units= "cm", dpi=350)


# ............... --------------------------------------------------------

# Correlation Plots --------------------------------------------------------

# .... R2 (h)..... --------------------------------------------------------

# (mm/h)

# Load HW_dataset
setwd(derived_data_dir)
data_h <- readRDS("Plt_HW_2022_h.rds")

# Filter data for the period of study
data_h <- data_h %>% filter(date >= as.POSIXct("2022-07-16 00:00:00", tz="CET") & 
                              date <= as.POSIXct("2022-07-24 23:30:00", tz="CET")) 

# Select relevant columns and rename
data <- data_h[, c("T_irr", "ET", "Tair", "VPD", "Rn", "usoil_10", "ws", "GPP", "WUE_ET")]
colnames(data) <- c("Tv", "ET", "Tair", "VPD", "Rn", "usoil10", "ws", "GPP", "WUE")

# Calculate Tv/ET ratio
data$"Tv/ET" <- (data$Tv) / (data$ET)

# Correlation plot for mm/h data

# Replace -Inf/Inf values by NA
data_corr <- data
is.na(data_corr) <- sapply(data_corr, is.infinite)

# Remove rows with NA values
data_corr <- data_corr[complete.cases(data_corr),]

# Define LaTeX-style variable names with bold font
var_names <- c(expression(bold(Tv)),
               expression(bold(ET)),
               expression(bold(Tair)),
               expression(bold(VPD)),
               expression(bold(Rn)),
               expression(bold(usoil[10])),
               expression(bold(ws)),
               expression(bold(GPP)),
               expression(bold(WUE)),
               expression(bold(Tv/ET)))

var_names_x <- var_names[2:10]

# Compute correlation matrix
M <- cor(data_corr)

# Perform significance test
testRes <- cor.mtest(data_corr, conf.level = 0.95)

# Define color palette from red to blue
color_palette <- colorRampPalette(c("red", "white", "blue"))(100)

setwd(plots_dir)
tiff("corrplot_h.tiff", width = 15, height = 15, units = 'cm', res = 350)

# Plot correlations without text labels
corrplot(M,
         p.mat = testRes$p,
         method = 'ellipse', 
         type = 'upper', 
         insig = 'blank',
         addCoef.col = 'black', 
         number.cex = 0.8,          # Adjust font size of correlation coefficients
         order = 'original', 
         diag = FALSE,
         tl.pos = 'n',              # Suppress default text labels
         cl.cex = 1.0, 
         col = color_palette, 
         title = 'Hourly-scale',
         mar = c(3, 3, 3, 3),       # Adjust margins to prevent clipping of labels
         pch.col = 'black', 
         pch.cex = 1, 
         pch.cor = testRes$p,
         lower = color_palette, 
         upper = color_palette
)

# Get plot coordinates
n <- ncol(M)
coords <- seq(1, n, by = 1)

# Add custom variable names using text function with smaller font size
for (i in 1:n) {           # 9 column names to be displayed
  # Add variable names on the upper row
  text(i + 1.2, n + 1.2, labels = var_names_x[i], srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  # Add variable names on the left column
  text(-0.5, n - i + 1, labels = var_names[i], adj = 1, xpd = TRUE, cex = 0.8)
  # Add variable names on the diagonal closer to the plot
  text(i + 0.2, n - i + 1 + 0.2, labels = var_names[i], srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
}

dev.off()


# .... R2 (d)..... --------------------------------------------------------

# (mm/d)

# Load HW_dataset for daily data
setwd(derived_data_dir)
data_d <- readRDS("Plt_HW_2022_d.rds")

# Filter data for the period of study
data_d <- data_d %>% filter(date >= as.POSIXct("2022-07-16 00:00:00", tz="CET") & 
                              date <= as.POSIXct("2022-07-24 23:30:00", tz="CET")) 

# Select relevant columns and rename
data <- data_d[, c("T_irr", "ET", "Tair", "VPD", "Rn", "usoil_10", "ws", "GPP", "WUE_ET")]
colnames(data) <- c("Tv", "ET", "Tair", "VPD", "Rn", "usoil10", "ws", "GPP", "WUE")

# Calculate Tv/ET ratio
data$"Tv/ET" <- (data$Tv) / (data$ET)

# Replace -Inf/Inf values by NA
data_corr <- data
is.na(data_corr) <- sapply(data_corr, is.infinite)

# Remove rows with NA values
data_corr <- data_corr[complete.cases(data_corr),]

# Define LaTeX-style variable names with bold font
var_names <- c(expression(bold(Tv)),
               expression(bold(ET)),
               expression(bold(Tair)),
               expression(bold(VPD)),
               expression(bold(Rn)),
               expression(bold(usoil[10])),
               expression(bold(ws)),
               expression(bold(GPP)),
               expression(bold(WUE)),
               expression(bold(Tv/ET)))

var_names_x <- var_names[2:10]

# Compute correlation matrix
M <- cor(data_corr)

# Perform significance test
testRes <- cor.mtest(data_corr, conf.level = 0.95)

# Define color palette from red to blue
color_palette <- colorRampPalette(c("red", "white", "blue"))(100)

setwd(plots_dir)
tiff("corrplot_d.tiff", width = 15, height = 15, units = 'cm', res = 350)

# Plot correlations without text labels
corrplot(M,
         p.mat = testRes$p,
         method = 'ellipse', 
         type = 'upper', 
         insig = 'blank',
         addCoef.col = 'black', 
         number.cex = 0.8,          # Adjust font size of correlation coefficients
         order = 'original', 
         diag = FALSE,
         tl.pos = 'n',              # Suppress default text labels
         cl.cex = 1.0, 
         col = color_palette, 
         title = 'Daily-scale',
         mar = c(3, 3, 3, 3),       # Adjust margins to prevent clipping of labels
         pch.col = 'black', 
         pch.cex = 1, 
         pch.cor = testRes$p,
         lower = color_palette, 
         upper = color_palette
)

# Get plot coordinates
n <- ncol(M)
coords <- seq(1, n, by = 1)

# Add custom variable names using text function with smaller font size
for (i in 1:n) {
  # Add variable names on the upper row
  text(i + 1.1, n + 1.25, labels = var_names_x[i], srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  # Add variable names on the left column
  text(-0.5, n - i + 1, labels = var_names[i], adj = 1, xpd = TRUE, cex = 0.8)
  # Add variable names on the diagonal closer to the plot
  text(i + 0.2, n - i + 1 + 0.2, labels = var_names[i], srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
}

dev.off()


# References --------------------------------------------------------

# Beer, C., Ciais, P., Reichstein, M., Baldocchi, D., Law, B. E., et al. (2009). 
# Temporal and among-site variability of inherent water use efficiency at the ecosystem level. 
# Global Biogeochemical Cycles, 23. https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2008GB003233
# 
# Nelson, J. A., Carvalhais, N., Cuntz, M., Delpierre, N., Knauer, J., Ogée, J., et al. (2018). 
# Coupling water and carbon fluxes to constrain estimates of transpiration: The TEA algorithm. 
# Journal of Geophysical Research: Biogeosciences, 123, 3617–3632. https://doi.org/10.1029/2018JG004727

# corrplot library
citation("corrplot")


# done.









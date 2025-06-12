# ISCO Loads Plotting
# 26June2024
# Carly Bauer
# uses csv from ISCO data package 

rm(list=ls(all=TRUE))

library(tidyverse)
library(tidyr)
library(ggplot2)
library(paletteer)
library(lubridate)
library(dplyr)
library(readxl)
library(broom)

# published isco data
data <- 	read_csv("https://pasta.lternet.edu/package/data/eml/edi/2035/1/a3faa5a1c31d6eb10ac698a8fdfb9dd2")

data <- data %>% 
  mutate(End_time = as_date(Collection_end_time)) %>% 
  mutate(Start_time = as_date(Collection_start_time))

imetals <- data %>%
  mutate(Year = year(Start_time)) %>% 
  filter(!is.na(Start_time),
         Year == 2020 | Year == 2023)

metals3 <- imetals %>% 
  filter(Element_name == "TBa_mgL" | Element_name == "TAl_mgL" | 
                   Element_name == "TCu_mgL")

imetalsSI <- data %>% 
  mutate(Year = year(Start_time)) %>% 
  filter(!is.na(Start_time),
         Year == 2021 | Year == 2022)

metals3SI <- imetalsSI %>% 
filter(Element_name == "TBa_mgL" | Element_name == "TAl_mgL" | 
         Element_name == "TCu_mgL")
##::::::::::::::::::::::::::::::::::::::::::::::::::##
# manuscript 
ggplot(metals3) +
  ggtitle("Total Metal Loads")+
  theme_bw() + theme(panel.grid = element_blank()) + 
  facet_wrap(vars(Year), nrow = 2, scales = 'free_x' ) +
  scale_x_date(date_labels = '%b', breaks = '1 month') +
  geom_point(aes(x = End_time, y = Load_kg, colour = Element_name), size = 2) +
  scale_color_paletteer_d("ltc::fernande",
                          name = "Metal",
                          labels = c("Al", "Ba", "Cu")) +
  xlab(NULL)+
  ylab('Metal Load (kg)') +
  theme(plot.title = element_text(size=10),
        axis.text=element_text(size=10), #change font size of axis text
        axis.title=element_text(size=10), #change font size of axis titles
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  guides(shape = "none") # Adjust size for legend shapes

# SI
ggplot(metals3SI) +
  ggtitle("Total Metal Loads")+
  theme_bw() + theme(panel.grid = element_blank()) + 
  facet_wrap(vars(Year), nrow = 2, scales = 'free_x' ) +
  scale_x_date(date_labels = '%b', breaks = '1 month') +
  geom_point(aes(x = End_time, y = Load_kg, colour = Element_name), size = 2) +
  scale_color_paletteer_d("ltc::fernande",
                          name = "Metal",
                          labels = c("Al", "Ba", "Cu")) +
  xlab(NULL)+
  ylab('Metal Load (kg)') +
  theme(plot.title = element_text(size=10),
        axis.text=element_text(size=10), #change font size of axis text
        axis.title=element_text(size=10), #change font size of axis titles
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  guides(shape = "none") # Adjust size for legend shapes

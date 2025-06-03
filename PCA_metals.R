# PCA metals
# 29 May 2024
# Carly Bauer

install.packages("factoextra")
install.packages("FactoMineR")
install.packages("ggfortify")

rm(list=ls(all=TRUE))
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)
library(factoextra) # for visualizing PCA 
library(stats) # prcomp to perform PCA
library(FactoMineR)
library(ggfortify)
library(ggpubr)

# load metal data from EDI using version through 2023 because not including 2024 in MS
metals <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/455/8/9c8c61b003923f4f03ebfe55cea8bbfd")

# this gets rid of any values that we flagged as abnormally high in the data
metals <- metals %>% 
  filter(Flag_TLi_mgL != 8 & Flag_SLi_mgL != 8 & Flag_TLi_mgL != 68 & Flag_SLi_mgL != 68,
         Flag_TNa_mgL != 8 & Flag_SNa_mgL != 8 & Flag_TNa_mgL != 68 & Flag_SNa_mgL != 68,
         Flag_TMg_mgL != 8 & Flag_SMg_mgL != 8 & Flag_TMg_mgL != 68 & Flag_SMg_mgL != 68,
         Flag_TAl_mgL != 8 & Flag_SAl_mgL != 8 & Flag_TAl_mgL != 68 & Flag_SAl_mgL != 68,
         Flag_TSi_mgL != 8 & Flag_SSi_mgL != 8 & Flag_TSi_mgL != 68 & Flag_SSi_mgL != 68,
         Flag_TK_mgL != 8 & Flag_SK_mgL != 8 & Flag_TK_mgL != 68 & Flag_SK_mgL != 68,
         Flag_TCa_mgL != 8 & Flag_SCa_mgL != 8 & Flag_TCa_mgL != 68 & Flag_SCa_mgL != 68,
         Flag_TFe_mgL != 8 & Flag_SFe_mgL != 8 & Flag_TFe_mgL != 68 & Flag_SFe_mgL != 68,
         Flag_TMn_mgL != 8 & Flag_SMn_mgL != 8 & Flag_TMn_mgL != 68 & Flag_SMn_mgL != 68,
         Flag_TCu_mgL != 8 & Flag_SCu_mgL != 8 & Flag_TCu_mgL != 68 & Flag_SCu_mgL != 68,
         Flag_TSr_mgL != 8 & Flag_SSr_mgL != 8 & Flag_TSr_mgL != 68 & Flag_SSr_mgL != 68,
         Flag_TBa_mgL != 8 & Flag_SBa_mgL != 8 & Flag_TBa_mgL != 68 & Flag_SBa_mgL != 68)


## format data so its just the 12 metals, nothing else in dataframe for FCR and BVR
# save new df that only includes FCR site 50 data and only T metal concentrations 
FCRmetals <- metals %>%
  mutate(Year = year(DateTime)) %>% 
  filter(Reservoir == "FCR", Site == 50, Year >= 2020) %>%
  select(starts_with("T")) %>%
  #select(-Site) %>% 
  mutate(across(starts_with("T"), log10)) %>% # log10 concentrations to help linearity, not assumption just helps increase strength
  na.omit()  # Remove rows with any NA values


# save new df that only includes BVR site 50 data and only T metal concentrations 
BVRmetals <- metals %>%
  mutate(Year = year(DateTime)) %>% 
  filter(Reservoir == "BVR", Site == 50, Year >= 2020) %>%
  select(starts_with("T")) %>%
  #select(-Site) %>% 
  mutate(across(starts_with("T"), log10)) %>% # log10 concentrations to help linearity, not assumption just helps increase strength
  na.omit()  # Remove rows with any NA values

#::::::::::::::::::::::::::::::::::::::::::::::::::::::
# do PCA
PCAmetals <- prcomp(BVRmetals, scale=TRUE) # rerun this line and below for other reservoir
summary(PCAmetals)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::
## scree plot to visualize percentages of variance explained by each principal component
 fviz_eig(PCAmetals, addlabels = TRUE)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::
# what variables contribute the most to PC1 and PC2 
fviz_cos2(PCAmetals, choice = "var", axes =1:2 )

 
 ## graph of variables - need to rerun line 58 for BVR results 
 #::::::::::::::::::::::::::::::::::::::::::::::::::::::
 # positive correlated variables point to same direction 
 # negative correlated variables point to opposite direction of other variables
 pca_var_plot<-fviz_pca_var(PCAmetals,
                            col.var = "contrib", # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # those with similar cos2 values will plot similar colors 
                            repel = TRUE     # Avoid text overlapping
 )
 library(ggplot2)
 pca_var_plot + ggtitle("PCA BVR") # change title depending on reservoir


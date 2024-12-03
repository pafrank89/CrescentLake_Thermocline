### CRESCENT LAKE TEMPERATURE DATA ANALYSIS ####

#Written by Peter Frank#

#Script for analysis of temperature data from Crescent Lake on the Kenai Peninsula in Alaska.#
#Data was from a single series of 17 Hobo temperature sensors spaced every two meters #
#in the water column deployed between March to October of 2024#

##PACKAGES##
install.packages("remotes")
require(remotes)
remotes::install_github("robertladwig/LakeModelR")

install.packages("LakeModelR")
library(LakeModelR)
require(tidyverse)

install.packages("rLakeAnalyzer")
library(rLakeAnalyzer)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)

install.packages("lubridate")
library(lubridate)

##Transform raw data from wide format to long format for analysis##
TempData <- TempData_Raw %>% pivot_longer(cols = c('33','31','29','27','25','23','21',
                                                   '19','17','13','11','9','7','5','1'),
                                          names_to = 'Depth',
                                          values_to = 'points')


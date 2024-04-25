# 01-load


# load packages -----------------------------------------------------------

library(highcharter)
library(custom.map)
library(tidyverse)
library(dplyr)
library(readxl)
library(htmltools)
library(shiny)
library(bsutils)
# library(reshape2)

# load data ---------------------------------------------------------------

#set options
options(readr.show_col_types = FALSE)

#load data
govData <- read_csv("data/GovStruct.csv")
Workforce <- read_csv("data/Workforce.csv")
Attrition <- read_csv("data/workforceAttrition.csv")
vacant_change <- read_excel("data/vacPosChangeCat.xlsx")

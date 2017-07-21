# eda4plant

*eda4plant* aims to implement an Exploratory Data Analysis (EDA) for plant breeding trials. At the beginning, it might deliver:

- Static Graphics: Box plots, histogram, bar plots, among others. 
- Interactive Graphics: Biplots, density diagrams.

## Required packages
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggplot2")
#install.packages("ggrepel")
#install.packages("dplyr")
#install.packages("tidyr")

## Requiered packages eda4plant
devtools("CIP-RIU/eda4plant")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)

library(eda4plant)

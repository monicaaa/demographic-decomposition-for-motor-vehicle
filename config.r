# install these packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(foreign)
library(stargazer)
library(stringr)
library(purrr)
library(lazyeval)
library(xtable)
library(tidyr)
library(srvyr)
library(broom)
library(scales) 
library(magrittr)
library(knitr)

# Set your project directory
project.dir <- "/Users/moniking/Documents/demographic-decomposition-for-motor-vehicle/"
setwd(project.dir)

# Set your data directory
data.dir <- paste0(project.dir, "data/") 

source("utils.r") # sourcing all functions

########################################################################
#### Read in data ------------------------------------------------------
########################################################################

# Read in raw NHTS datasets 

raw.person2001 <- read.table(c(paste0(data.dir, "2001/", "person", ".csv")), 
                             fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))
raw.trip2001 <- read.table(c(paste0(data.dir, "2001/", "trip", ".csv")), 
                           fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))
raw.person2009 <- read.table(c(paste0(data.dir, "2009/", "person", ".csv")), 
                             fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))
raw.trip2009 <- read.table(c(paste0(data.dir, "2009/", "trip", ".csv")), 
                           fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))

# Read in CDC Vital Stats files
list_of_cdc_travel_files <- c("cdc_travel_2001_2010", 
                              "cdc_travel_2001_2010_ped",
                              "cdc_travel_2001_2010_pveh")

# 2000 population distribution for age-standardized calculations
pop2000stand <- read.csv(c(paste0(data.dir, "pop2000stand.csv")))

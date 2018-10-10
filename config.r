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

project.dir <- "/Users/moniking/Documents/demographic-decomposition-for-motor-vehicle/"
setwd(project.dir)
data.dir <- "/Users/moniking/Documents/demographic-decomposition-for-motor-vehicle/data/" # change accordingly
source("utils.r")

# output.dir <- paste(project.dir, "output/", sep="")

# Read in data -----------------------------------------

# CDC vital stats 1984 to 2013 ---------------

# Read in national household travel survey

raw.person2001 <- read.table(c(paste0(data.dir, "travel/2001/", "person", ".csv")), 
                             fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))
raw.trip2001 <- read.table(c(paste0(data.dir, "travel/2001/", "trip", ".csv")), 
                           fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))
raw.person2009 <- read.table(c(paste0(data.dir, "travel/2009/", "person", ".csv")), 
                             fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))
raw.trip2009 <- read.table(c(paste0(data.dir, "travel/2009/", "trip", ".csv")), 
                           fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))

# Read in and clean CDC Travel files
list_of_cdc_travel_files <- c("cdc_travel_2001_2010", 
                              "cdc_travel_2001_2010_ped",
                              "cdc_travel_2001_2010_pveh",
                              "cdc_travel_2001_2010_motorcycle")

# 2000 population distribution
pop2000stand <- read.csv(c(paste0(data.dir, "pop2000stand.csv")))
pop2000standmotor <- read.csv(c(paste0(data.dir, "pop2000standmotor.csv")))

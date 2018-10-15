# install these packages
library(tidyverse)
library(reshape2)
library(here)
library(ggthemes)
library(knitr)

source("functions.r") # sourcing all functions

########################################################################
#### Read in data ------------------------------------------------------
########################################################################

# Read in raw NHTS datasets 
raw.person2001 <- read.table(here("data", "person2001.csv"), 
                             fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))
raw.trip2001 <- read.table(here("data", "trip2001.csv"), 
                           fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))
raw.person2009 <- read.table(here("data", "person2009.csv"), 
                             fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))
raw.trip2009 <- read.table(here("data", "trip2009.csv"), 
                           fill=TRUE, header=TRUE, sep=",", na.strings = c(".", seq(-10,-1)))

# Read in CDC Vital Stats files
list_of_cdc_travel_files <- c("cdc_travel_2001_2010", 
                              "cdc_travel_2001_2010_ped",
                              "cdc_travel_2001_2010_pveh")

# 2000 population distribution for age-standardized calculations
pop2000stand <- read.csv(here("pop2000stand.csv"))

########################################################################
#### Clean data and perform preliminary calculations--------------------
########################################################################

# clean all CDC data and bring them into the R environment
cleaned.cdc.travel<-lapply(list_of_cdc_travel_files, make_cdc_travel_df)
names(cleaned.cdc.travel) <- list_of_cdc_travel_files
list2env(cleaned.cdc.travel, .GlobalEnv)

# clean NHTS data
person2001 <- clean_all_nhts(raw.person2001)
trip2001 <- clean_all_nhts(raw.trip2001)
person2009 <- clean_all_nhts(raw.person2009)
trip2009 <- clean_all_nhts(raw.trip2009)

# get national population represented by the data
popgroup2001 <- get_pop_bygroup(person2001)
popgroup2009 <- get_pop_bygroup(person2009)

# calculate risk and exposure for each survey year
exp2001.all <- get_exposure_1year(year_in = 2001)
exp2001.ped <- get_exposure_1year(year_in = 2001, mode_in = "Walk")
exp2001.pveh <- get_exposure_1year(year_in = 2001, mode_in = "Pvehicle")

exp2009.all <- get_exposure_1year(year_in = 2009)
exp2009.ped <- get_exposure_1year(year_in = 2009, mode_in = "Walk")
exp2009.pveh <- get_exposure_1year(year_in = 2009, mode_in = "Pvehicle")

# calculate risk and exposure for 2001-2010 (averaged for the two survey years)
exp.all <- calc_exp_risk_2000(df_2001 = exp2001.all, df_2009 = exp2009.all, df_cdc = cdc_travel_2001_2010)
exp.ped <- calc_exp_risk_2000(df_2001 = exp2001.ped, df_2009 = exp2009.ped, df_cdc = cdc_travel_2001_2010_ped)
exp.pveh <- calc_exp_risk_2000(df_2001 = exp2001.pveh, df_2009 = exp2009.pveh, df_cdc = cdc_travel_2001_2010_pveh)

# calculate decompositions for each type of MV death
decomp.all <- decompose(df_in = exp.all) %>% mutate(type = "Total")
decomp.ped <- decompose(df_in = exp.ped) %>% mutate(type = "Pedestrian")
decomp.pveh <- decompose(df_in = exp.pveh) %>% mutate(type = "Passenger Vehicle")

# combine the previous decompositions into one df
decomp_combined <- rbind(decomp.all, decomp.ped, decomp.pveh)
decomp_combined$type <- as.factor(decomp_combined$type)
decomp_combined$type <- factor(decomp_combined$type, 
                               levels = c("Total", "Passenger Vehicle", "Pedestrian"))

# age standardized rates for death, exposure, and risk
age_stand_all <- calculate_as_rates(exp.all)
age_stand_pveh <- calculate_as_rates(exp.pveh)
age_stand_ped <- calculate_as_rates(exp.ped)
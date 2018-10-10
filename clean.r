cleaned.cdc.travel<-lapply(list_of_cdc_travel_files, make_cdc_travel_df)
names(cleaned.cdc.travel) <- list_of_cdc_travel_files
list2env(cleaned.cdc.travel, .GlobalEnv)

person2001 <- clean_all_nhts(raw.person2001)
trip2001 <- clean_all_nhts(raw.trip2001)
person2009 <- clean_all_nhts(raw.person2009)
trip2009 <- clean_all_nhts(raw.trip2009)

# DO I NEED THIS? popgroup2001 <- get_pop_bygroup(person2001)
# DO I NEED THIS? popgroup2009 <- get_pop_bygroup(person2009)

# calculated risk and exposure
exp2001.all <- get_exposure_1year(year_in = 2001)
exp2001.ped <- get_exposure_1year(year_in = 2001, mode_in = "Walk")
exp2001.pveh <- get_exposure_1year(year_in = 2001, mode_in = "Pvehicle")

exp2009.all <- get_exposure_1year(year_in = 2009)
exp2009.ped <- get_exposure_1year(year_in = 2009, mode_in = "Walk")
exp2009.pveh <- get_exposure_1year(year_in = 2009, mode_in = "Pvehicle")

exp.all <- calc_exp_risk_2000(df_2001 = exp2001.all, df_2009 = exp2009.all, df_cdc = cdc_travel_2001_2010)
exp.ped <- calc_exp_risk_2000(df_2001 = exp2001.ped, df_2009 = exp2009.ped, df_cdc = cdc_travel_2001_2010_ped)
exp.pveh <- calc_exp_risk_2000(df_2001 = exp2001.pveh, df_2009 = exp2009.pveh, df_cdc = cdc_travel_2001_2010_pveh)

### Calculating decompositions ----------
decomp.all <- decompose(df_in = exp.all) %>% mutate(type = "Total")
decomp.ped <- decompose(df_in = exp.ped) %>% mutate(type = "Pedestrian")
decomp.pveh <- decompose(df_in = exp.pveh) %>% mutate(type = "Passenger Vehicle")

decomp_combined <- rbind(decomp.all, decomp.ped, decomp.pveh)
decomp_combined$type <- as.factor(decomp_combined$type)
decomp_combined$type <- factor(decomp_combined$type, 
                               levels = c("Total", "Passenger Vehicle", "Pedestrian"))

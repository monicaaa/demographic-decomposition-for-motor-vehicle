########################################################################
### 1. CDC VITAL STATS: clean and tidy ---------------------------------
########################################################################

# Function reads in CDC Vital Stat data
# @Param filename is the name of the .txt file
# @return is a df
read_in_cdc_region <- function(filename, col = 10){
  rawdata <- read.table(c(paste("data/", filename, ".txt", sep="")), 
                        fill=TRUE, header=TRUE, sep="\t", na.strings=c(""), 
                        colClasses = c("NULL",rep(NA,col)))
  return(na.omit(rawdata))
}

# Function cleans column names of CDC Vital Stat data
# @Param df_in is the df to be cleaned
# @return is the cleaned df
rename_column_cdcwonder <- function(df_in){
  names(df_in) <- tolower(names(df_in)) 
  colnames(df_in)[grep(".*age.*code", colnames(df_in))] <- "age" # simplify age 
  colnames(df_in)[grep("cause.of.death.code", colnames(df_in))] <- "icd" # simplify icd
  if("census.region" %in% colnames(df_in)){
    names(df_in)[names(df_in)=="census.region"] <- "region"
  }
  if("age.adjusted.rate" %in% colnames(df_in)){
    names(df_in)[names(df_in)=="age.adjusted.rate"] <- "aa.rate"
  }
  return(df_in)
}

# Function cleans levels of certain columns of CDC Vital Stat data
# @Param df_in is the df to be cleaned
# @return is the cleaned df
rename_levels_cdcwonder <- function(df_in){
  levels(df_in$race)[levels(df_in$race)=="Black or African American"] <- "Black" # simplify race label
  if("age" %in% colnames(df_in)){
    levels(df_in$age)[levels(df_in$age)=="5-9"] <- "05-09" # format age
  }
  if("region" %in% colnames(df_in)){
    levels(df_in$region) <- sub(".*:", "", levels(df_in$region))
  }
  df_in$race <- relevel(df_in$race, ref = 2)
  return(df_in)
}

# Function makes new age group variable with new levels for CDC Vital Stat data
# @Param df_in is the df to be cleaned
# @return is the cleaned df called newdf
make_age_groups <- function(df_in){
  newdf <- df_in %>% 
    mutate(age.range = gsub("-", ".", paste0("age",age)))  # reformat age
  newdf$age.range <- as.factor(newdf$age.range)
  newdf$age.cat <- NA
  newdf$age.cat[newdf$age.range %in% c("age05.09", "age10.14", "age5.14")] <- "age05.14"
  newdf$age.cat[newdf$age.range %in% c("age15.19", "age20.24", "age15.24")] <- "age15.24"
  newdf$age.cat[newdf$age.range %in% c("age25.34", "age35.44")] <- "age25.44"
  newdf$age.cat[newdf$age.range %in% c("age45.54", "age55.64")] <- "age45.64"
  newdf$age.cat[newdf$age.range %in% c("age65.74", "age75.84")] <- "age65.84"
  newdf$age.cat <- as.factor(newdf$age.cat)
  newdf$age <- factor(newdf$age, 
                      levels = c("05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                                 "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84"))
  return(newdf)
}

#### Main function to make clean CDC death data and make it compatible with travel survey groups
# @Param df_in is the filename of the raw CDC txt file 
# @return is the cleaned df with aggreagated MV deaths and pop grouped by 
# race, age, and gender
make_cdc_travel_df <- function(df_in){
  newdf <- read_in_cdc_region(df_in) %>%
    rename_column_cdcwonder() %>%
    rename_levels_cdcwonder() %>%
    make_age_groups() %>%
    dplyr::select(gender, race, deaths, population, age.cat) %>%
    group_by(gender, race, age.cat) %>%
    summarise(deaths = sum(deaths),
              population = sum(population))
  return(newdf)
}

########################################################################
### 2. NHTS TRAVEL SURVEY: read ----------------------------------------
########################################################################

# Function to lower column names
# @Param df_in is the df to be cleaned
# @return is the cleaned df
lower_colnames <- function(df_in){
  colnames(df_in) <- tolower(colnames(df_in))
  return(df_in)
}

# Function to relabel variable names, especially as names differ across surveys
# @Param df_in is the df to be cleaned
# @return is the cleaned df
rename_column_travel <- function(df_in){
  names(df_in)[names(df_in) %in% c("serial","houseid")]<- "house_id"
  names(df_in)[names(df_in) %in% c("per_no","personid")]<- "person_id"
  names(df_in)[names(df_in) %in% c("hh_race", "hhr_race")]<- "race"
  names(df_in)[names(df_in) %in% c("hh_hisp", "hhr_hisp")]<- "hisp"
  names(df_in)[names(df_in) %in% c("perwgt", "pe1fiwgt","wtperfin")]<- "perweight"
  names(df_in)[names(df_in) %in% c("sex", "r_sex")]<- "gender"
  names(df_in)[names(df_in) %in% c("r_age")]<- "age"
  names(df_in)[names(df_in) %in% c("trpmiles")]<- "distance"
  names(df_in)[names(df_in) %in% c("perwttif", "dapfiwgt", "wttrdfin")]<- "tripweight"
  names(df_in)[names(df_in) %in% c("means", "trptrans")]<- "mode"
  return(df_in)
}

# Function to relabel race as white, black, other
# @Param df_in is the df to be cleaned
# @return is the cleaned df
clean_race_travel <- function(df_in){
  if("race" %in% colnames(df_in)){
    df_in$race <- as.factor(df_in$race)
    levels(df_in$race) <- c("White", "Black", 
                            rep("Other", length.out = length(levels(df_in$race))-2))
  }
  return(df_in)
}

# Function to relabel gender
# @Param df_in is the df to be cleaned
# @return is the cleaned df
clean_sex_travel <- function(df_in){
  if("gender" %in% colnames(df_in)){
    df_in$gender <- as.factor(df_in$gender)
    levels(df_in$gender) <- c("Male", "Female", 
                              rep("Other", length.out = length(levels(df_in$gender))-2))
  }
  return(df_in)
}

# Function to relabel census regions
# @Param df_in is the df to be cleaned
# @return is the cleaned df
clean_region_travel <- function(df_in){
  if("census_r" %in% colnames(df_in)){
    df_in$census_r <- as.factor(df_in$census_r)
    levels(df_in$census_r) <- c("Northeast", "Midwest", "South", "West")
  }
  return(df_in)
}

# Function to recode trip distance based on data documention
# @Param df_in is the df to be cleaned
# @return is the cleaned df
clean_distance_travel <- function(df_in){
  if("distance" %in% colnames(df_in)){
    df_in$distance[df_in$distance==99997] <- 0.25
    df_in$distance[df_in$distance>9997] <- NA
    df_in$distance[df_in$distance==0] <- 0.25
    df_in$distance[df_in$distance==9997] <- 0.50
    df_in$distance[df_in$distance==9996] <- 0.06
    df_in$distance[df_in$distance<0] <- NA
  }
  return(df_in)
}

# Function to relabel mode across different years of the NHTS 
# @Param df_in is the df to be cleaned
# @return is the cleaned df
clean_mode_travel <- function(df_in){
  if("mode" %in% colnames(df_in)){
    df_in$mode <- as.factor(df_in$mode)
    if(length(levels(df_in$mode))==20){
      levels(df_in$mode) <- c("Auto", "Van", "Van", "Other Truck",
                              "Motorcycle", "Rec", "Taxi", 
                              "Bus", "Train", "Streetcar",
                              "El", "Airplane", "Taxi", "Truck", "Bicycle",
                              "Walk", "School bus", "Moped", "Other", NA)
    }
    if(length(levels(df_in$mode))==23){
      levels(df_in$mode) <- c("Auto", "Station Wagon", "Passenger Van", "Other Van",
                              "Pickup Truck", "Pickup with camper", "Other truck", 
                              "Motorized camper coach", "Motorcycle", "Motorized bike moped",
                              "Other POV", "Bus", "Train", "Streetcar", "El or subway",
                              "Airplane", "Taxi Commercial", "Bicycle", "Walk", "School bus", 
                              "Other", NA, NA)
    }
    if(length(levels(df_in$mode))==22){
      levels(df_in$mode) <- c("Auto", "Van", "Van", "Pickup truck",
                              "Other truck", "RV", "Motorcycle", "Motorized bike moped",
                              "Other POV", "Bus", "Amtrak", "Commuter train", "Streetcar", "El or subway",
                              "Airplane", "Taxi Commercial", "Bicycle", "Walk", "School bus", 
                              "Other", NA, NA)
    }
    if(length(levels(df_in$mode))==21){
      levels(df_in$mode) <- c("Auto", "Van", "SUV", "Pickup truck",
                              "Other truck", "RV", "Motorcycle",
                              "Other POV", "Bus", "Amtrak", "Commuter train", "Streetcar", "El or subway",
                              "Airplane", "Taxi Commercial", "Bicycle", "Walk", "School bus", 
                              "Other", NA, NA)
    }
    if(length(levels(df_in$mode))==27){
      levels(df_in$mode) <- c("Auto", "Van", "SUV", "Pickup truck",
                              "Other truck", "RV", "Motorcycle", "Plane",
                              "Plane", "Bus", "Bus", "School bus", "Bus", "Bus", 
                              "Amtrak", "Commuter train", "El or subway", "Streetcar",
                              "Ship", "Ferry", "Sailboat", "Taxi Commercial", "Limo",
                              "Hotel shuttle", "Bicycle", "Walk",
                              "Other")
    }
    if(length(levels(df_in$mode))==25){
      levels(df_in$mode) <- c("Auto", "Van", "SUV", "Pickup truck",
                              "Other truck", "RV", "Motorcycle", "Light electric vehicle",
                              "Bus", "Bus", "School bus", "Bus", "Bus", "Bus",
                              "Amtrak", "Commuter train", "El or subway", "Streetcar",
                              "Taxi Commercial", "Ferry", "Plane", 
                              "Bicycle", "Walk", "Special transit", "Other")
    }
  }
  return(df_in)
}

# Function to create a new variable with only the relevant modes of travel
# @Param df_in is the df to be cleaned
# @return is the cleaned df
clean_mode_type_travel <- function(df_in){
  if("mode" %in% colnames(df_in)){
    df_in$mode_type <- "Other"
    df_in$mode_type[df_in$mode %in% c("Auto", "Van", "Pickup truck", "SUV")] <- "Pvehicle"
    df_in$mode_type[df_in$mode %in% c("Walk")] <- "Walk"
    df_in$mode_type[df_in$mode %in% c("Motorcycle")] <- "Motorcycle"
    df_in$mode_type <- as.factor(df_in$mode_type)
  }
  return(df_in)
}

# Function to relabel Hispanic
# @Param df_in is the df to be cleaned
# @return is the cleaned df
clean_hisp_travel <- function(df_in){
  if("hisp" %in% colnames(df_in)){
    df_in$hisp <- as.factor(df_in$hisp)
    levels(df_in$hisp) <- c("Hisp", "Nonhisp", 
                            rep("Other", length.out = length(levels(df_in$hisp))-2))
    return(df_in)
  }
  if(!("hisp" %in% colnames(df_in))){ # older NHTS didn't collect Hispanic ethnicity
    df_in$hisp <- "Nonhisp"
    df_in$hisp <- as.factor(df_in$hisp)
    return(df_in)
  }
}

# Function to relabel age groups for 5-year categories
# @Param df_in is the df to be cleaned
# @return is the cleaned df
make_age_groups_5 <- function(df_in){
  if("age" %in% colnames(df_in)){
    df_in$age.range <- NA # those outside the ranges below are labeled as missing 
    df_in$age.range[df_in$age >= 5 & df_in$age <=9] <- "age05.09"
    df_in$age.range[df_in$age >= 10 & df_in$age <=14] <- "age10.14"
    df_in$age.range[df_in$age >= 15 & df_in$age <=19] <- "age15.19"
    df_in$age.range[df_in$age >= 20 & df_in$age <=24] <- "age20.24"
    df_in$age.range[df_in$age >= 25 & df_in$age <=29] <- "age25.29"
    df_in$age.range[df_in$age >= 30 & df_in$age <=34] <- "age30.34"
    df_in$age.range[df_in$age >= 35 & df_in$age <=39] <- "age35.39"
    df_in$age.range[df_in$age >= 40 & df_in$age <=44] <- "age40.44"
    df_in$age.range[df_in$age >= 45 & df_in$age <=49] <- "age45.49"
    df_in$age.range[df_in$age >= 50 & df_in$age <=54] <- "age50.54"
    df_in$age.range[df_in$age >= 55 & df_in$age <=59] <- "age55.59"
    df_in$age.range[df_in$age >= 60 & df_in$age <=64] <- "age60.64"
    df_in$age.range[df_in$age >= 65 & df_in$age <=69] <- "age65.69"
    df_in$age.range[df_in$age >= 70 & df_in$age <=74] <- "age70.74"
    df_in$age.range[df_in$age >= 75 & df_in$age <=79] <- "age75.79"
    df_in$age.range[df_in$age >= 80 & df_in$age <=84] <- "age80.84"
    df_in$age.range <- as.factor(df_in$age.range)
  }
  return(df_in)
}

# Function to relabel age groups for 10-year categories
# @Param df_in is the df to be cleaned
# @return is the cleaned df
make_age_groups_10 <- function(df_in){
  if("age" %in% colnames(df_in)){
    df_in$age.cat10 <- NA # those outside of the ranges below are labeled as missing
    df_in$age.cat10[df_in$age >= 15 & df_in$age <=24] <- "age15.24"
    df_in$age.cat10[df_in$age >= 25 & df_in$age <=34] <- "age25.34"
    df_in$age.cat10[df_in$age >= 35 & df_in$age <=44] <- "age35.44"
    df_in$age.cat10[df_in$age >= 45 & df_in$age <=54] <- "age45.54"
    df_in$age.cat10[df_in$age >= 55 & df_in$age <=64] <- "age55.64"
    df_in$age.cat10[df_in$age >= 65 & df_in$age <=74] <- "age65.74"
    df_in$age.cat10[df_in$age >= 75 & df_in$age <=84] <- "age75.84"
    df_in$age.cat10 <- as.factor(df_in$age.cat10)
  }
  return(df_in)
}

# Function to relabel age groups for lifecycle groups
# @Param df_in is the df to be cleaned
# @return is the cleaned df
make_age_groups_cont <- function(df_in){
  if("age" %in% colnames(df_in)){
    df_in$age.cat <- NA # those outside of ages below are labeled as missing
    df_in$age.cat[df_in$age >= 5 & df_in$age <=14] <- "age05.14"
    df_in$age.cat[df_in$age >= 15 & df_in$age <=24] <- "age15.24"
    df_in$age.cat[df_in$age >= 25 & df_in$age <=44] <- "age25.44"
    df_in$age.cat[df_in$age >= 45 & df_in$age <=64] <- "age45.64"
    df_in$age.cat[df_in$age >= 65 & df_in$age <=84] <- "age65.84"
    df_in$age.cat <- as.factor(df_in$age.cat)
  }
  return(df_in)
}

### Larger function that includes previous functions to clean NHTS data
# @Param df_in is the df to be cleaned
# @return is the cleaned df as newdf
clean_all_nhts <- function(df_in){
  newdf <- df_in %>%
    lower_colnames %>%
    rename_column_travel %>%
    clean_race_travel %>%
    clean_sex_travel %>%
    clean_region_travel %>%
    clean_distance_travel %>%
    clean_mode_travel %>%
    clean_mode_type_travel %>%
    clean_hisp_travel %>%
    make_age_groups_cont %>%
    make_age_groups_5 %>%
    make_age_groups_10
  return(newdf)
}

########################################################################
### 3. NHTS TRAVEL SURVEY: calculate -----------------------------------
########################################################################

# Function that calculates population represented in the survey
# by race, sex, and age
# @Param df_in is the df of NHTS
# returns a df 
get_pop_bygroup <- function(df_in){
  if("hisp" %in% colnames(df_in)){
    newdf <- df_in %>%
      group_by(gender, race, hisp, age.cat) %>%
      summarise(pop = sum(perweight)) %>%
      filter(race %in% c("Black", "White"),
             hisp == "Nonhisp",
             gender %in% c("Male", "Female")) %>%
      na.omit %>%
      as.data.frame()
    return(newdf)
  }
}

# Function that calculates travel amount (person trips and person miles travelled) 
# by race, sex, and age
# @Param df_in is the df to be cleaned
# @Param mode_type is the mode of travel, default is ALL modes of travel
# returns a df
get_travel_bygroup <- function(df_in, mode_type = "All"){
  if(mode_type=="Walk"){
    df_in <- df_in %>%
      filter(mode_type == "Walk")
  }
  if(mode_type=="Pvehicle"){
    df_in <- df_in %>%
      filter(mode_type == "Pvehicle")
  }
  if(mode_type=="Motorcycle"){
    df_in <- df_in %>%
      filter(mode_type == "Motorcycle")
  }
  newdf <- df_in %>%
    group_by(gender, race, hisp, age.cat) %>%
    summarise(ptrips = sum(as.numeric(tripweight, na.rm= TRUE)), # calculate person-trips
              pmt = sum(tripweight*distance, na.rm = TRUE)) %>% # calculate person miles travelled
    filter(race %in% c("Black", "White"),
           hisp == "Nonhisp") %>%
    na.omit %>%
    as.data.frame()
  return(newdf)
}

# Function that calculates exposure rate (travel amount / population) 
# by race, sex, and age
# @Param pop_in is the df with population data
# @Param travel_in is the df with travel amount
# returns a df
get_exposure <- function(pop_in, travel_in, exp_type = NULL){
  newdf <- pop_in %>%
    inner_join(travel_in) %>%
    mutate(ptrip_per_pop = ptrips / pop / 365, 
           pmt_per_pop = pmt / pop/ 365) %>% 
    dplyr::select(-c(pop:pmt)) %>%
    melt(variable.name = "mode_type", 
         value.name = "exposure")  
  return(newdf)
}

# Function that calculates exposure for a given year of the survey
# by race, sex, and age
# @Param year_in is an integer for survey year
# @Param mode_in is the mode of travel, a string, default is ALL modes of travel
# returns a df
get_exposure_1year <- function(year_in, mode_in = "All"){
  newdf <- get_travel_bygroup(eval(parse(text = paste0("trip", year_in))), mode_type = mode_in) %>%
    get_exposure(pop_in = eval(parse(text = paste0("popgroup", year_in))),
                 travel_in = .) %>%
    mutate(year = year_in)
  return(newdf)
}

# Function that calculates exposure (travel amount/pop) and risk (MV deaths/travel amount)
# during the 2001-2010 period by race, sex, and age
# assumes that 2001-2010 period travel amount is the average of 2001 and 2009 amounts
# @Param df_2001 is df of exposure amounts for 2001, created from get_exposure_1year function
# @Param df_2009 is df of exposure amounts for 2009, created from get_exposure_1year function
# @Param df_cdc is df from cleaned CDC Vital Stats data, includes population and MV deaths
# returns a df that includes risk and exposure for each population group
calc_exp_risk_2000 <- function(df_2001, df_2009, df_cdc){
  newdf <- df_2001 %>% 
    rbind(df_2009) %>%
    dcast(gender + race + age.cat~ mode_type + year, value.var = "exposure") %>%
    mutate(avg_daily_trip = (ptrip_per_pop_2001 + ptrip_per_pop_2009)/2, # average daily trip across two surveys 
           avg_daily_mile = (pmt_per_pop_2001 + pmt_per_pop_2009)/2) %>%
    left_join(df_cdc) %>% # merges with formatted population data from CDC data for 2001-2010
    mutate(total_trip_exp = avg_daily_trip*365*population,
           total_mile_exp = avg_daily_mile*365*population,
           risk_trip = deaths/total_trip_exp*10000000,
           risk_mile = deaths/total_mile_exp*100000000,
           death_10000_pop = deaths/population*100000,
           exp_trip = (total_trip_exp/10000000)/population*100000,
           exp_mile = (total_mile_exp/100000000)/population*100000) %>%
    dplyr::select(-(ptrip_per_pop_2001:pmt_per_pop_2009)) %>%
    arrange(gender, age.cat, race)
  return(newdf)
}

# Function that calculates the Das Gupta demographic decomposition
# to decompose MV death rate differences into an exposure and risk effect
# during the 2001-2010 period by race, sex, and age
# @Param df_in is the output from the calc_exp_risk_2000 function
# returns a df with the risk and exposure effects
decompose <- function(df_in){
  df_in$race <- as.factor(df_in$race)
  df_in$age.cat <- as.factor(df_in$age.cat)
  df_in$gender <- as.factor(df_in$gender)
  newdf <- df_in %>%
    arrange(gender, age.cat, desc(race)) %>%
    dplyr::group_by(gender, age.cat) %>%
    summarise(total_effect = diff(death_10000_pop), 
              risk_effect_mile = (sum(exp_mile)/2*diff(risk_mile)),
              exp_effect_mile = (sum(risk_mile)/2*diff(exp_mile)),
              risk_percent_mile = risk_effect_mile/total_effect*100,
              exp_percent_mile = exp_effect_mile/total_effect*100)
  return(newdf)
}

# Function that calculates rates for MV death, exposure, and risk
# during the 2001-2010 period by race, sex, and age
# @Param df_in is the output from the calc_exp_risk_2000 function
# returns a df with the calculated rates
calculate_as_rates <- function(df_in){
  newdf <- df_in %>%
  left_join(pop2000stand) %>%
  group_by(gender, race) %>%
  mutate(death = death_10000_pop*proportion,
         risk = risk_mile*proportion,
         exposure = exp_mile*proportion) %>%
  summarise(death_stand = sum(death),
            risk_stand = sum(risk),
            exp_stand = sum(exposure)) 
  return(newdf)
}

# Function that calculates b-w % differences in MV death, exposure, and risk rates
# during the 2001-2010 period by race, sex, and age
# @Param df_in is the output from the calculate_as_rates function
# returns a df with % differences
calculate_percent_diff <- function(df_in){
  newdf <- df_in %>% 
    gather(variable, value, -(gender:race)) %>%
    unite(temp, race, variable) %>%
    spread(temp, value) %>% 
    mutate(bw_percent_diff_death = (Black_death_stand-White_death_stand)/White_death_stand*100,
           bw_percent_diff_risk = (Black_risk_stand-White_risk_stand)/White_risk_stand*100,
           bw_percent_diff_exp = (Black_exp_stand-White_exp_stand)/White_exp_stand*100) %>%
    select(-c(Black_death_stand:White_risk_stand))
  return(newdf)
}

########################################################################
### 4. PLOTS -----------------------------------------------------------
########################################################################

# Function that plots b-w absolute rates in exposure, risk, and death
# during the 2001-2010 period by race, sex, and age
# @Param exp_df is the output from the calculate_as_rates function
# returns a plot
plot_exposure <- function(exp_df, plot_title=NULL){
  exp_df <- exp_df %>%
    dplyr::select(gender, race, age.cat, death_10000_pop, risk_mile, exp_mile) %>%
    melt(id.vars = c("gender", "race", "age.cat"),
         variable.name = "factor_type", 
         value.name = "rate")
  levels(exp_df$factor_type) <- c("MV Death Rate", "Risk Rate", "Exposure Rate")
  plot_out <- exp_df %>%
    ggplot(aes(x=age.cat, y=rate, fill=race)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values= c("#0D4F8B", "#E69F00")) +
    ggtitle(plot_title) + 
    theme(legend.position="bottom") + 
    facet_grid(factor_type ~ gender, scales = "free") +
    theme_bw() +
    scale_x_discrete("Age Categories", 
                     labels = c("5-14", "15-24", "25-44", "45-64", "65-84")) +
    theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0)) +
    theme(axis.title.x = element_blank()) +   # Remove x-axis label
    theme(axis.title.y = element_blank()) +   # Remove x-axis label
    theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0)) +
    theme(legend.position="bottom")   +
    theme(legend.title=element_blank()) +
    theme(legend.text = element_text(size = 12)) +
    theme(strip.text = element_text(size=12)) +
    theme(axis.text=element_text(size=12))
  return(plot_out)
}

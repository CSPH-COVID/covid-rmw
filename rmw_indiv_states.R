# BREAKING DOWN THE SAFEGRAPH DATA INTO THE SIX INDIVIDUAL STATES

# clear global environment
rm(list=ls())
# set working directory
setwd("/Users/emwu9912/Documents/CU Anschutz/Employment/COVID-19 Modeling Group")
# load libraries
library(pacman)
p_load(data.table, dendextend, dplyr, ggplot2, igraph, R.utils, stringr, tidyr, usdata, usmap)
# set theme
theme_set(theme_bw())

# read in SafeGraph mobility seed data
within_region <- fread("./CSTE/SafeGraph Data/cste_within_region_movements.csv.gz")

# create an origin county dataset from the US Data library in R, naming all variables with "origin"
origin_county_fips <- usdata::county_2019[c('fips', 'name', 'state')] %>%
  filter(state == "Colorado" | state == "Idaho" |
         state == "Montana" | state == "New Mexico" |
         state == "Utah" | state == "Wyoming") %>%
  mutate(origin_fips = fips,
         origin_state = as.factor(usdata::state2abbr(state)),
         origin_county = name) %>%
  select(origin_fips, origin_county, origin_state)
  
# make a destination county dataset, which is a duplicate of the origin county dataset
# but with "destination" for all the variable names
dest_county_fips <- origin_county_fips %>%
  mutate(dest_fips = origin_fips,
         dest_state = origin_state,
         dest_county = origin_county) %>%
  select(dest_fips, dest_county, dest_state)

# join within region movements with county fips
# include origin county, origin state, destination county, and destination state
df <- left_join(left_join(within_region, origin_county_fips,
                by = c("origin_fips" = "origin_fips")),
                dest_county_fips, by = c("dest_fips" = "dest_fips")) %>%
  # drop observations where the origin state does not match the destination state
  filter(origin_state == dest_state,
         # restrict the dataset time period to after January 1st, 2021
         measure_date >= as.Date("2021-01-01"))

# now create individual datasets for each state
# create an object (factor variable) whose levels are the six unique states
states <- unique(unlist(df$origin_state))
# using a for-loop, subset the data into smaller datasets where the origin states are the states along the factor variable
for(i in seq_along(states)){
  df1 <- subset(df, origin_state==states[i])
  assign(paste0("state_", states[i]), df1, envir = .GlobalEnv)
  # save results to the database
  #write.csv(df1, paste0("./CSTE/Partitioning/state_", states[i], ".csv"))
}

# get population data for counties
county_pops <- usdata::county_2019[c('fips', 'pop')] %>% mutate(fips = as.numeric(sprintf("%05d", fips)))
county_names <- usdata::county_2019[c('fips', 'name')] %>% mutate(fips = as.numeric(sprintf("%05d", fips)),
                                                                  name = stringr::str_replace_all(name, " County",""))

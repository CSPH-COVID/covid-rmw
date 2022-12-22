### FIGURING OUT THE SAMPLING FRACTION FOR DEVICES FROM THE SAFEGRAPH DATA ###

# launch setup options
source("/Users/emwu9912/RProjects/covid-analysis/setup.R")

# load CBG data to get populations
pop <- read.csv("./CSTE/Data Sets/cbg_pop.csv", colClasses=c("character", "numeric")) %>%
  mutate(county_id = substring(census_block_group, 1, 5)) %>%
  group_by(county_id) %>% summarize(total_pop = sum(total_pop))
  
# BigQuery settings
bq_auth(path = "./Admin Documents/Training and Access/co-covid-models-8ba58310c1b8.json")
projectid = "co-covid-models"
  
# download data from BigQuery
bqt = bq_project_query('co-covid-models', 'SELECT * FROM `co-covid-models.mobility.county_to_county_by_week`')
df_mob = bq_table_download(bqt)

# join everything together
mob_dates = sort(unique(df_mob$measure_date))
mob_durations = data.frame(measure_date = mob_dates[2:length(mob_dates)], 
                           mob_duration = as.numeric(mob_dates[2:length(mob_dates)] - mob_dates[1:(length(mob_dates)-1)]))


# create an origin county dataset from the US Data library in R, naming all variables with "origin"
origin_fips <- usdata::county_2019[c('fips', 'name', 'state')] %>%
  mutate(origin_county_id = as.character(fips),
         origin_state = state,
         origin_county = name) %>%
  select(origin_county_id, origin_county, origin_state)

dest_fips <- usdata::county_2019[c('fips', 'name', 'state')] %>%
  mutate(destination_county_id = as.character(fips),
         destination_state = state,
         destination_county = name) %>%
  select(destination_county_id, destination_county, destination_state)

# join everything together (strip leading zeros in the df_mob dataset to get the fips codes to join properly)
df_joined <- df_mob %>% left_join(mob_durations) %>%
  mutate(origin_county_id = str_remove(origin_county_id, "^0+"),
         destination_county_id = str_remove(destination_county_id, "^0+")) %>%
  left_join(origin_fips) %>%
  left_join(dest_fips) %>%
  left_join(pop %>% rename(origin_county_id=county_id, origin_pop=total_pop)) %>%
  left_join(pop %>% rename(destination_county_id=county_id, destination_pop=total_pop))

### what percent of the population is represented in mobility data? (check for Colorado)
# mobility data has CO -> CO and US -> CO but not US -> US
# so we don't know what percent of the population is measured by the mobility data
# but we can look at what proportion of Colorado's population is represented
# since we're only missing CO -> US, which presumably is lower than CO -> CO

co_pop <- 5807719

# by date overall
df_joined %>% 
  filter(origin_state == "Colorado") %>% 
  group_by(measure_date) %>% 
  summarize(device_count = sum(device_count)) %>%
  mutate(pct_surveilled = device_count / co_pop) %>%
  ggplot(aes(measure_date, pct_surveilled)) + geom_step(linewidth=1.0) +
  labs(title="Mean Proportion Surveilled Over Time\n(Devices Based in Colorado)", x="Date", y="Devices / Population")

# generate an overall estimate of the proportion of the population being sampled
frac_of_co_leaving_co <- 0.00
sample_frac <- df_joined %>% 
  filter(origin_state == "Colorado") %>% 
  group_by(measure_date) %>% 
  summarize(device_count = sum(device_count)) %>%
  mutate(pct_surveilled = device_count / (co_pop * (1-frac_of_co_leaving_co))) %>%
  ungroup() %>% summarize(mean_pct_surveilled = mean(pct_surveilled)) %>%
  as.numeric()
sample_frac

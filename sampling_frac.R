# FIGURING OUT THE SAMPLING FRACTION FOR DEVICES FROM THE SAFEGRAPH DATA #

# launch setup options
source("/Users/emwu9912/Documents/CU Anschutz/COVID-19 Modeling Group/Analysis/setup.R")

# get population data for counties
county_pops <- usdata::county_2019[c('fips', 'pop')] %>% mutate(fips = sprintf("%05d", fips))
county_names <- usdata::county_2019[c('fips', 'name')] %>% mutate(fips = sprintf("%05d", fips),
                                                                  name = stringr::str_replace_all(name, " County",""))

# download case data from NYT
case_files = c("us-counties-2020.csv",
               "us-counties-2021.csv",
               "us-counties-2022.csv")
for(fname in case_files){
  download.file(paste0("https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/", fname), fname)
  }
df_prev = do.call(rbind, lapply(case_files, function(fname){
  read.csv(fname, colClasses=list('date'='Date')) %>%
    mutate(county_id = substr(geoid, 5, length(geoid)))
  }))
  
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


df_prev$mob_date = do.call(c, lapply(df_prev$date, function(d) { mob_dates[which(d < mob_dates)[1]]}))

df_prev_agg <- df_prev %>% group_by(mob_date, state, county, county_id) %>% summarize(cases_avg = mean(cases_avg))

df_joined <- df_mob %>% left_join(
  mob_durations
) %>% left_join(
  df_prev_agg %>% rename(measure_date = mob_date, origin_state = state, origin_county_id = county_id, origin_county=county, origin_state = state, origin_cases=cases_avg)
) %>% left_join(
  df_prev_agg %>% rename(measure_date = mob_date, destination_state = state, destination_county_id = county_id, destination_county=county, destination_state = state, destination_cases=cases_avg)
) %>% left_join(
  pop %>% rename(origin_county_id=county_id, origin_pop=total_pop)
) %>% left_join(
  pop %>% rename(destination_county_id=county_id, destination_pop=total_pop)
)

### what percent of the population is represented in mobility data? (check for Colorado)
# mobility data has CO -> CO and US -> CO but not US -> US
# so we don't know what percent of the population is measured by the mobility data
# but we can look at what proportion of Colorado's population is represented
# since we're only missing CO -> US, which presumably is lower than CO -> CO

co_pop <- df_prev %>% 
  filter(state == "Colorado") %>%
  group_by(county) %>% summarize(county_id = max(county_id)) %>%
  left_join(county_pops %>% rename(county_id = fips)) %>% 
  summarize(pop = sum(pop, na.rm=T)) %>% as.numeric()

# by date overall
df_joined %>% 
  filter(origin_state == "Colorado") %>% 
  group_by(measure_date) %>% 
  summarize(device_count = sum(device_count)) %>%
  mutate(pct_surveilled = device_count / co_pop) %>%
  ggplot(aes(measure_date, pct_surveilled)) + geom_step(size=1.0) +
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

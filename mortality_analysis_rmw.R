####################################################
######## COVID-19 RMW ANALYSIS: MORTALITY ##########
#################### APRIL 2023 ####################
####################### EJW ########################
####################################################

# Purpose:
# To quantify in-hospital and out-of-hospital mortality
# parameters by age group and over time

#####################################################

# launch setup options
source("/Users/emwu9912/Documents/CU Anschutz/COVID-19 Modeling Group/Analysis/setup.R")

# read in and manipulate NCHS death data
raw <- read.csv("https://data.cdc.gov/resource/4va6-ph5s.csv?$limit=10000000000")

rmw <- raw %>%
  filter(state %in% c("Colorado", "Idaho", "Montana", "New Mexico", "Utah", "Wyoming"),) %>%
  select(year, month, state, place_of_death, age_group, covid_19_deaths)

# get the total death counts by age group at the year timescale (remove 2023)
year <- rmw %>%
  filter(is.na(month),
         year != 2023,
         place_of_death %in% c("Total - All Places of Death", "Healthcare setting, inpatient")) %>%
  pivot_wider(names_from = place_of_death, values_from = covid_19_deaths) %>%
  rename(total_deaths = `Total - All Places of Death`,
         inhosp_deaths = `Healthcare setting, inpatient`) %>%
  select(-month)

# first, we are going to get an age distribution of deaths for total deaths
# identify the states that have complete death data (i.e., no age groups are censored),
# or we can easily complete the death data (i.e. one age group is censored and we can just back-calculate)
year_totaldeaths <- year %>%
  group_by(year, state) %>%
  filter(sum(is.na(total_deaths)) <= 1) %>%
  select(-inhosp_deaths) #%>%
  #filter(age_group != "All Ages")

year_totaldeaths_neg <- year_totaldeaths %>%
  mutate(total_deaths = case_when(age_group == "All Ages" ~ total_deaths,
                                  TRUE ~ -total_deaths))

year_backcalc_totaldeaths <- year_totaldeaths_neg %>%
  group_by(year, state) %>%
  # sum down the group (ignoring NA's)
  summarize(total_deaths = sum(total_deaths, na.rm = T)) %>%
  # assign these counts to an age group (we'll call it "Censored")
  # this is now the leftover death count to be divided among the censored groups
  mutate(age_group = "Censored")

totaldeaths <- rbind(year_totaldeaths, year_backcalc_totaldeaths) %>%
  arrange(state, year, match(age_group, c("All Ages", "0-17 years", "18-29 years", "30-39 years",
                                          "40-49 years", "50-64 years", "65-74 years", "75-84 years",
                                          "85 years and over", "Censored"))) %>%
  mutate(across(total_deaths, ~ replace(., is.na(.),
                                        total_deaths[10]))) %>%
  filter(age_group != c("All Ages", "Censored"))

distribution_totaldeaths <- totaldeaths %>%
  group_by(age_group) %>%
  summarize(total_deaths = sum(total_deaths)) %>%
  mutate(prop_total_deaths = total_deaths/sum(total_deaths)) %>%
  select(-total_deaths)

# repeat for in-hospital deaths
year_inhospdeaths <- year %>%
  group_by(year, state) %>%
  filter(sum(is.na(inhosp_deaths)) <= 1) %>%
  select(-total_deaths)

year_inhospdeaths_neg <- year_inhospdeaths %>%
  mutate(inhosp_deaths = case_when(age_group == "All Ages" ~ inhosp_deaths,
                                  TRUE ~ -inhosp_deaths))

year_backcalc_inhospdeaths <- year_inhospdeaths_neg %>%
  group_by(year, state) %>%
  # sum down the group (ignoring NA's)
  summarize(inhosp_deaths = sum(inhosp_deaths, na.rm = T)) %>%
  # assign these counts to an age group (we'll call it "Censored")
  # this is now the leftover death count to be divided among the censored groups
  mutate(age_group = "Censored")

inhospdeaths <- rbind(year_inhospdeaths, year_backcalc_inhospdeaths) %>%
  arrange(state, year, match(age_group, c("All Ages", "0-17 years", "18-29 years", "30-39 years",
                                          "40-49 years", "50-64 years", "65-74 years", "75-84 years",
                                          "85 years and over", "Censored"))) %>%
  mutate(across(inhosp_deaths, ~ replace(., is.na(.),
                                        inhosp_deaths[10]))) %>%
  filter(age_group != c("All Ages", "Censored"))

distribution_inhospdeaths <- inhospdeaths %>%
  group_by(age_group) %>%
  summarize(inhosp_deaths = sum(inhosp_deaths)) %>%
  mutate(prop_inhosp_deaths = inhosp_deaths/sum(inhosp_deaths)) %>%
  select(-inhosp_deaths)

distribution_total <- ggplot(data = distribution_totaldeaths) +
  geom_bar(aes(x = age_group, y = prop_total_deaths),
           fill = "darkblue", stat = "identity", alpha = 0.8) +
  ggtitle("Age Distribution of Total Deaths, Rocky Mountain West") +
  labs(x = NULL, y = "Proportion of Total Deaths") +
  ggplot_theme; distribution_total
ggsave("./CSTE/Figures/distribution_total.png", height = 6, width = 12, plot = distribution_total)

distribution_inhosp <- ggplot(data = distribution_inhospdeaths) +
  geom_bar(aes(x = age_group, y = prop_inhosp_deaths),
           fill = "goldenrod3", stat = "identity", alpha = 0.8) +
  ggtitle("Age Distribution of In-Hospital Deaths, Rocky Mountain West") +
  labs(x = NULL, y = "Proportion of In-Hospital Deaths") +
  scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 0.1)) +
  ggplot_theme; distribution_inhosp
ggsave("./CSTE/Figures/distribution_inhosp.png", height = 6, width = 12, plot = distribution_inhosp)

# now, we are going to allocate the censored deaths in the other states according to this distribution
# identify the states where more than one age group is censored for total deaths
year_totaldeaths_censored <- year %>%
  group_by(year, state) %>%
  filter(sum(is.na(total_deaths)) > 1) %>%
  select(-inhosp_deaths)

year_totaldeaths_neg_censored <- year_totaldeaths_censored %>%
  mutate(total_deaths = case_when(age_group == "All Ages" ~ total_deaths,
                                  TRUE ~ -total_deaths))

year_backcalc_totaldeaths_censored <- year_totaldeaths_neg_censored %>%
  group_by(year, state) %>%
  # sum down the group (ignoring NA's)
  summarize(total_deaths = sum(total_deaths, na.rm = T)) %>%
  # assign these counts to an age group (we'll call it "Censored")
  # this is now the leftover death count to be divided among the censored groups
  mutate(age_group = "Censored")

totaldeaths_censored <- rbind(year_totaldeaths_censored, year_backcalc_totaldeaths_censored) %>%
  left_join(distribution_totaldeaths) %>%
  arrange(state, year, match(age_group, c("All Ages", "0-17 years", "18-29 years", "30-39 years",
                                          "40-49 years", "50-64 years", "65-74 years", "75-84 years",
                                          "85 years and over", "Censored")))

censored_props_totaldeaths <- totaldeaths_censored %>%
  filter(is.na(total_deaths)) %>%
  group_by(year, state) %>%
  summarize(censored_prop = sum(prop_total_deaths))

# IMPUTATION #1
totaldeaths_impute <- totaldeaths_censored %>%
  left_join(censored_props_totaldeaths) %>%
  group_by(year, state) %>%
  mutate(total_deaths = as.numeric(total_deaths),
         censored = total_deaths[10],
         total_deaths = case_when(is.na(total_deaths) ~ censored*prop_total_deaths/censored_prop,
                                   TRUE ~ total_deaths)) %>%
  filter(age_group != c("All Ages", "Censored")) %>%
  select(year, state, age_group, total_deaths)

totaldeaths_all <- rbind(totaldeaths, totaldeaths_impute) %>%
  arrange(state, year)

# repeat for in-hospital deaths
year_inhospdeaths_censored <- year %>%
  group_by(year, state) %>%
  filter(sum(is.na(inhosp_deaths)) > 1) %>%
  select(-total_deaths)

year_inhospdeaths_neg_censored <- year_inhospdeaths_censored %>%
  mutate(inhosp_deaths = case_when(age_group == "All Ages" ~ inhosp_deaths,
                                  TRUE ~ -inhosp_deaths))

year_backcalc_inhospdeaths_censored <- year_inhospdeaths_neg_censored %>%
  group_by(year, state) %>%
  # sum down the group (ignoring NA's)
  summarize(inhosp_deaths = sum(inhosp_deaths, na.rm = T)) %>%
  # assign these counts to an age group (we'll call it "Censored")
  # this is now the leftover death count to be divided among the censored groups
  mutate(age_group = "Censored")

inhospdeaths_censored <- rbind(year_inhospdeaths_censored, year_backcalc_inhospdeaths_censored) %>%
  left_join(distribution_inhospdeaths) %>%
  arrange(state, year, match(age_group, c("All Ages", "0-17 years", "18-29 years", "30-39 years",
                                          "40-49 years", "50-64 years", "65-74 years", "75-84 years",
                                          "85 years and over", "Censored")))

censored_props_inhospdeaths <- inhospdeaths_censored %>%
  filter(is.na(inhosp_deaths)) %>%
  group_by(year, state) %>%
  summarize(censored_prop = sum(prop_inhosp_deaths))

inhospdeaths_impute <- inhospdeaths_censored %>%
  left_join(censored_props_inhospdeaths) %>%
  group_by(year, state) %>%
  mutate(inhosp_deaths = as.numeric(inhosp_deaths),
         censored = inhosp_deaths[10],
         inhosp_deaths = case_when(is.na(inhosp_deaths) ~ censored*prop_inhosp_deaths/censored_prop,
                                   TRUE ~ inhosp_deaths)) %>%
  filter(age_group != c("All Ages", "Censored")) %>%
  select(year, state, age_group, inhosp_deaths)

alldeaths <- rbind(inhospdeaths, inhospdeaths_impute) %>%
  left_join(totaldeaths_all) %>%
  mutate(month = 0,
         # if in-hospital deaths exceed total deaths, set total deaths
         # equal to in-hospital deaths
         total_deaths = case_when(inhosp_deaths > total_deaths ~ inhosp_deaths,
                                  TRUE ~ total_deaths)) %>%
  arrange(state, year)
# you should now have a dataset for total and in-hospital deaths for each state and age group
# on the year timescale for 2020, 2021, and 2022 with no censored values

# now get the total death counts by age group at the month timescale (remove 2023)
month <- rmw %>%
  filter(!is.na(month),
         year != 2023)

# transpose to wide
month_wide <- reshape(month, idvar = c("state", "age_group", "year", "month"), timevar = "place_of_death", direction = "wide") %>%
  rename(total_deaths = `covid_19_deaths.Total - All Places of Death`,
         inhosp_deaths = `covid_19_deaths.Healthcare setting, inpatient`) %>%
  select(state, age_group, year, month, total_deaths, inhosp_deaths)

# stack the year dataset with the month dataset
yearmonth <- rbind(alldeaths, month_wide %>% filter(age_group != "All Ages")) %>%
  arrange(age_group)

# make all the individual age group deaths negative
# (because we will sum down in order to represent subtraction)
yearmonth_neg <- yearmonth %>%
  mutate(total_deaths = case_when(month == 0 ~ total_deaths,
                                  TRUE ~ -total_deaths),
         inhosp_deaths = case_when(month == 0 ~ inhosp_deaths,
                                   TRUE ~ -inhosp_deaths)) %>%
  arrange(year, state)

# sum down the states and months and give this month a name
# we'll call it 13 (this represents the censored counts)
yearmonth2 <- yearmonth_neg %>%
  group_by(state, year, age_group) %>%
  summarize(total_deaths = sum(total_deaths, na.rm = T),
            inhosp_deaths = sum(inhosp_deaths, na.rm = T)) %>%
  mutate(month = 13)

# stack and combine
yearmonth3 <- rbind(yearmonth, yearmonth2) %>%
  arrange(age_group, year, month) #%>%
  #select(year, month, age_group, total_deaths, inhosp_deaths) %>%
  #arrange(year, state, age_group)

# IMPUTATION #2
# start with the whole year death count for each age group
# subtract all the known month-specific counts for that year
# divide the rest evenly across the unknown months
yearmonth4 <- yearmonth3 %>%
  group_by(state, age_group, year) %>%
  mutate(total_deaths_nrow_na = sum(is.na(total_deaths)),
         inhosp_deaths_nrow_na = sum(is.na(inhosp_deaths)),
         across(total_deaths, ~ replace(., is.na(.),
                                        total_deaths[14]/total_deaths_nrow_na[14])),
         across(inhosp_deaths, ~ replace(., is.na(.),
                                         inhosp_deaths[14]/inhosp_deaths_nrow_na[14]))) %>%
  arrange(year, state) %>%
  select(state, year, month, age_group, total_deaths, inhosp_deaths)

# you should now have all the death counts for all states, age groups, and months with nothing censored
# drop month 0 (this represents the whole year) and month 13 (this represents the censored)
nchs_final <- yearmonth4 %>%
  filter(month %in% c(1:12)) %>%
  mutate(day = 1,
         date1 = mdy(paste0(month, "/", day, "/", year)),
         # if in-hospital deaths > total deaths, set total deaths equal to
         # in-hospital deaths, then calculate out-of-hospital deaths
         total_deaths = case_when(inhosp_deaths > total_deaths ~ inhosp_deaths,
                                  TRUE ~ total_deaths),
         age_group = case_when(age_group == "0-17 years" ~ "0-17",
                               age_group %in% c("18-29 years", "30-39 years", "40-49 years", "50-64 years") ~ "18-64",
                               age_group %in% c("65-74 years", "75-84 years", "85 years and over") ~ "65+",
                               TRUE ~ NA_character_),
         state = as.factor(state2abbr(state))) %>%
  drop_na(age_group) %>%
  group_by(state, date1, age_group) %>%
  summarize(total_deaths = sum(total_deaths),
            inhosp_deaths = sum(inhosp_deaths),
            outofhosp_deaths = total_deaths - inhosp_deaths) %>%
  select(state, age_group, date1, total_deaths, inhosp_deaths, outofhosp_deaths)

nchs_combined <- nchs_final %>%
  group_by(date1, age_group) %>%
  summarize(total_deaths = sum(total_deaths),
            inhosp_deaths = sum(inhosp_deaths),
            outofhosp_deaths = sum(outofhosp_deaths))

# read in and manipulate HHS hospitalization data
hosp_hhs <- read.csv("https://healthdata.gov/resource/g62h-syeh.csv?$limit=10000000000") %>%
  select(state, date,
         previous_day_admission_pediatric_covid_confirmed,
         previous_day_admission_adult_covid_confirmed_18_19,
         previous_day_admission_adult_covid_confirmed_20_29,
         previous_day_admission_adult_covid_confirmed_30_39,
         previous_day_admission_adult_covid_confirmed_40_49,
         previous_day_admission_adult_covid_confirmed_50_59,
         previous_day_admission_adult_covid_confirmed_60_69,
         previous_day_admission_adult_covid_confirmed_70_79,
         previous_day_admission_adult_covid_confirmed_80) %>%
  filter(state %in% c("CO", "ID", "MT", "NM", "UT", "WY")) %>%
  mutate(date = as.Date(date),
         month = month(date),
         day = 1,
         year = year(date),
         date1 = mdy(paste0(month, "/", day, "/", year))) %>%
  replace(is.na(.), 0) %>%
  mutate(nhosp017 = previous_day_admission_pediatric_covid_confirmed,
         nhosp1864 = previous_day_admission_adult_covid_confirmed_18_19 +
           previous_day_admission_adult_covid_confirmed_20_29 +
           previous_day_admission_adult_covid_confirmed_30_39 +
           previous_day_admission_adult_covid_confirmed_40_49 +
           previous_day_admission_adult_covid_confirmed_50_59 +
           previous_day_admission_adult_covid_confirmed_60_69/2,
         nhosp65p = previous_day_admission_adult_covid_confirmed_60_69/2 +
           previous_day_admission_adult_covid_confirmed_70_79 +
           previous_day_admission_adult_covid_confirmed_80) %>%
  select(state, date1, nhosp017, nhosp1864, nhosp65p) %>%
  filter(date1 >= "2020-07-01") %>%
  arrange(state, date1)

hosp_hhs_rmw_combined <- hosp_hhs %>%
  group_by(date1) %>%
  summarize(nhosp017 = sum(nhosp017),
            nhosp1864 = sum(nhosp1864),
            nhosp65p = sum(nhosp65p))

# transpose to long
hosp_hhs_long <- reshape2::melt(hosp_hhs_rmw_combined, id.vars = "date1") %>%
  mutate(age_group = case_when(variable == "nhosp017" ~ "0-17",
                               variable == "nhosp1864" ~ "18-64",
                               variable == "nhosp65p" ~ "65+")) %>%
  rename(nhosp = value) %>%
  select(date1, age_group, nhosp)

hosps <- nchs_combined %>%
  select(date1, age_group, inhosp_deaths)

dh_all <- merge(hosps, hosp_hhs_long, c("date1", "age_group"), all = T) %>%
  rename(ndied = inhosp_deaths) %>%
  mutate(dh = ndied/nhosp,
         age_group = as.factor(age_group)) %>%
  filter(date1 >= as.Date("2020-07-01"),
         date1 <= as.Date("2022-12-01"))

# read in COPHS hospitalization data from DUA
hosp_data_cophs <- read.csv("./CDPHE DUA Data/cophs_hospitalization_data.csv")

# clean up anomalies from hospitalization data
hosp1_cophs <- hosp_data_cophs %>%
  mutate(age = floor(as.numeric(difftime(Sys.Date(), dob, units = "weeks"))/52.25),
         admit_date = as.Date(hospital_admission_date),
         discharge_date = as.Date(discharge_transfer_death_date)) %>%
  # drop observations with missing admit and/or discharge dates
  drop_na(admit_date, discharge_date) %>%
  filter(admit_date >= "2020-03-01",
         # for this analysis, go until June 30th, 2020
         admit_date <= "2020-06-30") %>%
  # add 1 day to the discharge date to account for day of discharge and same-day discharge patients
  mutate(dis_date_adj = discharge_date + 1,
         age_group = as.factor(case_when(age <= 17 ~ "0-17",
                                         age >= 18 & age <= 64 ~ "18-64",
                                         age >= 65 ~ "65+",
                                         TRUE ~ NA_character_)),
         los = as.numeric(difftime(dis_date_adj, admit_date, unit = "days")),
         # create a synthetic admit date that is on the first of the month to plot summary LOS by month
         month = month(admit_date),
         year = year(admit_date),
         day = 1,
         admit_date1 = mdy(paste0(month, "/", day, "/", year))) %>%
  # drop any observations with a length of stay that is not possible (i.e. negative or greater than the length of the pandemic)
  filter(los >= 1,
         los <= as.numeric(difftime(as.Date("2020-06-30"), as.Date("2020-03-01")))) 

hosp2_cophs <- hosp1_cophs %>%
  # standardize entries for gender
  mutate(gender = case_when(gender %in% c("f", "F", "Female") ~ "Female",
                            gender %in% c("m", "M", "Male") ~ "Male",
                            gender %in% c("other", "Other") ~ "Other",
                            TRUE ~ "Unknown")) %>%
  # drop individuals whose zip code is either missing or 
  filter(nchar(zip_code)==5) %>%
  # convert ZIP codes to numeric (it will drop the leading zeros but that's OK
  # because we're just linking on them, not using the actual values)
  # note: you will get a message saying NA's were introduced by coercion
  mutate(zip_code = as.numeric(zip_code)) %>%
  # drop invalid ZIP codes
  filter(!zip_code %in% c(0, 1, 99999)) %>%
  # drop NA ZIP codes (these were turned into NA's during the conversion to
  # numeric because they were letters)
  drop_na(zip_code) %>%
  # link observations by DOB, gender, and ZIP code
  add_count(dob, gender, zip_code) %>%
  group_by(dob, gender, zip_code) %>%
  # create a unique identifier for each patient
  mutate(ID = cur_group_id()) %>%
  select(ID, facility_name, admit_date, admit_date1, discharge_date,
         age, age_group, dob, gender, zip_code, discharge_transfer_death_disposition, n) %>%
  # arrange by patient ID, and within each patient ID, admit date
  arrange(ID, admit_date)

hosp3_cophs <- hosp2_cophs %>%
  group_by(ID) %>%
  arrange(ID, admit_date) %>%
  # calculate the number of days between the last discharge date and the next admit date
  mutate(diff = as.numeric(admit_date - lag(discharge_date, default = first(admit_date))),
         los_raw = as.numeric(difftime(discharge_date, admit_date, unit = "days"))) %>%
  # drop any observations with a negative difference (these are data entry errors)
  filter(diff >= 0) %>%
  # reconsolidate the observations into their respective counts
  add_count(dob, gender, zip_code)

# for patients with only one hospital visit, treat as independent events
# for patients who are readmitted within 30 days, combine readmissions into a single hospitalization
# for patients who are readmitted outside of 30 days, treat those as independent events
hosp4_cophs <- hosp3_cophs %>%
  group_by(ID) %>%
  mutate(los = case_when(n == 1 ~ los_raw,
                         n > 1 & diff > 30 ~ los_raw,
                         # temporarily set readmissions within 30 days to have a LOS of 999
                         TRUE ~ 999))

# create a new dataset where instead of 999, we set to NA
# then drop those observations out of this new dataset (we will recombine afterwards)
los1 <- hosp4_cophs %>%
  mutate(los = case_when(los != 999 ~ los)) %>%
  drop_na(los)

# create a new dataset with patients that are readmitted within 30 days
los2 <- hosp4_cophs %>%
  filter(los == 999) %>%
  group_by(ID) %>%
  # add up the lengths of stay for each patient
  mutate(los = sum(los_raw)) %>%
  # the sums will repeatedly show up, so just take the first observation for each ID
  slice(1)

# recombine
hosp_final <- rbind(los1, los2) %>%
  select(ID, admit_date, admit_date1, discharge_date, age_group, discharge_transfer_death_disposition)

hosp_cumul <- hosp_final %>%
  filter(admit_date1 <= as.Date("2020-07-01")) %>%
  group_by(admit_date1, age_group) %>%
  count() %>%
  rename(totalhosps = n,
         date1 = admit_date1)

nchs_final_spring2020_co <- nchs_final %>%
  filter(state == "CO",
         date1 <= as.Date("2020-06-01"),
         date1 >= as.Date("2020-03-01")) %>%
  select(date1, age_group, inhosp_deaths) %>%
  left_join(hosp_cumul) %>%
  mutate(dh = inhosp_deaths/totalhosps)

# make dh plots for the three age groups
dh_ages <- list()
ages <- unique(dh_all$age_group)
for (i in 1:length(ages)){
  dh_ages[[i]] <- ggplot() +
    geom_rect(aes(xmin = as.Date("2020-02-01"), xmax = as.Date("2020-08-01"),
                  ymin = 0, ymax = Inf), fill = "goldenrod1", alpha = 0.2) +
    geom_rect(aes(xmin = as.Date("2020-08-01"), xmax = as.Date("2021-08-01"),
                  ymin = 0, ymax = Inf), fill = "tomato", alpha = 0.2) +
    geom_rect(aes(xmin = as.Date("2021-08-01"), xmax = as.Date("2022-04-01"),
                  ymin = 0, ymax = Inf), fill = "dodgerblue2", alpha = 0.2) +
    geom_rect(aes(xmin = as.Date("2022-04-01"), xmax = as.Date("2023-01-01"),
                  ymin = 0, ymax = Inf), fill = "violetred", alpha = 0.2) +
    annotate("text", x = as.Date("2020-02-25"), y = 0.4, angle = 90, label = "early pandemic") +
    annotate("text", x = as.Date("2020-09-15"), y = 0.4, angle = 90, label = "wildtype/Alpha") +
    annotate("text", x = as.Date("2021-09-15"), y = 0.4, angle = 90, label = "Delta") +
    annotate("text", x = as.Date("2022-05-01"), y = 0.4, angle = 90, label = "Omicron") +
    geom_bar(data = dh_all %>%
               filter(age_group == ages[i]), aes(x = date1, y = dh, fill = "color2"),
             stat = "identity", alpha = 0.8) +
    geom_bar(data = nchs_final_spring2020_co %>%
               filter(age_group == ages[i]), aes(x = date1, y = dh, fill = "color1"),
             stat = "identity", alpha = 0.8) +
    labs(x = NULL, y = "Death Fraction Among Hospitalized") +
    #labs(x = NULL, y = NULL) +
    ggtitle(paste0(ages[i])) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "4 months") +
    scale_y_continuous(limits = c(0, 0.61), breaks = seq(0, 0.6, 0.2)) +
    scale_fill_manual(labels = c("color1" = "COPHS hospitalization data",
                                     "color2" = "HHS hospitalization data"),
                          values = c("color1" = "royalblue4",
                                     "color2" = "forestgreen")) +
    guides(fill = guide_legend(override.aes = list(linewidth = 8))) +
    ggplot_theme; dh_ages
  ggsave(paste0("./CSTE/Figures/dh_ages", i, ".png"), height = 7, width = 12, plot = dh_ages[[i]])
}

# now figure out what the in-hospital mortality parameters should be as a result of this analysis
# the best approach would be to determine the means for each of the four timeline sections
# first time stamp is 01/01/2020
# second time stamp is 07/01/2020 (when HHS data begins and mortality rates decrease)
# third time stamp is 09/01/2021 (when Delta was fully dominant, rounded to the nearest first of the month)
# third time stamp is 02/01/2022 (when Omicron was fully dominant, rounded to the nearest first of the month)
dh_cophs_hhs <- rbind(dh_all %>% mutate(state = "RMW"), nchs_final_spring2020_co %>%
                        rename(ndied = inhosp_deaths,
                               nhosp = totalhosps)) %>%
  select(-state) %>%
  arrange(date1)

dh_summary_sections <- dh_cophs_hhs %>%
  mutate(timestamp = case_when(date1 <= "2020-06-01" ~ as.Date("2020-01-01"),
                                   date1 >= "2020-08-01" & date1 <= "2021-08-01" ~ as.Date("2020-08-01"),
                                   date1 >= "2021-09-01" & date1 <= "2022-03-01" ~ as.Date("2021-09-01"),
                                   TRUE ~ as.Date("2022-04-01"))) %>%
  group_by(age_group, timestamp) %>%
  # for each of our new timestamps, compute the mean death fraction
  summarize(dh_param = mean(dh)) %>%
  ungroup()

# OUT-OF-HOSPITAL MORTALITY # 

# the case dataset is huge, so the read-in will hit the timeout of 60 seconds
getOption('timeout')
# set the timeout to 100000 seconds
options(timeout = 100000)

# read in case data with geography from CDC (it will take 20-30 minutes)
case_data <- read.csv("https://data.cdc.gov/resource/n8mc-b4w4.csv?$limit=10000000000")

case_data_rmw_indiv <- case_data %>%
  filter(res_state %in% c("CO", "ID", "MT", "NM", "UT", "WY")) %>%
  rename(state = res_state) %>%
  mutate(age_group = case_when(age_group == "0 - 17 years" ~ "0-17",
                               age_group %in% c("18 to 49 years", "50 to 64 years") ~ "18-64",
                               age_group == "65+ years" ~ "65+",
                               TRUE ~ NA_character_),
         month = substr(case_month, 6, 7),
         day = 1,
         year = substr(case_month, 1, 4),
         date1 = mdy(paste0(month, "/", day, "/", year))) %>%
  # drop unknown age group (~1% of the dataset)
  drop_na(age_group) %>% arrange(year, month)

case_data_rmw <- case_data_rmw_indiv %>%
  filter(date1 <= "2022-12-01") %>%
  group_by(date1, age_group) %>%
  count() %>%
  rename(ncases = n)

dnh_all <- case_data_rmw %>%
  left_join(nchs_combined %>% select(-total_deaths, -inhosp_deaths)) %>%
  mutate(dnh = outofhosp_deaths/ncases)

dnh_ages <- list()
ages <- unique(dnh_all$age_group)
for (i in 1:length(ages)){
  dnh_ages[[i]] <- ggplot() +
    geom_rect(aes(xmin = as.Date("2020-02-01"), xmax = as.Date("2020-08-01"),
                  ymin = 0, ymax = Inf), fill = "goldenrod1", alpha = 0.2) +
    geom_rect(aes(xmin = as.Date("2020-08-01"), xmax = as.Date("2021-08-01"),
                  ymin = 0, ymax = Inf), fill = "tomato", alpha = 0.2) +
    geom_rect(aes(xmin = as.Date("2021-08-01"), xmax = as.Date("2022-04-01"),
                  ymin = 0, ymax = Inf), fill = "dodgerblue2", alpha = 0.2) +
    geom_rect(aes(xmin = as.Date("2022-04-01"), xmax = as.Date("2023-01-01"),
                  ymin = 0, ymax = Inf), fill = "violetred", alpha = 0.2) +
    annotate("text", x = as.Date("2020-02-25"), y = 0.2, angle = 90, label = "early pandemic") +
    annotate("text", x = as.Date("2020-09-15"), y = 0.2, angle = 90, label = "wildtype/Alpha") +
    annotate("text", x = as.Date("2021-09-15"), y = 0.2, angle = 90, label = "Delta") +
    annotate("text", x = as.Date("2022-05-01"), y = 0.2, angle = 90, label = "Omicron") +
    geom_bar(data = dnh_all %>%
               filter(age_group == ages[i]), aes(x = date1, y = dnh), fill = "darkblue",
             stat = "identity", alpha = 0.8) +
    labs(x = "Case Report Month", y = "Death Fraction Among Non-Hospitalized") +
    #labs(x = NULL, y = NULL) +
    ggtitle(paste0(ages[i])) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "4 months") +
    scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1)) +
    guides(fill = guide_legend(override.aes = list(linewidth = 8))) +
    ggplot_theme; dnh_ages
  ggsave(paste0("./CSTE/Figures/dnh_ages", i, ".png"), height = 7, width = 12, plot = dnh_ages[[i]])
}

dnh_summary_sections <- dnh_all %>%
  mutate(timestamp = case_when(date1 <= "2020-06-01" ~ as.Date("2020-01-01"),
                               date1 >= "2020-08-01" & date1 <= "2021-08-01" ~ as.Date("2020-08-01"),
                               date1 >= "2021-09-01" & date1 <= "2022-03-01" ~ as.Date("2021-09-01"),
                               TRUE ~ as.Date("2022-04-01"))) %>%
  group_by(age_group, timestamp) %>%
  # for each of our new timestamps, compute the mean death fraction
  summarize(dnh_param = mean(dnh)) %>%
  #mutate(timestamp_original = as.Date(c("2020-01-01", "2020-07-01", "2020-09-30", "2020-12-01"))) %>%
  ungroup()

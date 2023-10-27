##########################################################################
######## COVID-19 RMW ANALYSIS: POPULATION-WEIGHTING PARAMETERS ##########
################################ MAY 2023 ################################
################################## EJW ###################################
##########################################################################

# Purpose:
# To population-weight vaccine efficacy and hospitalization rates to the age
# distribution of the RMW

##########################################################################

# launch setup options
source("/Users/emwu9912/Documents/CU Anschutz/COVID-19 Modeling Group/Analysis/setup.R")

# get the filenames of all the population csv's
list_csv_files <- list.files("CSTE/Data Sets/rmw_population", pattern = "ByCountyAge.csv", full.names = T)
df <- do.call(rbind, lapply(list_csv_files, function(x)
  read.csv(x, stringsAsFactors = F)))

rmw <- 14694790

pop_rmw_ve <- df %>%
  group_by(Age.Group) %>%
  summarize(pop = sum(Population)) %>%
  pivot_wider(names_from = Age.Group, values_from = pop) %>%
  mutate(`0-11` = `0-4 years` + `5-9 years` + `10-14 years`*2/5,
         `12-15` = `10-14 years`*3/5 + `15-19 years`*1/5,
         `16-44` = `15-19 years`*4/5 + `20-24 years` + `25-29 years` +
           `30-34 years` + `35-39 years` + `40-44 years`,
         `45-64` = `45-49 years` + `50-54 years` + `55-59 years` +
           `60-64 years `) %>%
  select(`0-11`, `12-15`, `16-44`, `45-64`) %>%
  pivot_longer(1:4, names_to = "age_bin", values_to = "pop")

ve_rmw <- as.data.frame(pop_rmw_ve %>%
  mutate(pop = round(pop),
         weight = pop/sum(pop),
         shot1_ve = case_when(age_bin == "0-11" ~ 0.67,
                              age_bin == "12-15" ~ 0.67,
                              age_bin == "16-44" ~ 0.62,
                              age_bin == "45-64" ~ 0.55),
         shot2_ve = case_when(age_bin == "0-11" ~ 0.91,
                              age_bin == "12-15" ~ 0.91,
                              age_bin == "16-44" ~ 0.73,
                              age_bin == "45-64" ~ 0.73),
         shot1_ve_share = shot1_ve*weight,
         shot2_ve_share = shot2_ve*weight) %>%
  summarize(shot1_ve_064 = sum(shot1_ve_share),
            shot2_ve_064 = sum(shot2_ve_share)) %>%
  mutate(booster1_ve_064 = shot2_ve_064,
         booster23_ve_064 = booster1_ve_064,
         across(where(is.numeric), round, 2)))

ve_rmw

pop_rmw_reese <- df %>%
  group_by(Age.Group) %>%
  summarize(pop = sum(Population)) %>%
  pivot_wider(names_from = Age.Group, values_from = pop) %>%
  mutate(`0-4` = `0-4 years`,
         `5-17` = `5-9 years` + `10-14 years` + `15-19 years`*3/5,
         `18-49` = `15-19 years`*2/5 + `20-24 years` + `25-29 years` + `30-34 years` + `35-39 years` + `40-44 years` + `45-49 years`,
         `50-64` = `50-54 years` + `55-59 years` + `60-64 years `,
         `65+` = `65-69 years` + `70-74 years` + `75-79 years` + `80-84 years` + `85+ years`) %>%
  select(`0-4`, `5-17`, `18-49`, `50-64`, `65+`) %>%
  pivot_longer(1:5, names_to = "age_bin", values_to = "pop")

hosp_rmw_reese <- as.data.frame(pop_rmw_reese %>%
                             mutate(pop = round(pop),
                                    hosp = case_when(age_bin == "0-4" ~ 0.0182,
                                                   age_bin == "5-17" ~ 0.0094,
                                                   age_bin == "18-49" ~ 0.0260,
                                                   age_bin == "50-64" ~ 0.07196,
                                                   age_bin == "65+" ~ 0.2244),
                                    age_group = case_when(age_bin %in% c("0-4", "5-17") ~ "0-17",
                                                          age_bin %in% c("18-49", "50-64") ~ "18-64",
                                                          TRUE ~ "65+")) %>%
                                      group_by(age_group) %>%
                                      mutate(weight = pop/sum(pop),
                                             hosp_share = hosp*weight) %>%
                                      summarize(hosp_weighted = sum(hosp_share)) %>%
                                      mutate(across(where(is.numeric), round, 4)))

hosp_rmw_reese

##########################################################################
                             

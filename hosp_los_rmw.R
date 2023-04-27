### INVESTIGATING AVERAGE HOSPITAL LENGTH OF STAY ###

# launch setup options
source("/Users/emwu9912/Documents/CU Anschutz/COVID-19 Modeling Group/Analysis/setup.R")

# source BigQuery DUA credentials
#source("./Admin Documents/Training and Access/bq_credentials_dua.R")

# read in COPHS hospitalization data from BigQuery
#sql <- "SELECT * FROM `raw.cophs_hospitalization_data`"
#tb <- bq_project_query(x="co-covid-models-dua", query=sql)
#hosp <- bq_table_download(tb)

hosp <- read.csv("./CDPHE DUA Data/cophs_hospitalization_data.csv")

# clean up anomalies from hospitalization data
hosp1 <- hosp %>%
  mutate(age = floor(as.numeric(difftime(Sys.Date(), dob, units = "weeks"))/52.25),
         admit_date = as.Date(hospital_admission_date),
         discharge_date = as.Date(discharge_transfer_death_date)) %>%
  # drop observations with missing admit and/or discharge dates
  drop_na(admit_date, discharge_date) %>%
  filter(admit_date >= "2020-03-01",
         discharge_date <= "2023-01-31") %>%
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
         los <= as.numeric(difftime(as.Date("2023-01-31"), as.Date("2020-03-01")))) 

# limit the dataset only to Colorado residents
# grab the CO counties from usdata library
counties <- as.data.frame(usdata::county_2019 %>%
                            filter(state == "Colorado") %>%
                            mutate(county = str_sub(name, 1, str_length(name)-7)) %>%
                            mutate_if(is.character, str_to_upper) %>%
                            select(county))
# create a vector that lists the CO counties
counties_char <- as.character(counties[,])

hosp2 <- hosp1 %>%
  # standardize entries for gender
  mutate(gender = case_when(gender %in% c("f", "F", "Female") ~ "Female",
                            gender %in% c("m", "M", "Male") ~ "Male",
                            gender %in% c("other", "Other") ~ "Other",
                            TRUE ~ "Unknown")) %>%
  # drop individuals with non-Colorado or invalid ZIP code
  filter(substr(zip_code, 1, 1) == "8") %>%
  # drop individuals with a non-Colorado or invalid county
  mutate(county = case_when(county_of_residence %in% counties_char ~ county_of_residence,
                            TRUE ~ NA_character_)) %>%
  drop_na(county) %>%
  # link observations by DOB, gender, ZIP code, and county of residence
  add_count(dob, gender, zip_code, county) %>%
  group_by(dob, gender, zip_code, county) %>%
  # create a unique identifier for each patient
  mutate(ID = cur_group_id()) %>%
  select(ID, facility_name, admit_date, admit_date1, discharge_date, age, age_group, dob, gender, zip_code, county, discharge_transfer_death_disposition, n) %>%
  # arrange by patient ID, and within each patient ID, admit date
  arrange(ID, admit_date)

# analyze repeat hospitalizations

# summarize the number of observations that have given counts
readmit_count <- as.data.frame(table(hosp2$n)) %>%
  mutate(n_admissions = as.numeric(Var1),
         Freq = as.numeric(Freq)) %>%
  select(n_admissions, Freq)

# change row 11 to 14 (numeric vector wants to count one by one)
readmit_count[11,1] <- 14

# calculate the number of patients with a given number of admissions by
# dividing the number of observations by the number of admissions
readmit_count <- readmit_count %>%
  mutate(n_patients = Freq/n_admissions) %>%
  rename(n_obs = Freq)

# let's look at the people who were admitted more than once
readmit <- hosp2 %>%
  filter(n > 1)

time_to_readmit <- readmit %>%
  group_by(ID) %>%
  arrange(ID, admit_date) %>%
  mutate(diff = as.numeric(admit_date - lag(discharge_date, default = first(admit_date)))) %>%
  # drop any observations with a negative difference (these are data entry errors)
  filter(diff >= 0,
         diff <= 30)

readmit_patients <- time_to_readmit %>%
  group_by(ID) %>%
  slice(1) %>%
  arrange(diff)

nrow <- scales::label_comma()(nrow(time_to_readmit))
nrow_patients <- scales::label_comma()(nrow(readmit_patients))

# plot a histogram of the time to readmission (for readmissions within 30 days)
readmit_hist <- ggplot(data = time_to_readmit) +
  geom_histogram(aes(x = diff), binwidth = 1, color = "white", fill = "royalblue") +
  geom_vline(aes(xintercept = mean(diff)), color = "goldenrod", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(diff)), color = "chartreuse2", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(time_to_readmit$diff) + 1, y = 5400,
           angle = 90, label = paste("mean time to readmission", round(mean(time_to_readmit$diff), 1), "days"), color = "goldenrod", size = 6) +
  annotate("text", x = median(time_to_readmit$diff) + 1.2, y = 5400,
           angle = 90, label = paste("median time to readmission", median(time_to_readmit$diff), "days"), color = "chartreuse2", size = 6) +
  labs(x = "Number of Days to Readmission", y = "Count") +
  ggtitle("Distribution of Time to COVID-19 Re-Hospitalization Within 30 Days",
          subtitle = paste(nrow, "Hospitalizations Among Colorado Residents, November 2020 to January 2023")) +
  scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, 2000), labels = scales::comma) +
  ggplot_theme +
  theme(axis.text.x = element_text(size = 18, angle = 0, margin=margin(10, 0, 20, 0, "pt"))); readmit_hist
ggsave("./CSTE/Figures/time_to_readmit.png", height = 13, width = 15, plot = readmit_hist)

# look at the age distribution of readmissions within 30 days
readmit_hist_age <- ggplot(data = readmit_patients) +
  geom_histogram(aes(x = age, fill = "color1"), binwidth = 1, color = "white") +
  geom_density(aes(x = age, y = after_stat(count)/n*10000, fill = "color2"), alpha = 0.4) +
  geom_vline(aes(xintercept = mean(age)), color = "goldenrod", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(age)), color = "dodgerblue2", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(readmit_patients$age) - 4, y = 300,
           angle = 90, label = paste("mean age", round(mean(readmit_patients$age), 1), "years"), color = "goldenrod", size = 6) +
  annotate("text", x = median(readmit_patients$age) + 3, y = 300,
           angle = 90, label = paste("median age", median(readmit_patients$age), "years"), color = "dodgerblue2", size = 6) +
  labs(x = "Age", y = "Count") +
  ggtitle("Age Distribution of COVID-19 Hospital Patients Readmitted Within 30 Days",
          subtitle = paste(nrow_patients, "Colorado Residents Hospitalized, November 2020 to January 2023")) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 100),
                     sec.axis = sec_axis(trans ~. /10000, name = "Share of All Hospitalizations")) +
  scale_fill_manual(values = c("color1" = "forestgreen", "color2" = "turquoise"),
                    labels = c("Count", "Proportion")) +
  ggplot_theme +
  theme(axis.text.x = element_text(size = 18, angle = 0, margin=margin(10, 0, 20, 0, "pt")),
        axis.title.y.right = element_text(margin = margin(0, 0, 0, 30, "pt"))); readmit_hist_age
ggsave("./CSTE/Figures/readmit_age_density.png", height = 10, width = 20, plot = readmit_hist_age)

# now that we've investigated the readmissions, let's take the following approach
# for patients with only one hospital visit, treat as independent events
# for patients who are readmitted within 30 days, combine lengths of stay
# for patients who are readmitted outside of 30 days, treat those as independent events
hosp3 <- hosp2 %>%
  group_by(ID) %>%
  arrange(ID, admit_date) %>%
  # calculate the number of days between the last discharge date and the next admit date
  mutate(diff = as.numeric(admit_date - lag(discharge_date, default = first(admit_date))),
         los_raw = as.numeric(difftime(discharge_date, admit_date, unit = "days"))) %>%
  # drop any observations with a negative difference (these are data entry errors)
  filter(diff >= 0) %>%
  # reconsolidate the observations into their respective counts
  add_count(dob, gender, zip_code, county)

los <- hosp3 %>%
  group_by(ID) %>%
  mutate(los = case_when(n == 1 ~ los_raw,
                         n > 1 & diff > 30 ~ los_raw,
                         # temporarily set readmissions within 30 days to have a LOS of 999
                         TRUE ~ 999))

# create a new dataset where instead of 999, we set to NA
# then drop those observations out of this new dataset (we will recombine afterwards)
los1 <- los %>%
  mutate(los = case_when(los != 999 ~ los)) %>%
  drop_na(los)

# create a new dataset with patients that are readmitted within 30 days
los2 <- los %>%
  filter(los == 999) %>%
  group_by(ID) %>%
  # add up the lengths of stay for each patient
  mutate(los = sum(los_raw)) %>%
  # the sums will repeatedly show up, so just take the first observation for each ID
  slice(1)

# recombine
los_final <- rbind(los1, los2) %>%
  select(ID, facility_name, age_group, admit_date1, los)

# get summary stats
los_summary <- as.data.frame(los_final %>%
  group_by(age_group, admit_date1) %>%
  summarize(mean_los = mean(los),
            median_los = median(los)))

# subset into the three age groups
df_ages <- list()
ages <- levels(los_summary$age_group)
for(i in 1:length(ages)) {
  df_ages[[i]] <- los_summary %>%
    filter(age_group == ages[i]) %>%
    rename(date = admit_date1)
  assign(paste0("df_ages", i), df_ages[[i]], envir = .GlobalEnv)
}

# now create hospital census to overlay LOS plots
# subset the data into each of the three age groups
df_age <- list()
df_age2 <- list()
for (i in 1:length(ages)){
  df_age[[i]] <- hosp1 %>%
    filter(age_group == ages[i])
  df_age2[[i]] <- df_age[[i]] %>%
    mutate(patient = seq(1:nrow(df_age[[i]])))
  setDT(df_age2[[i]])
  assign(paste0("df_age2_", i), df_age2[[i]], envir = .GlobalEnv)
}

# create a table with all dates
dt_dates <- list()
pre_census <- list()
census <- list()
admdates <- list(df_age2_1$admit_date, df_age2_2$admit_date, df_age2_3$admit_date)
disdates <- list(df_age2_1$dis_date_adj, df_age2_2$dis_date_adj, df_age2_3$dis_date_adj)

# run for loop to create census for all three age groups
for (i in 1:length(ages)){
  dt_dates[[i]] <- data.table(date = seq(min(admdates[[i]]), max(disdates[[i]]), by = "1 days"))
  pre_census[[i]] <- df_age2[[i]][dt_dates[[i]], .(date, patient),
                                  on = .(admit_date <= date, dis_date_adj > date), nomatch = 0L]
  census[[i]] <- as.data.frame(pre_census[[i]][, .(hospitalized = uniqueN(patient)), by = date])
  assign(paste0("census", i), census[[i]], envir = .GlobalEnv)
}

# merge to create three datasets that have date, median length of stay for that month, and the patient count on each day
df_los <- list()
for (i in 1:length(ages)){
  df_los[[i]] <- merge(df_ages[i], census[i], "date", all = TRUE) %>%
    select(date, median_los, hospitalized) %>%
    mutate(median_los_all = zoo::na.locf(median_los),
           hospitalized = hospitalized/100)
  #assign(paste0("df_los", i), df_los[[i]], envir = .GlobalEnv)
}

hosp_los_plot <- list()
for (i in 1:length(ages)){
  hosp_los_plot[[i]] <- ggplot(data = los_summary %>%
                                 filter(age_group == ages[i])) +
    geom_bar(aes(x = admit_date1, y = median_los), fill = "darkorange3", stat = "identity", alpha = 0.8) +
  #  annotate("rect", xmin = as.Date("2020-11-20"), xmax = as.Date("2022-11-15"), alpha = 0.4,
  #           ymin = 0, ymax = Inf, fill = "darkred") +
  #  annotate("rect", xmin = as.Date("2021-12-10"), xmax = as.Date("2022-12-31"), alpha = 0.4,
  #           ymin = 0, ymax = Inf, fill = "grey3") +
  #  annotate("text", x = as.Date("2021-02-10"), y = 7.5, label = "mAb ramp up", size = 8) +
  #  annotate("text", x = as.Date("2022-03-15"), y = 7.5, label = "omicron takeover", size = 8) +
    labs(x = NULL, y = NULL) +
    #labs(x = "Hospital Admission Month", y = "Median Hospital Length of Stay (Days)") +
    #      caption = paste("Last Updated", Sys.Date())) +
    ggtitle(ages[i]) +
    scale_x_date(date_labels="%b %Y", date_breaks="4 months", limits = as.Date(c("2020-03-01", "2022-12-31"))) +
    scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 2)) +
    guides(fill = guide_legend(override.aes = list(size = 8))) +
    ggplot_theme; hosp_los_plot
  ggsave(paste0("./CSTE/Figures/hosp_los_plot", i, ".png"), height = 6, width = 12, plot = hosp_los_plot[[i]])
}

hosp_los_plot_mean <- list()
for (i in 1:length(ages)){
  hosp_los_plot_mean[[i]] <- ggplot(data = los_summary %>%
                                 filter(age_group == ages[i])) +
    geom_bar(aes(x = admit_date1, y = mean_los), fill = "dodgerblue4", stat = "identity", alpha = 0.8) +
    #annotate("rect", xmin = as.Date("2020-11-20"), xmax = as.Date("2022-11-15"), alpha = 0.4,
    #         ymin = 0, ymax = Inf, fill = "darkred") +
    #annotate("rect", xmin = as.Date("2021-12-10"), xmax = as.Date("2022-12-31"), alpha = 0.4,
    #         ymin = 0, ymax = Inf, fill = "grey3") +
    #annotate("text", x = as.Date("2021-02-10"), y = 18, label = "mAb ramp up", size = 8) +
    #annotate("text", x = as.Date("2022-03-15"), y = 18, label = "omicron takeover", size = 8) +
    labs(x = NULL, y = NULL) +
    # labs(x = "Hospital Admission Month", y = "Mean Hospital Length of Stay",
    #      caption = paste("Last Updated", Sys.Date())) +
    ggtitle(ages[i]) +
    scale_x_date(date_labels="%b %Y", date_breaks="4 months", limits = as.Date(c("2020-03-01", "2022-12-31"))) +
    scale_y_continuous(limits = c(0, 23), breaks = seq(0, 20, 5)) +
    guides(fill = guide_legend(override.aes = list(size = 8))) +
    ggplot_theme; hosp_los_plot_mean
  ggsave(paste0("./CSTE/Figures/hosp_los_plot_mean", i, ".png"), height = 6, width = 12, plot = hosp_los_plot_mean[[i]])
}

hosp_los_overlay_plot <- list()
# plot hospital length of stay over time in the Colorado data
for (i in 1:length(ages)){
  # call back our empty list so we can store all our outputs
  hosp_los_overlay_plot[[i]] <- ggplot(data = df_los[[i]]) +
    geom_bar(aes(x = date, y = hospitalized, fill = "color1"), stat = "identity", alpha = 0.8) +
    geom_bar(aes(x = date, y = median_los_all, fill = "color2"), stat = "identity", alpha = 0.6) +
    labs(x = NULL, y = NULL) +
    # labs(x = "Hospital Admission Month", y = "Median Hospital Length of Stay",
    #      caption = paste("Last Updated", Sys.Date())) +
    ggtitle(ages[i]) +
    scale_x_date(date_labels="%b %Y", date_breaks="2 months", limits = as.Date(c("2020-03-01", "2022-12-31"))) +
    scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 3),
                       sec.axis = sec_axis(trans ~. *100, labels = scales::comma)) +
    scale_fill_manual(values = c("color1" = "grey3",
                                 "color2" = "chartreuse3"),
                      labels = c("color1" = "Hospital Census",
                                 "color2" = "Median Length of Stay")) +
    guides(fill = guide_legend(override.aes = list(size = 8))) +
    ggplot_theme; hosp_los_overlay_plot
  ggsave(paste0("./CSTE/Figures/hosp_los_overlay_plot", i, ".png"), height = 10, width = 18, plot = hosp_los_overlay_plot[[i]])
}

# now figure out what the length of stay parameters should be as a result of this analysis
# the best approach would be to determine the medians of the means for each of the three timeline sections
# first time stamp is 01/01/2020
# second time stamp is 12/01/2020 (when mAb's first became available, rounded to the nearest first of the month)
# third time stamp is 12/01/2021 (when Omicron started to take over, rounded to the nearest first of the month)
los_summary_sections <- los_summary %>%
  mutate(timestamp_new = case_when(admit_date1 <= "2020-11-01" ~ as.Date("2020-01-01"),
                                admit_date1 >= "2020-12-01" & admit_date1 <= "2021-11-01" ~ as.Date("2020-12-01"),
                                TRUE ~ as.Date("2021-12-01"))) %>%
  group_by(age_group, timestamp_new) %>%
  # for each of our new timestamps, compute the median of the means
  summarize(los_param_new = median(mean_los)) %>%
  #mutate(timestamp_original = as.Date(c("2020-01-01", "2020-08-01", "2020-12-03"))) %>%
  ungroup() %>%
  # manually input the original LOS parameters
  mutate(los_param_original = c(5.8303, 5.5747, 5.5747, 8.505509701, 6.898889621, 6.898889621,
                        10.53665902, 8.136581647,8.136581647)) %>%
  # reorder variables
  select(age_group, timestamp_original, los_param_original, timestamp_new, los_param_new)













       
# until we discuss this as a team, let's treat every hospitalization as an independent event for now


# EXTRA CODE BELOW #



# look at the age distribution of readmissions within 30 days
#readmit_patients$county_of_residence <- as.ordered(readmit_patients$county_of_residence)
#readmit_patients_county <- readmit_patients %>%
#group_by(county_of_residence) %>%
#add_count(county_of_residence) %>%
#mutate(weight = 1/nn


#readmit_patients$weight <- 1/nrow(readmit_patients)
#readmit_hist_county <- ggplot(data = readmit_patients) +
#geom_col(aes(x = county_of_residence, y = weight), color = "darkorchid") +
#geom_vline(aes(xintercept = mean(age)), color = "goldenrod", linetype = "dashed", linewidth = 1) +
#geom_vline(aes(xintercept = median(age)), color = "dodgerblue2", linetype = "dashed", linewidth = 1) +
#annotate("text", x = mean(time_to_readmit$age) - 4, y = 700,
#angle = 90, label = paste("mean age", round(mean(time_to_readmit$age), 1), "years"), color = "goldenrod", size = 6) +
#annotate("text", x = median(time_to_readmit$age) + 3, y = 700,
#angle = 90, label = paste("median age", median(time_to_readmit$age), "years"), color = "dodgerblue2", size = 6) +
#labs(x = "County of Residence", y = "Share of Total Hospitalizations") +
#ggtitle("Distribution of County of Residence Among COVID-19 Hospital Patients Readmitted Within 30 Days",
#subtitle = paste(nrow, "Hospitalizations Among Colorado Residents, November 2020 to January 2023")) +
#scale_y_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, 0.03)) +
#ggplot_theme +
#theme(axis.text.x = element_text(size = 12, angle = 90, margin=margin(10, 0, 20, 0, "pt"))); readmit_hist_county
#ggsave("./CSTE/Figures/readmit_county.png", height = 10, width = 20, plot = readmit_hist_county)



#labs(x = NULL, y = NULL) +
# labs(x = "Hospital Admission Month", y = "Median Hospital Length of Stay",
#      caption = paste("Last Updated", Sys.Date())) +
#ggtitle(ages[i]) +
#scale_x_date(date_labels="%b %Y", date_breaks="2 months", limits = as.Date(c("2020-03-01", "2022-12-31"))) +
#scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
#guides(fill = guide_legend(override.aes = list(size = 8))) +
#ggplot_theme; hosp_los_plot
#ggsave(paste0("./CSTE/Figures/hosp_los_plot", i, ".png"), height = 10, width = 18, plot = hosp_los_plot[[i]])







# side analysis to look for repeat hospitalizations

#hosp_readmit <- hosp1 %>%
  #add_count(dob, gender, zip_code, county_of_residence, race, ethnicity) #%>%
  #filter(discharge_transfer_death_disposition == "Another Hospital") %>%
  #arrange(dob, gender, zip_code, county_of_residence, race, ethnicity, discharge_transfer_death_disposition) %>%
  #select(facility_name, dob, hospital_admission_date, discharge_transfer_death_date, discharge_transfer_death_disposition, los, n) %>%
  #filter(n > 1,
        # discharge_transfer_death_disposition == "Another Hospital")
  #filter(n > 1)

# frequency table for number of times a patient has been admitted
#table(hosp_readmit$n)
#as.data.frame(table(hosp_readmit$n))

# the add_count function counts the number of occurrences something has,
# but it repeats that number for however many occurrences there are
# for example, someone who has been to the hospital 8 times will have the number 8, 8 times
# so, we need to divide that number by the number of times it shows up
# to get a count of the actual patients who have shown up n times at the hospital









# create a clustered bar chart of length of stay for each age group and phase of the pandemic
# los_bar <- ggplot(data = df_los) +
#   geom_bar(aes(x = age_group, y = mean_los, fill = phase), position = "dodge", stat = "identity")+
#   labs(x = "Age Group", y = "Mean Hospital Length of Stay (Days)",
#        caption = paste("Last Updated", Sys.Date())) +
#   ggtitle("Average Length of Stay Among Hospitalized COVID-19 Cases") + 
#   guides(fill=guide_legend(nrow=3, override.aes = list(size = 8))) +
#   ggplot_theme; los_bar
# ggsave("./Figures/Hospitalization Analysis/los_bar.png", height = 10, width = 15, plot = los_bar)


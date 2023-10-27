####################################################
####### COVID-19 RMW ANALYSIS: THERAPEUTICS ########
################### JANUARY 2023 ###################
####################### EJW ########################
####################################################

# Purpose:
# To estimate the prevalence of Paxlovid use among
# symptomatic cases as estimated by the model using
# real data on Paxlovid administration

#####################################################

# launch setup options
source("/Users/emwu9912/Documents/CU Anschutz/COVID-19 Modeling Group/Analysis/setup.R")

# read in the Paxlovid data (manually entered from HHS database)
rmw_pax <- read.csv("./CSTE/Data Sets/rmw_paxlovid.csv") %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

# supply and demand plots (all states)

# empty lists to store wide and long datasets
df_100k <- list()
df_100k_long <- list()

# list of strings to select for delivered or administered
k <- list("del_100k", "adm_100k")

for (i in 1:length(k)){
  # select the variables for either delivered or administered
  df_100k[[i]] <- rmw_pax %>%
    select(date, contains(k[[i]]))
  # transpose to long
  df_100k_long[[i]] <- reshape2::melt(df_100k[[i]], id.vars = "date")
}

# create lists to index over for captions and titles
caption <- list("delivered", "administered")
title <- list("Delivered", "Administered")

# empty list to store ggplot outputs
adm_del <- list()

# plot delivered and administered
for (i in 1:length(df_100k)) {
  # call back our empty list so we can store all our outputs
  adm_del[[i]] <- ggplot(data = df_100k_long[[i]]) +
    geom_line(aes(x = date, y = value, group = variable, color = variable), linewidth = 1.3) +
    annotate("rect", xmin = as.Date("2021-12-23"), xmax = as.Date("2022-09-04"), ymin = 0, ymax = Inf, alpha = 0.4) +
    annotate("text", x = as.Date("2022-04-15"), y = 3300, size = 7, label =
    # index over the caption list to create our captions
    paste("interpolated region assuming 20% of
    cumulative doses were", caption[[i]], "at a
    constant rate between 12/23/2021 and 04/26/2022,
    and 80% of cumulative doses were", caption[[i]], "at
    a constant rate between 04/26/2022 and 09/04/2022")) +
    labs(x = NULL, y = "Courses per 100,000") +
    # index over the title list to create our titles
    ggtitle(paste("Cumulative Patient Courses of Paxlovid", title[[i]], "to Date")) +
    scale_x_date(breaks="6 weeks", limits = as.Date(c("2021-12-22", "2023-01-22")), date_labels="%m/%d/%Y") +
    scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, 1000), labels = scales::comma) +
    guides(color = guide_legend(override.aes = list(linewidth = 8))) +
    scale_color_manual(labels = c("Colorado", "Idaho", "Montana",
                                  "New Mexico", "Utah", "Wyoming"),
                       values = c("royalblue2", "firebrick3", "goldenrod1", "darkorchid",
                                              "darkorange4", "turquoise3")) +
                                                ggplot_theme + theme(plot.caption = element_text(color = "blue")); adm_del
  # now write the plots to the database using the strings for delivered or administered
  ggsave(paste0("./CSTE/Figures/pax_", k[[i]], ".png"), height = 10, width = 15, plot = adm_del[[i]])
}

# supply and demand plots for each state

# create lists to index over for the individual states
states <- c("co", "id", "mt", "nm", "ut", "wy")
# list of full state names for plot titles
states_full <- c("Colorado", "Idaho", "Montana", "New Mexico", "Utah", "Wyoming")

# empty lists to store wide and long datasets
state_df <- list()
state_df_long <- list()

for (i in 1:length(states)){
  # select datasets for each state that contain variables per 100k
  state_df[[i]] <- rmw_pax %>%
    select(date, contains(paste0("_", states[i], "_"))) %>%
    select(date, contains("100k"))
  # transpose to long
  state_df_long [[i]] <- reshape2::melt(state_df[[i]], id.vars = "date")
}

# empty list to store ggplot outputs
pax_plot_states <- list()

# plot delivered and administered for each state
for (i in 1:length(state_df_long)) {
  # call back our empty list so we can store all our outputs
  pax_plot_states[[i]] <- ggplot(data = state_df_long[[i]]) +
    geom_line(aes(x = date, y = value, group = variable, color = variable), linewidth = 1.5) +
    annotate("rect", xmin = as.Date("2021-12-23"), xmax = as.Date("2022-09-04"), ymin = 0, ymax = Inf, alpha = 0.4) +
    labs(x = NULL, y = "Courses per 100,000")+
    # index over the names of the states to create the titles
    ggtitle(states_full[[i]]) +
    scale_x_date(breaks="2 months", limits = as.Date(c("2021-12-23", "2023-01-22")), date_labels="%m/%d/%Y") +
    scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, 1000), labels = scales::comma) +
    guides(color = guide_legend(override.aes = list(linewidth = 8))) +
    scale_color_manual(labels = c("Delivered", "Administered"),
                       values = c("royalblue2", "firebrick3")) +
                                                ggplot_theme + theme(plot.caption = element_text(color = "blue")); pax_plot_states
  # now write the plots to the database using the state abbreviations
  ggsave(paste0("./CSTE/Figures/pax_plot_", states[i], ".png"), height = 7, width = 10, plot = pax_plot_states[[i]])
}

# Paxlovid in the model vs. real life

# read in the RMW model output dates 02/02/2023
out2 <- read.csv("./CSTE/Data Sets/combined_out2.csv") %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))

# subset into the individual states
out2_df <- list()
for(i in 1:length(states)){
  out2_df[[i]] <- out2 %>%
    select(date, region, Inew) %>%
    filter(substr(region, 1, 2) == states[[i]]) %>%
    group_by(date) %>%
    summarize(Inew = sum(Inew))
}

# compare model-estimated symptomatic infections to model-estimated and actual Paxlovid use

# create a data frame with the date stamps and prevalence for model-estimated Paxlovid
pax_date <- as.Date(c("2020-01-24", "2021-12-28", "2022-01-04", "2022-01-11",
                      "2022-01-18", "2022-01-25", "2022-02-01", "2022-02-08",
                      "2022-02-13", "2022-03-07", "2022-03-14", "2022-03-20"))
pax_prev <- c(0, 0.0026714285714285716, 0.007328571428571428, 0.012385714285714286,
              0.030000000000000002, 0.05332857142857143, 0.07667142857142857, 0.09666,
              0.1067, 0.13332857142857144, 0.17666666666666667, 0.2)
pax <- data.frame(pax_date, pax_prev)

# select the raw variables from the HHS Paxlovid dataset
rmw_pax_raw <- rmw_pax %>%
  select(date, contains("raw"))

# calculate the model-estimated Paxlovid use by multiplying by our time-varying parameter for Paxlovid prevalence
pax_df <- list()
for(i in 1:length(out2_df)){
  pax_df[[i]] <- out2_df[[i]] %>%
    filter(date >= "2021-12-23",
           date <= "2023-01-22") %>%
    mutate(pax_model = case_when(date < pax[2,1] ~ Inew*pax[1,2], date >= pax[2,1] & date < pax[3,1] ~ Inew*pax[2,2],
                                 date >= pax[3,1] & date < pax[4,1] ~ Inew*pax[3,2], date >= pax[4,1] & date < pax[5,1] ~ Inew*pax[4,2],
                                 date >= pax[5,1] & date < pax[6,1] ~ Inew*pax[5,2], date >= pax[6,1] & date < pax[7,1] ~ Inew*pax[6,2],
                                 date >= pax[7,1] & date < pax[8,1] ~ Inew*pax[7,2], date >= pax[8,1] & date < pax[9,1] ~ Inew*pax[8,2],
                                 date >= pax[9,1] & date < pax[10,1] ~ Inew*pax[9,2], date >= pax[10,1] & date < pax[11,1] ~ Inew*pax[10,2],
                                 date >= pax[11,1] & date < pax[12,1] ~ Inew*pax[11,2], date >= pax[12,1] ~ Inew*pax[12,2]),
           Icumul = cumsum(Inew),
           pax_model_cumul = cumsum(pax_model)) %>%
    left_join(rmw_pax_raw) %>%
    select(date, Inew, Icumul, pax_model, pax_model_cumul, contains(paste0("_", states[[i]], "_adm_raw")))
  assign(paste0("pax_df_", states[[i]]), pax_df[[i]], envir = .GlobalEnv)
  }

# first make a list of the raw columns we want to interpolate
raw_cols <- list(pax_df_co[,6], pax_df_id[,6], pax_df_mt[,6],
                 pax_df_nm[,6], pax_df_ut[,6], pax_df_wy[,6])

# perform linear interpolation for cumulative Paxlovid doses administered (because HHS does not have daily data)
df1 <- list()
df2 <- list()
df3 <- list()
for (i in 1:length(pax_df)){
  df1[[i]] <- as.data.frame(na_interpolation(raw_cols[[i]], option = "linear"))
  df2[[i]] <- as.data.frame(lapply(df1[[i]], function(x) diff(c(0, x)))) %>%
    # rename column to daily Paxlovid
    rename(daily_pax = 1)
  df3[[i]] <- cbind(pax_df[[i]], df1[[i]], df2[[i]]) %>%
    select(-6) %>%
    # rename cumulative raw Paxlovid variable to generic name
    rename(cumul_pax_raw = 6)
}

# plot Paxlovid use in the model (cumulative) for each state
pax_df_cumul <- list()
pax_df_cumul_long <- list()

for (i in 1:length(df3)){
  pax_df_cumul[[i]] <- df3[[i]] %>%
    select(date, Icumul, pax_model_cumul, cumul_pax_raw)
  pax_df_cumul_long[[i]] <- reshape2::melt(pax_df_cumul[[i]], id.vars = "date")
}

pax_model_cumul <- list()
# plot model-estimated symptomatic infections with model-estimated and actual Paxlovid use over time
for (i in 1:length(pax_df_cumul_long)) {
  # call back our empty list so we can store all our outputs
  pax_model_cumul[[i]] <- ggplot(data = pax_df_cumul_long[[i]]) +
    geom_bar(aes(x = date, y = value, group = variable, fill = variable, alpha = variable),
             stat = "identity", position = "identity") +
    labs(x = "Onset Date", y = "Cumulative Infections or Paxlovid Courses") +
    ggtitle(states_full[[i]]) +
    scale_x_date(date_labels="%m/%d/%y", date_breaks="6 weeks", limits=as.Date(c("2021-12-22", "2023-01-22"))) +
    scale_y_continuous(labels=scales::comma) +
    scale_fill_manual(values=c("gray30", "darkblue", "goldenrod1"),
                      labels=c("Model Estimated Infections (Symptomatic Only)", "Model Estimated Paxlovid Use",
                               "Actual Paxlovid Use")) +
    scale_alpha_manual(values=c(0.3, 0.8, 1),
                       labels=c("Model Estimated Infections (Symptomatic Only)", "Model Estimated Paxlovid Use",
                                "Actual Paxlovid Use")) +
    guides(fill = guide_legend(nrow = 3, override.aes = list(linewidth = 8))) +
    ggplot_theme; pax_model_cumul
  ggsave(paste0("./CSTE/Figures/pax_model_cumul_", states[[i]], ".png"), height = 10, width = 15, plot = pax_model_cumul[[i]])
}

# plot Paxlovid use in the model (daily) for each state
pax_df_daily <- list()
pax_df_daily_long <- list()

for (i in 1:length(df3)){
  pax_df_daily[[i]] <- df3[[i]] %>%
    select(date, Inew, pax_model, daily_pax)
  pax_df_daily_long[[i]] <- reshape2::melt(pax_df_daily[[i]], id.vars = "date")
}

pax_model_daily <- list()
# plot model-estimated symptomatic infections with model-estimated and actual Paxlovid use over time
for (i in 1:length(pax_df_daily_long)) {
  # call back our empty list so we can store all our outputs
  pax_model_daily[[i]] <- ggplot(data = pax_df_daily_long[[i]]) +
    geom_bar(aes(x = date, y = value, group = variable, fill = variable, alpha = variable),
             stat = "identity", position = "identity") +
    labs(x = "Onset Date", y = "Daily Infections or Paxlovid Courses") +
    ggtitle(states_full[[i]]) +
    scale_x_date(date_labels="%m/%d/%y", date_breaks="6 weeks", limits=as.Date(c("2021-12-22", "2023-01-22"))) +
    scale_y_continuous(labels=scales::comma) +
    scale_fill_manual(values=c("gray30", "darkblue", "goldenrod1"),
                      labels=c("Model Estimated Infections (Symptomatic Only)", "Model Estimated Paxlovid Use",
                               "Actual Paxlovid Use")) +
    scale_alpha_manual(values=c(0.3, 0.8, 1),
                       labels=c("Model Estimated Infections (Symptomatic Only)", "Model Estimated Paxlovid Use",
                                "Actual Paxlovid Use")) +
    guides(fill = guide_legend(nrow = 3, override.aes = list(linewidth = 8))) +
    ggplot_theme; pax_model_daily
  ggsave(paste0("./CSTE/Figures/pax_model_daily_", states[[i]], ".png"), height = 10, width = 15, plot = pax_model_daily[[i]])
}

# now plot the proportion of daily Paxlovid use among symptomatic (model-estimated and observed)
prop <- list()
prop_long <- list()
for(i in 1:length(df3)){
  prop[[i]] <- df3[[i]] %>%
    mutate(prop_model = pax_model/Inew,
           prop_observed = daily_pax/Inew) %>%
    select(date, prop_model, prop_observed)
  prop_long[[i]] <- reshape2::melt(prop[[i]], id.vars = "date")
}

# plot proportion Paxlovid use (model estimated vs. observed)
pax_prop <- list()
for(i in 1:length(prop_long)){
  pax_prop[[i]] <- ggplot(data = prop_long[[i]]) +
    geom_line(aes(x = date, y = value, group = variable, color = variable), linewidth = 1.5) +
    ggtitle(states_full[[i]])+
    labs(x = "Onset Date", y = "Proportion of Daily Symptomatic Infections Treated") +
    scale_x_date(date_labels="%m/%d/%y", date_breaks="6 weeks", limits=as.Date(c("2021-12-22", "2023-01-22"))) +
    scale_y_continuous(limits=c(0, 0.4), breaks=seq(0, 0.4, 0.05)) +
    scale_color_manual(labels = c("Model Estimated Paxlovid Use",
                                  "Actual Paxlovid Use"),
                       values = c("forestgreen", "turquoise2")) +
    guides(color = guide_legend(override.aes = list(linewidth = 8))) +
    ggplot_theme; pax_prop
  ggsave(paste0("./CSTE/Figures/pax_prop_", states[[i]], ".png"), height = 10, width = 15, plot = pax_prop[[i]])
}

# now repeat the above exercise, but this time doing the region as a whole

out2_rmw <- out2 %>%
  group_by(date) %>%
  summarize(Inew_total = sum(Inew))

# compare model-estimated symptomatic infections to model-estimated and actual Paxlovid use

# create a data frame with the date stamps and prevalence for model-estimated Paxlovid
pax_date <- as.Date(c("2020-01-24", "2021-12-28", "2022-01-04", "2022-01-11",
                      "2022-01-18", "2022-01-25", "2022-02-01", "2022-02-08",
                      "2022-02-13", "2022-03-07", "2022-03-14", "2022-03-20"))
pax_prev <- c(0, 0.0026714285714285716, 0.007328571428571428, 0.012385714285714286,
              0.030000000000000002, 0.05332857142857143, 0.07667142857142857, 0.09666,
              0.1067, 0.13332857142857144, 0.17666666666666667, 0.2)
pax <- data.frame(pax_date, pax_prev)

# select the raw variables from the HHS Paxlovid dataset
rmw_pax_raw_agg <- rmw_pax %>%
  select(date, contains("adm_raw")) %>%
  mutate(cumul_pax_rmw_adm_raw = rowSums(across(where(is.numeric)))) %>%
  select(date, cumul_pax_rmw_adm_raw)

# calculate the model-estimated Paxlovid use for the whole RMW by multiplying by our time-varying parameter for Paxlovid prevalence
pax_df_rmw1 <- out2_rmw %>%
  filter(date >= "2021-12-23",
         date <= "2023-01-22") %>%
  mutate(pax_model = case_when(date < pax[2,1] ~ Inew_total*pax[1,2], date >= pax[2,1] & date < pax[3,1] ~ Inew_total*pax[2,2],
                               date >= pax[3,1] & date < pax[4,1] ~ Inew_total*pax[3,2], date >= pax[4,1] & date < pax[5,1] ~ Inew_total*pax[4,2],
                               date >= pax[5,1] & date < pax[6,1] ~ Inew_total*pax[5,2], date >= pax[6,1] & date < pax[7,1] ~ Inew_total*pax[6,2],
                               date >= pax[7,1] & date < pax[8,1] ~ Inew_total*pax[7,2], date >= pax[8,1] & date < pax[9,1] ~ Inew_total*pax[8,2],
                               date >= pax[9,1] & date < pax[10,1] ~ Inew_total*pax[9,2], date >= pax[10,1] & date < pax[11,1] ~ Inew_total*pax[10,2],
                               date >= pax[11,1] & date < pax[12,1] ~ Inew_total*pax[11,2], date >= pax[12,1] ~ Inew_total*pax[12,2]),
         Icumul = cumsum(Inew_total),
         pax_model_cumul = cumsum(pax_model)) %>%
  left_join(rmw_pax_raw_agg)

pax_df_rmw2 <- as.data.frame(na_interpolation(pax_df_rmw1[,6], option = "linear"))
pax_df_rmw3 <- as.data.frame(lapply(pax_df_rmw2, function(x) diff(c(0, x)))) %>%
  rename(daily_pax = 1)
pax_df_rmw <- cbind(pax_df_rmw1, pax_df_rmw2, pax_df_rmw3) %>%
  select(-6)

rmw_pax_plot <- ggplot(data = rmw_pax_raw_agg) +
  geom_line(aes(x = date, y = cumul_pax_rmw_adm_raw), color = "salmon", linewidth = 1.3) +
  annotate("rect", xmin = as.Date("2021-12-23"), xmax = as.Date("2022-09-04"), ymin = 0, ymax = Inf, alpha = 0.4) +
  annotate("text", x = as.Date("2022-04-15"), y = 160000, size = 7, label =
             # index over the caption list to create our captions
             paste("interpolated region assuming 20% of
    cumulative doses were administered at a
    constant rate between 12/23/2021 and 04/26/2022,
    and 80% of cumulative doses were administered at
    a constant rate between 04/26/2022 and 09/04/2022")) +
  labs(x = NULL, y = "Cumulative Number of Doses Given") +
  # index over the title list to create our titles
  ggtitle("Estimate of Cumulative Patient Doses of Paxlovid Administered to Date",
          subtitle = "Rocky Mountain West") +
  scale_x_date(breaks="6 weeks", limits = as.Date(c("2021-12-22", "2023-01-22")), date_labels="%m/%d/%Y") +
  scale_y_continuous(labels = scales::comma) +
  guides(color = guide_legend(override.aes = list(linewidth = 8))) +
  ggplot_theme + theme(plot.caption = element_text(color = "blue")); rmw_pax_plot
# now write the plots to the database using the strings for delivered or administered
ggsave("./CSTE/Figures/rmw_pax_plot.png", height = 8, width = 18, plot = rmw_pax_plot)

# plot Paxlovid use in the model (cumulative) for the RMW
pax_df_cumul_rmw <- pax_df_rmw %>%
  select(date, Icumul, cumul_pax_rmw_adm_raw)
pax_df_cumul_rmw_long <- reshape2::melt(pax_df_cumul_rmw, id.vars = "date")

pax_model_cumul_rmw <- ggplot(data = pax_df_cumul_rmw_long) +
  geom_bar(aes(x = date, y = value, group = variable, fill = variable, alpha = variable),
           stat = "identity", position = "identity") +
  labs(x = "Onset Date", y = "Cumulative Infections or Paxlovid Courses") +
  ggtitle("Rocky Mountain West") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks="6 weeks", limits=as.Date(c("2021-12-22", "2023-01-22"))) +
  scale_y_continuous(limits = c(0, 8000000), breaks = seq(0, 8000000, 2000000), labels=scales::comma) +
  scale_fill_manual(values=c("gray30", "darkblue"),
                    labels=c("Model Estimated Infections (Symptomatic Only)", "Actual Paxlovid Use")) +
  scale_alpha_manual(values=c(0.3, 0.8, 1),
                     labels=c("Model Estimated Infections (Symptomatic Only)", "Actual Paxlovid Use")) +
  guides(fill = guide_legend(nrow = 3, override.aes = list(linewidth = 8))) +
  ggplot_theme; pax_model_cumul_rmw
ggsave("./CSTE/Figures/pax_model_cumul_rmw.png", height = 10, width = 15, plot = pax_model_cumul_rmw)

# plot Paxlovid use in the model (daily) for the RMW
pax_df_daily_rmw <- pax_df_rmw %>%
  select(date, Inew_total, daily_pax)
pax_df_daily_rmw_long <- reshape2::melt(pax_df_daily_rmw, id.vars = "date")

pax_model_daily_rmw <- ggplot(data = pax_df_daily_rmw_long) +
  geom_bar(aes(x = date, y = value, group = variable, fill = variable, alpha = variable),
           stat = "identity", position = "identity") +
  labs(x = "Onset Date", y = "Daily Infections or Paxlovid Doses") +
  ggtitle("Model-Estimated Daily New Symptomatic Infections and Daily Paxlovid Administration",
  subtitle = "Rocky Mountain West") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks="6 weeks", limits=as.Date(c("2021-12-22", "2023-01-22"))) +
  scale_y_continuous(labels=scales::comma) +
  scale_fill_manual(values=c("gray30", "darkblue"),
                    labels=c("Model Estimated Infections (Symptomatic Only)", "Actual Paxlovid Use")) +
  scale_alpha_manual(values=c(0.3, 0.8, 1),
                     labels=c("Model Estimated Infections (Symptomatic Only)", "Actual Paxlovid Use")) +
  guides(fill = guide_legend(nrow = 3, override.aes = list(linewidth = 8))) +
  ggplot_theme; pax_model_daily_rmw
ggsave("./CSTE/Figures/pax_model_daily_rmw.png", height = 8, width = 16, plot = pax_model_daily_rmw)

# now plot the proportion of daily Paxlovid use among symptomatic (model-estimated and observed)
prop_rmw <- pax_df_rmw %>%
  mutate(prop_observed = daily_pax/Inew_total) %>%
  select(date, prop_observed)
prop_rmw_long <- reshape2::melt(prop_rmw, id.vars = "date")

# plot proportion Paxlovid use (model estimated vs. observed)
pax_prop_rmw <- ggplot(data = prop_rmw_long) +
  geom_line(aes(x = date, y = value), color = "forestgreen", linewidth = 1.5) +
  annotate("rect", xmin = as.Date("2022-04-27"), xmax = as.Date("2023-01-22"), alpha = 0.4,
           ymin = 0, ymax = Inf, fill = "goldenrod") +
  annotate("rect", xmin = as.Date("2022-09-04"), xmax = as.Date("2023-01-22"), alpha = 0.4,
           ymin = 0, ymax = Inf, fill = "darkorchid") +
  annotate("text", x = as.Date("2022-05-15"), y = 0.25, angle = 90, label = "Paxlovid becomes more\nwidely available", size = 6) +
  annotate("text", x = as.Date("2022-09-15"), y = 0.25, angle = 90, label = "demand stabilizes", size = 6) +
  ggtitle("Estimated Proportion of Model-Estimated Symptomatic Infections Treated with Paxlovid") +
  labs(x = "Onset Date", y = "Proportion") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks="6 weeks", limits=as.Date(c("2021-12-22", "2023-01-22"))) +
  scale_y_continuous(limits=c(0, 0.4), breaks=seq(0, 0.4, 0.05)) +
  ggplot_theme; pax_prop_rmw
ggsave("./CSTE/Figures/pax_prop_rmw.png", height = 8, width = 16, plot = pax_prop_rmw)

prop_rmw_sections <- prop_rmw %>%
  mutate(timestamp = case_when(date <= "2022-04-27" ~ as.Date("2021-12-23"),
                                   date >= "2022-04-28" & date <= "2022-09-04" ~ as.Date("2022-04-27"),
                                   TRUE ~ date("2022-09-04"))) %>%
  group_by(timestamp) %>%
  summarize(pax_prev = median(prop_observed))
  
#####################################################



# MOBILITY INFORMED SEEDING ALGORITHM

# launch setup options
source("/Users/emwu9912/RProjects/covid-analysis/setup.R")
# launch sampling fraction algorithm
#source("./CSTE/Mobility-Informed Seeding/sampling_frac.R")
# if you don't want to run the sampling fraction algorithm again, just import this value
sample_frac <- 0.0673167

# read in SafeGraph mobility seed data
sg_seed <- fread("./CSTE/Data Sets/cste_seed.csv.gz")
# get population data for counties
county_pops <- usdata::county_2019[c('fips', 'pop')] %>% mutate(fips = sprintf("%05d", fips))
county_names <- usdata::county_2019[c('fips', 'name')] %>% mutate(fips = sprintf("%05d", fips),
                                                                  name = stringr::str_replace_all(name, " County",""))

# read in county fips data from usdata library
county_fips <- usdata::county_2019[c('fips', 'name', 'state')] %>%
  mutate(dest_county = str_remove_all(name, " County")) %>%
  rename(dest_state = state,
         dest_fips = fips) %>%
  select(dest_fips, dest_county, dest_state)
# read in state fips data from tigris library
state_fips <- tigris::fips_codes[c('state', 'state_code')] %>%
  mutate(origin_fips = as.numeric(state_code),
         origin_state = as.factor(state)) %>%
  select(origin_fips, origin_state) %>%
  # remove duplicates that result from removing the county
  distinct()

# merge county and state fips data into mobility seed data
sg_seed1 <- merge(merge(sg_seed, county_fips, "dest_fips"), state_fips, "origin_fips") %>%
  # assign regions
  mutate(dest_region = case_when(dest_county %in% c("Baca", "Bent", "Cheyenne", "Crowley", "Custer", "Fremont", "Huerfano",
                                                    "Kiowa", "Kit Carson", "Las Animas", "Lincoln", "Logan", "Morgan", "Otero", "Phillips",
                                                    "Prowers", "Pueblo", "Sedgwick", "Washington", "Yuma") & dest_state == "Colorado" ~ "Colorado East",
                                 dest_county %in% c("Adams", "Arapahoe", "Boulder", "Broomfield", "Chaffee", "Clear Creek", "Denver", "Douglas", "Elbert",
                                                    "El Paso", "Gilpin", "Grand", "Jefferson", "Lake", "Larimer", "Park", "Summit", "Teller",
                                                    "Weld") & dest_state == "Colorado" ~ "Colorado North",
                                 dest_county %in% c("Alamosa", "Archuleta",  "Conejos", "Costilla", "Delta", "Dolores", "Eagle",
                                                    "Garfield", "Gunnison", "Hinsdale", "Jackson", "La Plata", "Mesa", "Mineral", "Moffat",
                                                    "Montezuma", "Montrose", "Ouray", "Pitkin", "Rio Blanco", "Rio Grande", "Routt",
                                                    "Saguache", "San Juan", "San Miguel") & dest_state == "Colorado" ~ "Colorado West",
                                 dest_county %in% c("Bannock", "Bear Lake", "Bingham", "Bonneville", "Butte", "Caribou", "Clark", "Custer", "Franklin",
                                                    "Fremont", "Jefferson", "Lemhi", "Madison", "Oneida", "Power", "Teton") & dest_state == "Idaho" ~ "Idaho East",
                                 dest_county %in% c("Benewah", "Bonner", "Boundary", "Clearwater", "Idaho", "Kootenai", "Latah", "Lewis", "Nez Perce",
                                                    "Shoshone") & dest_state == "Idaho" ~ "Idaho North",
                                 dest_county %in% c("Blaine", "Camas", "Cassia", "Gooding", "Jerome", "Lincoln", "Minidoka",
                                                    "Twin Falls") & dest_state == "Idaho" ~ "Idaho South",
                                 dest_county %in% c("Ada", "Adams", "Boise", "Canyon", "Elmore", "Gem", "Owyhee", "Payette", "Valley", "Washington") & dest_state == "Idaho" ~ "Idaho West",
                                 dest_county %in% c("Big Horn", "Carbon", "Carter", "Custer", "Daniels", "Dawson", "Fallon", "Fergus", "Garfield",
                                                    "Golden Valley", "Judith Basin", "McCone", "Musselshell", "Petroleum", "Phillips",
                                                    "Powder River", "Prairie", "Richland", "Roosevelt", "Rosebud", "Sheridan", "Stillwater",
                                                    "Sweet Grass", "Treasure", "Valley", "Wheatland", "Wibaux", "Yellowstone") & dest_state == "Montana" ~ "Montana East",
                                 dest_county %in% c("Blaine", "Cascade", "Chouteau", "Glacier", "Hill", "Liberty", "Pondera", "Teton", "Toole") & dest_state == "Montana" ~ "Montana North",
                                 dest_county %in% c("Beaverhead", "Broadwater", "Deer Lodge", "Flathead", "Gallatin", "Granite", "Jefferson",
                                                    "Lake", "Lewis and Clark", "Lincoln", "Madison", "Meagher", "Mineral", "Missoula", "Park",
                                                    "Powell", "Ravalli", "Sanders", "Silver Bow") & dest_state == "Montana" ~ "Montana West",
                                 dest_county %in% c("Colfax", "Curry", "De Baca", "Guadalupe", "Harding", "Mora", "Quay", "Roosevelt",
                                                    "San Miguel", "Union") & dest_state == "New Mexico" ~ "New Mexico East",
                                 dest_county %in% c("Los Alamos", "Rio Arriba", "Santa Fe", "Taos") & dest_state == "New Mexico" ~ "New Mexico North",
                                 dest_county %in% c("Catron", "Chaves", "Dona Ana", "Eddy", "Grant", "Hidalgo", "Lea", "Lincoln", "Luna",
                                                    "Otero", "Sierra") & dest_state == "New Mexico" ~ "New Mexico South",
                                 dest_county %in% c("Bernalillo", "Cibola", "McKinley", "San Juan", "Sandoval", "Socorro", "Torrance",
                                                    "Valencia") & dest_state == "New Mexico" ~ "New Mexico West",
                                 dest_county %in% c("Carbon", "Daggett", "Duchesne", "Emery", "Grand", "San Juan", "Uintah") & dest_state == "Utah" ~ "Utah East",
                                 dest_county %in% c("Box Elder", "Cache", "Davis", "Morgan", "Rich", "Weber") & dest_state == "Utah" ~ "Utah North",
                                 dest_county %in% c("Beaver", "Garfield", "Iron", "Kane", "Washington") & dest_state == "Utah" ~ "Utah South",
                                 dest_county %in% c("Juab", "Millard", "Piute", "Salt Lake", "Sanpete", "Sevier", "Summit", "Tooele", "Utah",
                                                    "Wasatch", "Wayne") & dest_state == "Utah" ~ "Utah West",
                                 dest_county %in% c("Albany", "Carbon", "Converse", "Fremont", "Goshen", "Laramie", "Natrona", "Niobrara",
                                                    "Platte") & dest_state == "Wyoming" ~ "Wyoming East",
                                 dest_county %in% c("Campbell", "Crook", "Johnson", "Sheridan", "Weston") & dest_state == "Wyoming" ~ "Wyoming North",
                                 dest_county %in% c("Big Horn", "Hot Springs", "Lincoln", "Park", "Sublette", "Sweetwater", "Teton",
                                                    "Uinta", "Washakie") & dest_state == "Wyoming" ~ "Wyoming West"),
         sample_frac = sample_frac,
         est_visitors = device_count/sample_frac,
         # extract the year so that we can subset into individual years
         year = year(measure_date))

# compare number of daily visitors for 2019 for the 21 regions over time (regular)
sg_seed_total <- sg_seed1 %>%
  filter(year == 2019) %>%
  group_by(measure_date, dest_region) %>%
  summarize(region_visitors = sum(est_visitors))

# create summary table
sg_2019 <- sg_seed_total %>%
  group_by(dest_region) %>%
  summarize(avg_region_visitors = round(mean(region_visitors))) %>%
  mutate(kappa = avg_region_visitors/avg_region_visitors[dest_region == "Colorado North"],
         measure_date = as.Date("2019-01-01"))

visitor_compare <- ggplot(data = sg_seed_total) +
  geom_line(aes(x = measure_date, y = region_visitors,
                group = dest_region, color = dest_region), size = 1.5) +
  labs(x = NULL, y = "Visitor Count**",
       caption = paste("Last Updated", Sys.Date())) +
  ggtitle("Estimated Number of Daily Visitors to the Rocky Mountain West Regions, 2019*") +
  scale_x_date(date_labels="%b %Y", date_breaks="2 months") +
  scale_y_continuous(labels = scales::comma) +
  guides(color=guide_legend(override.aes=list(size=8), nrow = 3)) +
  ggplot_theme; visitor_compare
ggsave("./CSTE/Figures/visitor_compare.png", height = 15, width = 22, plot = visitor_compare)

# draw a line to represent averaging out the visitation over the course of the year
visitor_compare_avg <- ggplot(data = sg_seed_total) +
  geom_line(aes(x = measure_date, y = region_visitors,
                group = dest_region, color = dest_region), size = 1.5) +
  geom_hline(data = sg_2019, aes(yintercept = avg_region_visitors, color = dest_region), size = 1) +
  labs(x = NULL, y = "Visitor Count**",
       caption = paste("Last Updated", Sys.Date())) +
  ggtitle("Estimated Number of Daily Visitors to the Rocky Mountain West Regions, 2019*") +
  scale_x_date(date_labels="%b %Y", date_breaks="2 months") +
  scale_y_continuous(labels = scales::comma) +
  guides(color=guide_legend(override.aes=list(size=8), nrow = 3)) +
  ggplot_theme; visitor_compare_avg
ggsave("./CSTE/Figures/visitor_compare_avg.png", height = 15, width = 22, plot = visitor_compare_avg)


# compare number of daily visitors for 2019 for the 21 regions over time (smoothed)
visitor_compare_smoothed <- ggplot(data = sg_seed_total) +
  geom_smooth(aes(x = measure_date, y = region_visitors,
                  group = dest_region, color = dest_region), size = 1.8) +
  labs(x = NULL, y = "Visitor Count (Smoothed)**",
       caption = paste("Last Updated", Sys.Date())) +
  ggtitle("Estimated Number of Visitors to the Rocky Mountain West Regions, 2019*") +
  scale_x_date(date_labels="%b %Y", date_breaks="2 months") +
  scale_y_continuous(labels = scales::comma) +
  guides(color=guide_legend(override.aes=list(size=8), nrow = 3)) +
  ggplot_theme; visitor_compare_smoothed
ggsave("./CSTE/Figures/visitor_compare_smoothed.png", height = 15, width = 22, plot = visitor_compare_smoothed)



# now create tables with average visitors and kappa value for 2019, 2020, and 2021
years <- c("2019", "2020", "2021")
df <- list()

for (i in 1:length(years)){
  df <- sg_seed1 %>%
    filter(year == years[i]) %>%
    group_by(measure_date, dest_region) %>%
    summarize(region_visitors = sum(est_visitors)) %>%
    group_by(dest_region) %>%
    summarize(avg_region_visitors = round(mean(region_visitors))) %>%
    # calculate kappa, which is the ratio of other regions' visitation to Colorado North
    mutate(kappa = avg_region_visitors/avg_region_visitors[dest_region == "Colorado North"])
  assign(paste0("sg_seed_", years[i]), df, envir = .GlobalEnv)
}

sg_seed_2019 <- sg_seed_2019 %>%
  rename(avg2019 = avg_region_visitors,
         kappa2019 = kappa)
sg_seed_2020 <- sg_seed_2020 %>%
  rename(avg2020 = avg_region_visitors,
         kappa2020 = kappa)
sg_seed_2021 <- sg_seed_2021 %>%
  rename(avg2021 = avg_region_visitors,
         kappa2021 = kappa)

sg_seed_all <- left_join(left_join(sg_seed_2019, sg_seed_2020), sg_seed_2021) %>%
  mutate(region_abbr = c("coe", "con", "cow", "ide", "idn", "ids", "idw",
                         "mte", "mtn", "mtw", "nme", "nmn", "nms", "nmw",
                         "ute", "utn", "uts", "utw", "wye", "wyn", "wyw"))

sg_seed_table <- sg_seed_all %>%
  select(dest_region, contains("kappa"))

# now subset each region into its own dataframe (we will use this for plotting)
regions <- unique(sg_seed_all$dest_region)
abbrs <- unique(sg_seed_all$region_abbr)

for (i in 1:length(regions)){
  # run the for loop!
  df1 <- sg_seed_all %>%
    filter(dest_region == regions[i]) %>%
    select(contains("kappa"))
  df2 <- reshape2::melt(df1) %>%
    mutate(date = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01"))) %>%
    rename(kappa = value)
  assign(paste0("visit_", abbrs[i]), df2, envir = .GlobalEnv)
}

# combine all the individual region visit datasets into something subsettable
visit_ds <- do.call(rbind, mget(ls(pattern = "visit_*")))

# extract the region abbreviation from the row names
visit_ds_all <- visit_ds %>%
  mutate(region = rownames(visit_ds),
         region_abbr = substr(region, 7, 9))

# now plot the three kappa values over time for all 21 regions
# first create an empty list to store plots
visitplot <- list()
# run the for loop!
for (i in 1:length(regions)) {
  # call back our empty list so we can store all our outputs
  visitplot[[i]] <- ggplot(data = visit_ds_all %>%
                             filter(region_abbr == abbrs[i])) +
    geom_step(aes(x = date, y = kappa), color = "darkblue", size = 1.3) +
    labs(x = NULL, y = NULL) +
    # index over the names of the regions to create the titles
    #ggtitle(paste(regions[i])) +
    scale_x_date(date_labels="%Y", date_breaks = "1 year") +
    scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1.1, 0.2)) +
    ggplot_theme +
    theme(plot.margin = margin(1, 1, 1, 1, "cm")); visitplot[[i]]
  # now write the plots to the database
  #ggsave(paste0("./CSTE/Figures/kappa_", abbrs[i], ".png"), height = 10, width = 15, plot = visitplot[[i]])
}

kappa_co <- ggarrange(visitplot[[1]], visitplot[[2]], visitplot[[3]],
                      labels = c("Colorado East", "Colorado North", "Colorado West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_co
ggsave("./CSTE/Figures/kappa_co.png", height = 10, width = 10, plot = kappa_co)

kappa_id <- ggarrange(visitplot[[4]], visitplot[[5]], visitplot[[6]], visitplot[[7]],
                      labels = c("Idaho East", "Idaho North", "Idaho South", "Idaho West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_id
ggsave("./CSTE/Figures/kappa_id.png", height = 10, width = 10, plot = kappa_id)

kappa_mt <- ggarrange(visitplot[[8]], visitplot[[9]], visitplot[[10]],
                      labels = c("Montana East", "Montana North", "Montana West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_mt
ggsave("./CSTE/Figures/kappa_mt.png", height = 10, width = 10, plot = kappa_mt)

kappa_nm <- ggarrange(visitplot[[11]], visitplot[[12]], visitplot[[13]], visitplot[[14]],
                      labels = c("New Mexico East", "New Mexico North", "New Mexico South", "New Mexico West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_nm
ggsave("./CSTE/Figures/kappa_nm.png", height = 10, width = 10, plot = kappa_nm)

kappa_ut <- ggarrange(visitplot[[15]], visitplot[[16]], visitplot[[17]], visitplot[[18]],
                      labels = c("Utah East", "Utah North", "Utah South", "Utah West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_ut
ggsave("./CSTE/Figures/kappa_ut.png", height = 10, width = 10, plot = kappa_ut)

kappa_wy <- ggarrange(visitplot[[19]], visitplot[[20]], visitplot[[21]],
                      labels = c("Wyoming East", "Wyoming North", "Wyoming West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_wy
ggsave("./CSTE/Figures/kappa_wy.png", height = 10, width = 10, plot = kappa_wy)

# now break down the year 2019 into three parts to look at seasonal trends
sg_seed_seasonal <- sg_seed_total %>%
  mutate(season = case_when(measure_date <= "2019-04-30" ~ "January-April",
                            measure_date >= "2019-05-01" & measure_date <= "2019-08-31" ~ "May-August",
                            measure_date >= "2019-09-01" & measure_date <= "2019-12-31" ~ "September-December"))

# now create tables with average visitors and kappa value for January-April, May-Auguswt, and September-December
seasons <- c("January-April", "May-August", "September-December")
df <- list()

for (i in 1:length(seasons)){
  df <- sg_seed_seasonal %>%
    filter(season == seasons[i]) %>%
    group_by(measure_date, dest_region) %>%
    #summarize(region_visitors = sum(est_visitors)) %>%
    group_by(dest_region) %>%
    summarize(avg_region_visitors = round(mean(region_visitors))) %>%
    # calculate kappa, which is the ratio of other regions' visitation to Colorado North
    mutate(kappa = avg_region_visitors/avg_region_visitors[dest_region == "Colorado North"])
  assign(paste0("sg_seed_", years[i]), df, envir = .GlobalEnv)
}

`sg_seed_January-April` <- `sg_seed_January-April` %>%
  rename(avg_jan_apr = avg_region_visitors,
         kappa_jan_apr = kappa)
`sg_seed_May-August` <- `sg_seed_May-August` %>%
  rename(avg_may_aug = avg_region_visitors,
         kappa_may_aug = kappa)
`sg_seed_September-December` <- `sg_seed_September-December` %>%
  rename(avg_sep_dec = avg_region_visitors,
         kappa_sep_dec = kappa)

sg_seed_all_seasonal <- left_join(left_join(`sg_seed_January-April`, `sg_seed_May-August`), `sg_seed_September-December`) %>%
  mutate(region_abbr = c("coe", "con", "cow", "ide", "idn", "ids", "idw",
                         "mte", "mtn", "mtw", "nme", "nmn", "nms", "nmw",
                         "ute", "utn", "uts", "utw", "wye", "wyn", "wyw"))

sg_seed_table_seasonal <- sg_seed_all_seasonal %>%
  select(dest_region, contains("kappa"))

# now subset each region into its own dataframe (we will use this for plotting)
regions <- unique(sg_seed_all_seasonal$dest_region)
abbrs <- unique(sg_seed_all_seasonal$region_abbr)

for (i in 1:length(regions)){
  # run the for loop!
  df1 <- sg_seed_all_seasonal %>%
    filter(dest_region == regions[i]) %>%
    select(contains("kappa"))
  df2 <- reshape2::melt(df1) %>%
    mutate(date = as.Date(c("2019-01-01", "2019-05-01", "2019-09-01"))) %>%
    rename(kappa = value)
  assign(paste0("visit1_", abbrs[i]), df2, envir = .GlobalEnv)
}

# combine all the individual region visit datasets into something subsettable
visit_ds_seasonal <- do.call(rbind, mget(ls(pattern = "visit1_*")))

# extract the region abbreviation from the row names
visit_ds_all_seasonal <- visit_ds_seasonal %>%
  mutate(region = rownames(visit_ds_seasonal),
         region_abbr = substr(region, 8, 10))

# now plot the three seasonal kappa values over time for all 21 regions
# first create an empty list to store plots
visitplot_seasonal <- list()
# run the for loop!
for (i in 1:length(regions)) {
  # call back our empty list so we can store all our outputs
  visitplot_seasonal[[i]] <- ggplot(data = visit_ds_all_seasonal %>%
                             filter(region_abbr == abbrs[i])) +
    geom_step(aes(x = date, y = kappa), color = "purple", size = 1.3) +
    labs(x = NULL, y = NULL) +
    # index over the names of the regions to create the titles
    #ggtitle(paste(regions[i])) +
    scale_x_date(date_labels="%b", date_breaks = "2 months") +
    scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1.1, 0.2)) +
    ggplot_theme +
    theme(plot.margin = margin(1, 1, 1, 1, "cm")); visitplot_seasonal[[i]]
  # now write the plots to the database
  #ggsave(paste0("./CSTE/Figures/kappa_", abbrs[i], ".png"), height = 10, width = 15, plot = visitplot[[i]])
}

kappa_co_seasonal <- ggarrange(visitplot_seasonal[[1]], visitplot_seasonal[[2]], visitplot_seasonal[[3]],
                      labels = c("Colorado East", "Colorado North", "Colorado West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_co_seasonal
ggsave("./CSTE/Figures/kappa_co_seasonal.png", height = 10, width = 10, plot = kappa_co_seasonal)

kappa_id_seasonal <- ggarrange(visitplot_seasonal[[4]], visitplot_seasonal[[5]],
                               visitplot_seasonal[[6]], visitplot_seasonal[[7]],
                      labels = c("Idaho East", "Idaho North", "Idaho South", "Idaho West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_id_seasonal
ggsave("./CSTE/Figures/kappa_id_seasonal.png", height = 10, width = 10, plot = kappa_id_seasonal)

kappa_mt_seasonal <- ggarrange(visitplot_seasonal[[8]], visitplot_seasonal[[9]], visitplot_seasonal[[10]],
                      labels = c("Montana East", "Montana North", "Montana West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_mt_seasonal
ggsave("./CSTE/Figures/kappa_mt_seasonal.png", height = 10, width = 10, plot = kappa_mt_seasonal)

kappa_nm_seasonal <- ggarrange(visitplot_seasonal[[11]], visitplot_seasonal[[12]],
                               visitplot_seasonal[[13]], visitplot_seasonal[[14]],
                      labels = c("New Mexico East", "New Mexico North", "New Mexico South", "New Mexico West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_nm_seasonal
ggsave("./CSTE/Figures/kappa_nm_seasonal.png", height = 10, width = 10, plot = kappa_nm_seasonal)

kappa_ut_seasonal <- ggarrange(visitplot_seasonal[[15]], visitplot_seasonal[[16]],
                               visitplot_seasonal[[17]], visitplot_seasonal[[18]],
                      labels = c("Utah East", "Utah North", "Utah South", "Utah West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_ut_seasonal
ggsave("./CSTE/Figures/kappa_ut_seasonal.png", height = 10, width = 10, plot = kappa_ut_seasonal)

kappa_wy_seasonal <- ggarrange(visitplot_seasonal[[19]], visitplot_seasonal[[20]], visitplot_seasonal[[21]],
                      labels = c("Wyoming East", "Wyoming North", "Wyoming West"),
                      ncol = 2, nrow = 2,
                      font.label = list(size = 20)); kappa_wy_seasonal
ggsave("./CSTE/Figures/kappa_wy_seasonal.png", height = 10, width = 10, plot = kappa_wy_seasonal)




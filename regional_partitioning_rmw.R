### REGIONAL PARTITIONING ALGORITHM FOR ROCKY MOUNTAIN WEST STATES ###

# launch setup options
source("/Users/emwu9912/Documents/CU Anschutz/COVID-19 Modeling Group/Analysis/setup.R")
library(data.table)

# read in SafeGraph mobility seed data
within_region <- fread("./CSTE/Data Sets/sg_within_region_movements.csv.gz")

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
         # restrict the dataset time period
         measure_date >= "2021-01-01")

# now create individual datasets for each state
# create an object (factor variable) whose levels are the six unique states
states <- unique(unlist(df$origin_state))
# using a for-loop, subset the data into smaller datasets where the origin states are the states along the factor variable
for(i in seq_along(states)){
  df1 <- subset(df, origin_state==states[i])
  assign(paste0("state_", states[i]), df1, envir = .GlobalEnv)
}

# get population data for counties
county_pops <- usdata::county_2019[c('fips', 'pop')] %>% mutate(fips = as.numeric(sprintf("%05d", fips)))
county_names <- usdata::county_2019[c('fips', 'name')] %>% mutate(fips = as.numeric(sprintf("%05d", fips)),
                                                                  name = stringr::str_replace_all(name, " County",""))

# partition Colorado
# get the individual counties for the state
co_counties <- unique(state_CO$dest_fips)
county_regions_co <- data.frame(fips=co_counties)

# create an object summarizing the mean device count for each origin/destination FIPS combo
df_co_a <- state_CO %>%
  group_by(origin_fips, dest_fips) %>%
  summarize_at('device_count', mean) %>% 
  ungroup()

# create a function called summarize_fn, which will summarize the mobility dynamics for the state
summarize_fn <- function(df0){
  # summarize the county information for the state, creating origin and destination variable names
  df1 <- df0 %>% 
    left_join(county_regions_co %>% rename(origin_fips=fips)) %>%
    left_join(county_regions_co %>% rename(dest_fips=fips)) %>%
    left_join(county_pops %>% rename(origin_fips=fips, origin_pop=pop)) %>%
    left_join(county_pops %>% rename(dest_fips=fips, dest_pop=pop))
  # take the total estimated number of devices observed for each origin-destination county pair
  # calculate each device count's share of origin and share of destination by summing over all
  # the origin counties for each destination county, and summing over all the destination counties
  # for each origin county
  df2 <- df1 %>%
    left_join(df1 %>% group_by(origin_fips) %>% summarize(origin_total_devices=sum(device_count))) %>%
    left_join(df1 %>% group_by(dest_fips) %>% summarize(dest_total_devices=sum(device_count))) %>%
    left_join(county_names %>% rename(origin_fips=fips, origin_county=name)) %>%
    left_join(county_names %>% rename(dest_fips=fips, dest_county=name)) %>%
    mutate(share_of_origin = device_count / origin_total_devices,
           share_of_destination = device_count / dest_total_devices)
  # put them all together
  county_summary <- df1 %>%
    group_by(origin_fips) %>% summarize_at("device_count", sum) %>% rename(fips=origin_fips) %>%
    left_join(county_pops) %>%
    left_join(county_names) %>%
    left_join(county_regions_co)
  
  return(list(df=df2, county_summary=county_summary))
}
# apply the summary function
dates_co <- unique(state_CO$measure_date)
dfs_list_co <- lapply(dates_co, function(d) summarize_fn(state_CO))
df_co_d2 <- do.call(rbind, lapply(1:length(dates_co), function(i) dfs_list_co[[i]]$df %>% mutate(measure_date=dates_co[i])))
county_summary_d_co <- do.call(rbind, lapply(1:length(dates_co), function(i) dfs_list_co[[i]]$county_summary %>% mutate(measure_date=dates_co[i])))

dfs_co <- summarize_fn(df_co_a)
dfa2_co <- dfs_co$df
county_summary <- dfs_co$county_summary

# let's do some clustering

# construct graph
# by date (not used for now)
gs <- lapply(dates_co, function(x) graph.data.frame(state_CO %>% filter(measure_date == x) %>% select(-measure_date)))
# overall
dfa2n_co <- cbind(dfa2_co %>% select(c('origin_county', 'dest_county')), dfa2_co %>% select(-c('origin_county', 'dest_county')))
# create adjacency matrices and multiply by their transposes
g <- graph.data.frame(dfa2n_co)
adjmat1 <- as.matrix(as_adjacency_matrix(g, attr="share_of_origin"))
adjmat2 <- as.matrix(as_adjacency_matrix(g, attr="share_of_destination"))
a <- adjmat1 + adjmat2
a <- a + t(a) -2*diag(diag(a))
g2 <- graph_from_adjacency_matrix(a, mode="undirected", weighted=T, diag=F)

# cluster nodes in network for however many regions we want (between two and four)
fgc <- cluster_fast_greedy(g2, weight=E(g2)$weight)
# create dendrogram
dend_co <- as.dendrogram(fgc)
ordered_labels <- data.frame(name=labels(dend_co)) %>% left_join(county_summary)
for(k in 2:4){
  png(paste0("./CSTE/Figures/Regional Partitioning/cluster_co_", k, "groups_color_by_region_direct_contact.png"), height=15, width=9, units='in', res=300)
  par(mar=c(4.1, 2.1, 2.1, 17.1), xpd=TRUE)
  plot(dend_co, horiz=TRUE, main = paste0("Colorado, ", k, " regions"), cex.main = 2)
  title(adj = 0.5)
  dend_co %>% rect.dendrogram(k=k, border='red', lty=5, lwd=2, horiz=TRUE)
  dev.off()
    
    new_regions <- data.frame(new_region=as.factor(cutree(dend_co, k=k))); new_regions$name <- rownames(new_regions)
    new_region_labs <- county_summary %>% left_join(new_regions) %>% group_by(new_region) %>% summarize(pop=sum(pop)) %>% mutate(lab=format(paste0(new_region, ", pop: ", format(pop, big.mark=','))))
    new_regions2 <- new_regions %>% left_join(new_region_labs%>% select(-pop))
    ordered_labels2 <- ordered_labels %>% left_join(new_regions2)
    
    p <- plot_usmap(data=ordered_labels2 %>% select(fips, lab), values="lab", regions="county", include=c("CO"), labels=T, label_color='grey10') + 
      theme(legend.position="right") + 
      scale_fill_viridis_d(name="New Regions") + 
      labs(title=paste0("Mobility informed regions using ", k, " groups, direct contact"))
    p$layers[[2]]$aes_params$size <- 2.0; p
    ggsave(paste0("./CSTE/Figures/Regional Partitioning/new_regions_co_", k, "groups_direct_contact.png"), height=7, width=7, units="in", dpi=300)
}

# plot the dendrogram
png("./CSTE/Figures/Regional Partitioning/dend_co.png", width=400, height=250, units="mm", res=600)
plot_dendrogram(fgc)
title(adj=0.5, "Colorado")
dev.off()

# partition Idaho
# get the individual counties for the state
id_counties <- unique(state_ID$dest_fips)
county_regions_id <- data.frame(fips=id_counties)

# create an object summarizing the mean device count for each origin/destination FIPS combo
df_id_a <- state_ID %>%
  group_by(origin_fips, dest_fips) %>%
  summarize_at('device_count', mean) %>% 
  ungroup()

# create a function called summarize_fn, which will summarize the mobility dynamics for the state
summarize_fn <- function(df0){
  # summarize the county information for the state, creating origin and destination variable names
  df1 <- df0 %>% 
    left_join(county_regions_id %>% rename(origin_fips=fips)) %>%
    left_join(county_regions_id %>% rename(dest_fips=fips)) %>%
    left_join(county_pops %>% rename(origin_fips=fips, origin_pop=pop)) %>%
    left_join(county_pops %>% rename(dest_fips=fips, dest_pop=pop))
  # take the total estimated number of devices observed for each origin-destination county pair
  # calculate each device count's share of origin and share of destination by summing over all
  # the origin counties for each destination county, and summing over all the destination counties
  # for each origin county
  df2 <- df1 %>%
    left_join(df1 %>% group_by(origin_fips) %>% summarize(origin_total_devices=sum(device_count))) %>%
    left_join(df1 %>% group_by(dest_fips) %>% summarize(dest_total_devices=sum(device_count))) %>%
    left_join(county_names %>% rename(origin_fips=fips, origin_county=name)) %>%
    left_join(county_names %>% rename(dest_fips=fips, dest_county=name)) %>%
    mutate(share_of_origin = device_count / origin_total_devices,
           share_of_destination = device_count / dest_total_devices)
  # put them all together
  county_summary <- df1 %>%
    group_by(origin_fips) %>% summarize_at("device_count", sum) %>% rename(fips=origin_fips) %>%
    left_join(county_pops) %>%
    left_join(county_names) %>%
    left_join(county_regions_id)
  
  return(list(df=df2, county_summary=county_summary))
}
# apply the summary function
dates_id <- unique(state_ID$measure_date)
dfs_list_id <- lapply(dates_id, function(d) summarize_fn(state_ID))
df_id_d2 <- do.call(rbind, lapply(1:length(dates_id), function(i) dfs_list_id[[i]]$df %>% mutate(measure_date=dates_id[i])))
county_summary_d_id <- do.call(rbind, lapply(1:length(dates_id), function(i) dfs_list_id[[i]]$county_summary %>% mutate(measure_date=dates_id[i])))

dfs_id <- summarize_fn(df_id_a)
dfa2_id <- dfs_id$df
county_summary <- dfs_id$county_summary

# let's do some clustering

# construct graph
# by date (not used for now)
gs <- lapply(dates_id, function(x) graph.data.frame(state_ID %>% filter(measure_date == x) %>% select(-measure_date)))
# overall
dfa2n_id <- cbind(dfa2_id %>% select(c('origin_county', 'dest_county')), dfa2_id %>% select(-c('origin_county', 'dest_county')))

# create adjacency matrices and multiply by their transposes
g <- graph.data.frame(dfa2n_id)
adjmat1 <- as.matrix(as_adjacency_matrix(g, attr="share_of_origin"))
adjmat2 <- as.matrix(as_adjacency_matrix(g, attr="share_of_destination"))
a <- adjmat1 + adjmat2
a <- a + t(a) -2*diag(diag(a))
g2 <- graph_from_adjacency_matrix(a, mode="undirected", weighted=T, diag=F)

# cluster nodes in network for however many regions we want (between two and four)
fgc <- cluster_fast_greedy(g2, weight=E(g2)$weight)
# create dendrogram
dend_id <- as.dendrogram(fgc)
ordered_labels <- data.frame(name=labels(dend_id)) %>% left_join(county_summary)
for(k in 2:4){
  png(paste0("./CSTE/Figures/Regional Partitioning/cluster_id_", k, "groups_color_by_region_direct_contact.png"), height=15, width=9, units='in', res=300)
  par(mar=c(4.1, 2.1, 2.1, 17.1), xpd=TRUE)
  plot(dend_id, horiz=TRUE, main = paste0("Idaho, ", k, " regions"), cex.main = 2)
  title(adj = 0.5)
  dend_id %>% rect.dendrogram(k=k, border='red', lty=5, lwd=2, horiz=TRUE)
  dev.off()
  
  new_regions <- data.frame(new_region=as.factor(cutree(dend_id, k=k))); new_regions$name <- rownames(new_regions)
  new_region_labs <- county_summary %>% left_join(new_regions) %>% group_by(new_region) %>% summarize(pop=sum(pop)) %>% mutate(lab=format(paste0(new_region, ", pop: ", format(pop, big.mark=','))))
  new_regions2 <- new_regions %>% left_join(new_region_labs%>% select(-pop))
  ordered_labels2 <- ordered_labels %>% left_join(new_regions2)
  
  p <- plot_usmap(data=ordered_labels2 %>% select(fips, lab), values="lab", regions="county", include=c("ID"), labels=T, label_color='grey10') + 
    theme(legend.position="right") + 
    scale_fill_viridis_d(name="New Regions") + 
    labs(title=paste0("Mobility informed regions using ", k, " groups, direct contact"))
  p$layers[[2]]$aes_params$size <- 2.0; p
  ggsave(paste0("./CSTE/Figures/Regional Partitioning/new_regions_id_", k, "groups_direct_contact.png"), height=7, width=7, units="in", dpi=300)
}

# plot the dendrogram
png("./CSTE/Figures/Regional Partitioning/dend_id.png", width=400, height=250, units="mm", res=600)
plot_dendrogram(fgc)
title(adj=0.5, "Idaho")
dev.off()

# partition Montana
# get the individual counties for the state
mt_counties <- unique(state_MT$dest_fips)
county_regions_mt <- data.frame(fips=mt_counties)

# create an object summarizing the mean device count for each origin/destination FIPS combo
df_mt_a <- state_MT %>%
  group_by(origin_fips, dest_fips) %>%
  summarize_at('device_count', mean) %>% 
  ungroup()

# create a function called summarize_fn, which will summarize the mobility dynamics for the state
summarize_fn <- function(df0){
  # summarize the county information for the state, creating origin and destination variable names
  df1 <- df0 %>% 
    left_join(county_regions_mt %>% rename(origin_fips=fips)) %>%
    left_join(county_regions_mt %>% rename(dest_fips=fips)) %>%
    left_join(county_pops %>% rename(origin_fips=fips, origin_pop=pop)) %>%
    left_join(county_pops %>% rename(dest_fips=fips, dest_pop=pop))
  # take the total estimated number of devices observed for each origin-destination county pair
  # calculate each device count's share of origin and share of destination by summing over all
  # the origin counties for each destination county, and summing over all the destination counties
  # for each origin county
  df2 <- df1 %>%
    left_join(df1 %>% group_by(origin_fips) %>% summarize(origin_total_devices=sum(device_count))) %>%
    left_join(df1 %>% group_by(dest_fips) %>% summarize(dest_total_devices=sum(device_count))) %>%
    left_join(county_names %>% rename(origin_fips=fips, origin_county=name)) %>%
    left_join(county_names %>% rename(dest_fips=fips, dest_county=name)) %>%
    mutate(share_of_origin = device_count / origin_total_devices,
           share_of_destination = device_count / dest_total_devices)
  # put them all together
  county_summary <- df1 %>%
    group_by(origin_fips) %>% summarize_at("device_count", sum) %>% rename(fips=origin_fips) %>%
    left_join(county_pops) %>%
    left_join(county_names) %>%
    left_join(county_regions_mt)
  
  return(list(df=df2, county_summary=county_summary))
}
# apply the summary function
dates_mt <- unique(state_MT$measure_date)
dfs_list_mt <- lapply(dates_mt, function(d) summarize_fn(state_MT))
df_mt_d2 <- do.call(rbind, lapply(1:length(dates_mt), function(i) dfs_list_mt[[i]]$df %>% mutate(measure_date=dates_mt[i])))
county_summary_d_mt <- do.call(rbind, lapply(1:length(dates_mt), function(i) dfs_list_mt[[i]]$county_summary %>% mutate(measure_date=dates_mt[i])))

dfs_mt <- summarize_fn(df_mt_a)
dfa2_mt <- dfs_mt$df
county_summary <- dfs_mt$county_summary

# let's do some clustering

# construct graph
# by date (not used for now)
gs <- lapply(dates_mt, function(x) graph.data.frame(state_MT %>% filter(measure_date == x) %>% select(-measure_date)))
# overall
dfa2n_mt <- cbind(dfa2_mt %>% select(c('origin_county', 'dest_county')), dfa2_mt %>% select(-c('origin_county', 'dest_county')))

# create adjacency matrices and multiply by their transposes
g <- graph.data.frame(dfa2n_mt)
adjmat1 <- as.matrix(as_adjacency_matrix(g, attr="share_of_origin"))
adjmat2 <- as.matrix(as_adjacency_matrix(g, attr="share_of_destination"))
a <- adjmat1 + adjmat2
a <- a + t(a) -2*diag(diag(a))
g2 <- graph_from_adjacency_matrix(a, mode="undirected", weighted=T, diag=F)

# cluster nodes in network for however many regions we want (between two and four)
fgc <- cluster_fast_greedy(g2, weight=E(g2)$weight)
# create dendrogram
dend_mt <- as.dendrogram(fgc)
ordered_labels <- data.frame(name=labels(dend_mt)) %>% left_join(county_summary)
for(k in 2:4){
  png(paste0("./CSTE/Figures/Regional Partitioning/cluster_mt_", k, "groups_color_by_region_direct_contact.png"), height=15, width=9, units='in', res=300)
  par(mar=c(4.1, 2.1, 2.1, 17.1), xpd=TRUE)
  plot(dend_mt, horiz=TRUE, main = paste0("Montana, ", k, " regions"), cex.main = 2)
  title(adj = 0.5)
  dend_mt %>% rect.dendrogram(k=k, border='red', lty=5, lwd=2, horiz=TRUE)
  dev.off()
  
  new_regions <- data.frame(new_region=as.factor(cutree(dend_mt, k=k))); new_regions$name <- rownames(new_regions)
  new_region_labs <- county_summary %>% left_join(new_regions) %>% group_by(new_region) %>% summarize(pop=sum(pop)) %>% mutate(lab=format(paste0(new_region, ", pop: ", format(pop, big.mark=','))))
  new_regions2 <- new_regions %>% left_join(new_region_labs%>% select(-pop))
  ordered_labels2 <- ordered_labels %>% left_join(new_regions2)
  
  p <- plot_usmap(data=ordered_labels2 %>% select(fips, lab), values="lab", regions="county", include=c("MT"), labels=T, label_color='grey10') + 
    theme(legend.position="right") + 
    scale_fill_viridis_d(name="New Regions") + 
    labs(title=paste0("Mobility informed regions using ", k, " groups, direct contact"))
  p$layers[[2]]$aes_params$size <- 2.0; p
  ggsave(paste0("./CSTE/Figures/Regional Partitioning/new_regions_mt_", k, "groups_direct_contact.png"), height=7, width=7, units="in", dpi=300)
}

# plot the dendrogram
png("./CSTE/Figures/Regional Partitioning/dend_mt.png", width=400, height=250, units="mm", res=600)
plot_dendrogram(fgc)
title(adj=0.5, "Montana")
dev.off()

# partition New Mexico
# get the individual counties for the state
nm_counties <- unique(state_NM$dest_fips)
county_regions_nm <- data.frame(fips=nm_counties)

# create an object summarizing the mean device count for each origin/destination FIPS combo
df_nm_a <- state_NM %>%
  group_by(origin_fips, dest_fips) %>%
  summarize_at('device_count', mean) %>% 
  ungroup()

# create a function called summarize_fn, which will summarize the mobility dynamics for the state
summarize_fn <- function(df0){
  # summarize the county information for the state, creating origin and destination variable names
  df1 <- df0 %>% 
    left_join(county_regions_nm %>% rename(origin_fips=fips)) %>%
    left_join(county_regions_nm %>% rename(dest_fips=fips)) %>%
    left_join(county_pops %>% rename(origin_fips=fips, origin_pop=pop)) %>%
    left_join(county_pops %>% rename(dest_fips=fips, dest_pop=pop))
  # take the total estimated number of devices observed for each origin-destination county pair
  # calculate each device count's share of origin and share of destination by summing over all
  # the origin counties for each destination county, and summing over all the destination counties
  # for each origin county
  df2 <- df1 %>%
    left_join(df1 %>% group_by(origin_fips) %>% summarize(origin_total_devices=sum(device_count))) %>%
    left_join(df1 %>% group_by(dest_fips) %>% summarize(dest_total_devices=sum(device_count))) %>%
    left_join(county_names %>% rename(origin_fips=fips, origin_county=name)) %>%
    left_join(county_names %>% rename(dest_fips=fips, dest_county=name)) %>%
    mutate(share_of_origin = device_count / origin_total_devices,
           share_of_destination = device_count / dest_total_devices)
  # put them all together
  county_summary <- df1 %>%
    group_by(origin_fips) %>% summarize_at("device_count", sum) %>% rename(fips=origin_fips) %>%
    left_join(county_pops) %>%
    left_join(county_names) %>%
    left_join(county_regions_nm)
  
  return(list(df=df2, county_summary=county_summary))
}
# apply the summary function
dates_nm <- unique(state_NM$measure_date)
dfs_list_nm <- lapply(dates_nm, function(d) summarize_fn(state_NM))
df_nm_d2 <- do.call(rbind, lapply(1:length(dates_nm), function(i) dfs_list_nm[[i]]$df %>% mutate(measure_date=dates_nm[i])))
county_summary_d_nm <- do.call(rbind, lapply(1:length(dates_nm), function(i) dfs_list_nm[[i]]$county_summary %>% mutate(measure_date=dates_nm[i])))

dfs_nm <- summarize_fn(df_nm_a)
dfa2_nm <- dfs_nm$df
county_summary <- dfs_nm$county_summary

# let's do some clustering

# construct graph
# by date (not used for now)
gs <- lapply(dates_nm, function(x) graph.data.frame(state_NM %>% filter(measure_date == x) %>% select(-measure_date)))
# overall
dfa2n_nm <- cbind(dfa2_nm %>% select(c('origin_county', 'dest_county')), dfa2_nm %>% select(-c('origin_county', 'dest_county')))

# create adjacency matrices and multiply by their transposes
g <- graph.data.frame(dfa2n_nm)
adjmat1 <- as.matrix(as_adjacency_matrix(g, attr="share_of_origin"))
adjmat2 <- as.matrix(as_adjacency_matrix(g, attr="share_of_destination"))
a <- adjmat1 + adjmat2
a <- a + t(a) -2*diag(diag(a))
g2 <- graph_from_adjacency_matrix(a, mode="undirected", weighted=T, diag=F)

# cluster nodes in network for however many regions we want (between two and four)
fgc <- cluster_fast_greedy(g2, weight=E(g2)$weight)
# create dendrogram
dend_nm <- as.dendrogram(fgc)
ordered_labels <- data.frame(name=labels(dend_nm)) %>% left_join(county_summary)
for(k in 2:4){
  png(paste0("./CSTE/Figures/Regional Partitioning/cluster_nm_", k, "groups_color_by_region_direct_contact.png"), height=15, width=9, units='in', res=300)
  par(mar=c(4.1, 2.1, 2.1, 17.1), xpd=TRUE)
  plot(dend_nm, horiz=TRUE, main = paste0("New Mexico, ", k, " regions"), cex.main = 2)
  title(adj = 0.5)
  dend_nm %>% rect.dendrogram(k=k, border='red', lty=5, lwd=2, horiz=TRUE)
  dev.off()
  
  new_regions <- data.frame(new_region=as.factor(cutree(dend_nm, k=k))); new_regions$name <- rownames(new_regions)
  new_region_labs <- county_summary %>% left_join(new_regions) %>% group_by(new_region) %>% summarize(pop=sum(pop)) %>% mutate(lab=format(paste0(new_region, ", pop: ", format(pop, big.mark=','))))
  new_regions2 <- new_regions %>% left_join(new_region_labs%>% select(-pop))
  ordered_labels2 <- ordered_labels %>% left_join(new_regions2)
  
  p <- plot_usmap(data=ordered_labels2 %>% select(fips, lab), values="lab", regions="county", include=c("NM"), labels=T, label_color='grey10') + 
    theme(legend.position="right") + 
    scale_fill_viridis_d(name="New Regions") + 
    labs(title=paste0("Mobility informed regions using ", k, " groups, direct contact"))
  p$layers[[2]]$aes_params$size <- 2.0; p
  ggsave(paste0("./CSTE/Figures/Regional Partitioning/new_regions_nm_", k, "groups_direct_contact.png"), height=7, width=7, units="in", dpi=300)
}

# plot the dendrogram
png("./CSTE/Figures/Regional Partitioning/dend_nm.png", width=400, height=250, units="mm", res=600)
plot_dendrogram(fgc)
title(adj=0.5, "New Mexico")
dev.off()

# partition Utah
# get the individual counties for the state
ut_counties <- unique(state_UT$dest_fips)
county_regions_ut <- data.frame(fips=ut_counties)

# create an object summarizing the mean device count for each origin/destination FIPS combo
df_ut_a <- state_UT %>%
  group_by(origin_fips, dest_fips) %>%
  summarize_at('device_count', mean) %>% 
  ungroup()

# create a function called summarize_fn, which will summarize the mobility dynamics for the state
summarize_fn <- function(df0){
  # summarize the county information for the state, creating origin and destination variable names
  df1 <- df0 %>% 
    left_join(county_regions_ut %>% rename(origin_fips=fips)) %>%
    left_join(county_regions_ut %>% rename(dest_fips=fips)) %>%
    left_join(county_pops %>% rename(origin_fips=fips, origin_pop=pop)) %>%
    left_join(county_pops %>% rename(dest_fips=fips, dest_pop=pop))
  # take the total estimated number of devices observed for each origin-destination county pair
  # calculate each device count's share of origin and share of destination by summing over all
  # the origin counties for each destination county, and summing over all the destination counties
  # for each origin county
  df2 <- df1 %>%
    left_join(df1 %>% group_by(origin_fips) %>% summarize(origin_total_devices=sum(device_count))) %>%
    left_join(df1 %>% group_by(dest_fips) %>% summarize(dest_total_devices=sum(device_count))) %>%
    left_join(county_names %>% rename(origin_fips=fips, origin_county=name)) %>%
    left_join(county_names %>% rename(dest_fips=fips, dest_county=name)) %>%
    mutate(share_of_origin = device_count / origin_total_devices,
           share_of_destination = device_count / dest_total_devices)
  # put them all together
  county_summary <- df1 %>%
    group_by(origin_fips) %>% summarize_at("device_count", sum) %>% rename(fips=origin_fips) %>%
    left_join(county_pops) %>%
    left_join(county_names) %>%
    left_join(county_regions_ut)
  
  return(list(df=df2, county_summary=county_summary))
}
# apply the summary function
dates_ut <- unique(state_UT$measure_date)
dfs_list_ut <- lapply(dates_ut, function(d) summarize_fn(state_UT))
df_ut_d2 <- do.call(rbind, lapply(1:length(dates_ut), function(i) dfs_list_ut[[i]]$df %>% mutate(measure_date=dates_ut[i])))
county_summary_d_ut <- do.call(rbind, lapply(1:length(dates_ut), function(i) dfs_list_ut[[i]]$county_summary %>% mutate(measure_date=dates_ut[i])))

dfs_ut <- summarize_fn(df_ut_a)
dfa2_ut <- dfs_ut$df
county_summary <- dfs_ut$county_summary

# let's do some clustering

# construct graph
# by date (not used for now)
gs <- lapply(dates_ut, function(x) graph.data.frame(state_UT %>% filter(measure_date == x) %>% select(-measure_date)))
# overall
dfa2n_ut <- cbind(dfa2_ut %>% select(c('origin_county', 'dest_county')), dfa2_ut %>% select(-c('origin_county', 'dest_county')))

# create adjacency matrices and multiply by their transposes
g <- graph.data.frame(dfa2n_ut)
adjmat1 <- as.matrix(as_adjacency_matrix(g, attr="share_of_origin"))
adjmat2 <- as.matrix(as_adjacency_matrix(g, attr="share_of_destination"))
a <- adjmat1 + adjmat2
a <- a + t(a) -2*diag(diag(a))
g2 <- graph_from_adjacency_matrix(a, mode="undirected", weighted=T, diag=F)

# cluster nodes in network for however many regions we want (between two and four)
fgc <- cluster_fast_greedy(g2, weight=E(g2)$weight)
# create dendrogram
dend_ut <- as.dendrogram(fgc)
ordered_labels <- data.frame(name=labels(dend_ut)) %>% left_join(county_summary)
for(k in 2:4){
  png(paste0("./CSTE/Figures/Regional Partitioning/cluster_ut_", k, "groups_color_by_region_direct_contact.png"), height=15, width=9, units='in', res=300)
  par(mar=c(4.1, 2.1, 2.1, 17.1), xpd=TRUE)
  plot(dend_ut, horiz=TRUE, main = paste0("Utah, ", k, " regions"), cex.main = 2)
  title(adj = 0.5)
  dend_ut %>% rect.dendrogram(k=k, border='red', lty=5, lwd=2, horiz=TRUE)
  dev.off()
  
  new_regions <- data.frame(new_region=as.factor(cutree(dend_ut, k=k))); new_regions$name <- rownames(new_regions)
  new_region_labs <- county_summary %>% left_join(new_regions) %>% group_by(new_region) %>% summarize(pop=sum(pop)) %>% mutate(lab=format(paste0(new_region, ", pop: ", format(pop, big.mark=','))))
  new_regions2 <- new_regions %>% left_join(new_region_labs%>% select(-pop))
  ordered_labels2 <- ordered_labels %>% left_join(new_regions2)
  
  p <- plot_usmap(data=ordered_labels2 %>% select(fips, lab), values="lab", regions="county", include=c("UT"), labels=T, label_color='grey10') + 
    theme(legend.position="right") + 
    scale_fill_viridis_d(name="New Regions") + 
    labs(title=paste0("Mobility informed regions using ", k, " groups, direct contact"))
  p$layers[[2]]$aes_params$size <- 2.0; p
  ggsave(paste0("./CSTE/Figures/Regional Partitioning/new_regions_ut_", k, "groups_direct_contact.png"), height=7, width=7, units="in", dpi=300)
}

# plot the dendrogram
png("./CSTE/Figures/Regional Partitioning/dend_ut.png", width=400, height=250, units="mm", res=600)
plot_dendrogram(fgc)
title(adj=0.5, "Utah")
dev.off()

# partition Wyoming
# get the individual counties for the state
wy_counties <- unique(state_WY$dest_fips)
county_regions_wy <- data.frame(fips=wy_counties)

# create an object summarizing the mean device count for each origin/destination FIPS combo
df_wy_a <- state_WY %>%
  group_by(origin_fips, dest_fips) %>%
  summarize_at('device_count', mean) %>% 
  ungroup()

# create a function called summarize_fn, which will summarize the mobility dynamics for the state
summarize_fn <- function(df0){
  # summarize the county information for the state, creating origin and destination variable names
  df1 <- df0 %>% 
    left_join(county_regions_wy %>% rename(origin_fips=fips)) %>%
    left_join(county_regions_wy %>% rename(dest_fips=fips)) %>%
    left_join(county_pops %>% rename(origin_fips=fips, origin_pop=pop)) %>%
    left_join(county_pops %>% rename(dest_fips=fips, dest_pop=pop))
  # take the total estimated number of devices observed for each origin-destination county pair
  # calculate each device count's share of origin and share of destination by summing over all
  # the origin counties for each destination county, and summing over all the destination counties
  # for each origin county
  df2 <- df1 %>%
    left_join(df1 %>% group_by(origin_fips) %>% summarize(origin_total_devices=sum(device_count))) %>%
    left_join(df1 %>% group_by(dest_fips) %>% summarize(dest_total_devices=sum(device_count))) %>%
    left_join(county_names %>% rename(origin_fips=fips, origin_county=name)) %>%
    left_join(county_names %>% rename(dest_fips=fips, dest_county=name)) %>%
    mutate(share_of_origin = device_count / origin_total_devices,
           share_of_destination = device_count / dest_total_devices)
  # put them all together
  county_summary <- df1 %>%
    group_by(origin_fips) %>% summarize_at("device_count", sum) %>% rename(fips=origin_fips) %>%
    left_join(county_pops) %>%
    left_join(county_names) %>%
    left_join(county_regions_wy)
  
  return(list(df=df2, county_summary=county_summary))
}
# apply the summary function
dates_wy <- unique(state_WY$measure_date)
dfs_list_wy <- lapply(dates_wy, function(d) summarize_fn(state_WY))
df_wy_d2 <- do.call(rbind, lapply(1:length(dates_wy), function(i) dfs_list_wy[[i]]$df %>% mutate(measure_date=dates_wy[i])))
county_summary_d_wy <- do.call(rbind, lapply(1:length(dates_wy), function(i) dfs_list_wy[[i]]$county_summary %>% mutate(measure_date=dates_wy[i])))

dfs_wy <- summarize_fn(df_wy_a)
dfa2_wy <- dfs_wy$df
county_summary <- dfs_wy$county_summary

# let's do some clustering

# construct graph
# by date (not used for now)
gs <- lapply(dates_wy, function(x) graph.data.frame(state_WY %>% filter(measure_date == x) %>% select(-measure_date)))
# overall
dfa2n_wy <- cbind(dfa2_wy %>% select(c('origin_county', 'dest_county')), dfa2_wy %>% select(-c('origin_county', 'dest_county')))

# create adjacency matrices and multiply by their transposes
g <- graph.data.frame(dfa2n_wy)
adjmat1 <- as.matrix(as_adjacency_matrix(g, attr="share_of_origin"))
adjmat2 <- as.matrix(as_adjacency_matrix(g, attr="share_of_destination"))
a <- adjmat1 + adjmat2
a <- a + t(a) -2*diag(diag(a))
g2 <- graph_from_adjacency_matrix(a, mode="undirected", weighted=T, diag=F)

# cluster nodes in network for however many regions we want (between two and four)
fgc <- cluster_fast_greedy(g2, weight=E(g2)$weight)
# create dendrogram
dend_wy <- as.dendrogram(fgc)
ordered_labels <- data.frame(name=labels(dend_wy)) %>% left_join(county_summary)
for(k in 2:4){
  png(paste0("./CSTE/Figures/Regional Partitioning/cluster_wy_", k, "groups_color_by_region_direct_contact.png"), height=15, width=9, units='in', res=300)
  par(mar=c(4.1, 2.1, 2.1, 17.1), xpd=TRUE)
  plot(dend_wy, horiz=TRUE, main = paste0("Wyoming, ", k, " regions"), cex.main = 2)
  title(adj = 0.5)
  dend_wy %>% rect.dendrogram(k=k, border='red', lty=5, lwd=2, horiz=TRUE)
  dev.off()
  
  new_regions <- data.frame(new_region=as.factor(cutree(dend_wy, k=k))); new_regions$name <- rownames(new_regions)
  new_region_labs <- county_summary %>% left_join(new_regions) %>% group_by(new_region) %>% summarize(pop=sum(pop)) %>% mutate(lab=format(paste0(new_region, ", pop: ", format(pop, big.mark=','))))
  new_regions2 <- new_regions %>% left_join(new_region_labs%>% select(-pop))
  ordered_labels2 <- ordered_labels %>% left_join(new_regions2)
  
  p <- plot_usmap(data=ordered_labels2 %>% select(fips, lab), values="lab", regions="county", include=c("WY"), labels=T, label_color='grey10') + 
    theme(legend.position="right") + 
    scale_fill_viridis_d(name="New Regions") + 
    labs(title=paste0("Mobility informed regions using ", k, " groups, direct contact"))
  p$layers[[2]]$aes_params$size <- 2.0; p
  ggsave(paste0("./CSTE/Figures/Regional Partitioning/new_regions_wy_", k, "groups_direct_contact.png"), height=7, width=7, units="in", dpi=300)
}

# plot the dendrogram
png("./CSTE/Figures/Regional Partitioning/dend_wy.png", width=400, height=250, units="mm", res=600)
plot_dendrogram(fgc)
title(adj=0.5, "Wyoming")
dev.off()


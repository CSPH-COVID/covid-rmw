# PARTITIONING ALGORITHM (COLORADO)
# random comment

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
dfs_list_co <- lapply(dates_co, function(d) summarize_fn(state_CO))# %>% filter(measure_date==d) %>% select(-measure_date)))
df_co_d2 <- do.call(rbind, lapply(1:length(dates_co), function(i) dfs_list_co[[i]]$df %>% mutate(measure_date=dates_co[i])))
county_summary_d_co <- do.call(rbind, lapply(1:length(dates_co), function(i) dfs_list_co[[i]]$county_summary %>% mutate(measure_date=dates_co[i])))

dfs_co <- summarize_fn(df_co_a)
dfa2_co <- dfs_co$df
county_summary <- dfs_co$county_summary

################################################################################
### let's do some clustering

# construct graph
# by date (not used for now)
gs <- lapply(dates_co, function(x) graph.data.frame(state_CO %>% filter(measure_date == x) %>% select(-measure_date)))

# overall
dfa2n_co <- cbind(dfa2_co %>% select(c('origin_county', 'dest_county')), dfa2_co %>% select(-c('origin_county', 'dest_county')))

# create adjacency matrices and multiply by their transposes
g <- graph.data.frame(dfa2n_co)
adjmat1 <- as.matrix(as_adjacency_matrix(g, attr="share_of_origin"))
adjmat2 <- as.matrix(as_adjacency_matrix(g, attr="share_of_destination"))
a <- adjmat1 %*% t(adjmat2)
a <- a + t(a) -2*diag(diag(a))
b <- adjmat1 + adjmat2
b <- b + t(b) -2*diag(diag(b))
g2 <- graph_from_adjacency_matrix(a, mode="undirected", weighted=T, diag=F)
g2b <- graph_from_adjacency_matrix(b, mode="undirected", weighted=T, diag=F)

# cluster nodes in network for however many regions we want (between two and four)
adjs <- list(a,b); adj_labels <- c("all contact", "direct contact"); labs2 <- c("all_contact", "direct_contact")
for(i in 1:length(adjs)){
  g2 <- graph_from_adjacency_matrix(adjs[[i]], mode="undirected", weighted=T, diag=F)
  fgc <- cluster_fast_greedy(g2, weight=E(g2)$weight)
  # create dendrogram
  dend_co <- as.dendrogram(fgc)
  ordered_labels <- data.frame(name=labels(dend_co)) %>% left_join(county_summary)
  labels_colors(dend_co) <- ordered_labels$color
  for(k in 2:4){
    png(paste0("./CSTE/Figures/cluster_co_", k, "groups_color_by_region_", labs2[i], ".png"), height=15, width=9, units='in', res=300)
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
      labs(title=paste0("Mobility informed regions using ", k, " groups, ", adj_labels[i]))
    p$layers[[2]]$aes_params$size <- 2.0; p
    ggsave(paste0("./CSTE/Figures/new_regions_co_", k, "groups_", labs2[i], ".png"), height=7, width=7, units="in", dpi=300)
  }
}

# plot the dendrogram
png("./CSTE/Figures/dend_co.png", width=400, height=250, units="mm", res=600)
plot_dendrogram(fgc)
title(adj=0.5, "Colorado")
dev.off()


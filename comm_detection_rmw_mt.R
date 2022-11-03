# PARTITIONING ALGORITHM (MONTANA)

# get the individual counties for the state
mt_counties <- unique(state_MT$dest_fips)
county_regions_mt <- data.frame(fips=mt_counties)

# summarize by date
df_mt_d <- state_MT %>% 
  filter(measure_date >= as.Date("2021-01-01"))

# summarize all
df_mt_a <- df_mt_d %>%
  group_by(origin_fips, dest_fips) %>%
  summarize_at('device_count', mean) %>% 
  ungroup()

summarize_fn <- function(df0){
  df1 <- df0 %>% 
    left_join(county_regions_mt %>% rename(origin_fips=fips)) %>%
    left_join(county_regions_mt %>% rename(dest_fips=fips)) %>%
    left_join(county_pops %>% rename(origin_fips=fips, origin_pop=pop)) %>%
    left_join(county_pops %>% rename(dest_fips=fips, dest_pop=pop))
  
  df2 <- df1 %>%
    left_join(df1 %>% group_by(origin_fips) %>% summarize(origin_total_devices=sum(device_count))) %>%
    left_join(df1 %>% group_by(dest_fips) %>% summarize(dest_total_devices=sum(device_count))) %>%
    left_join(county_names %>% rename(origin_fips=fips, origin_county=name)) %>%
    left_join(county_names %>% rename(dest_fips=fips, dest_county=name)) %>%
    mutate(share_of_origin = device_count / origin_total_devices,
           share_of_destination = device_count / dest_total_devices)

  county_summary <- df1 %>%
    group_by(origin_fips) %>% summarize_at("device_count", sum) %>% rename(fips=origin_fips) %>%
    left_join(county_pops) %>%
    left_join(county_names) %>%
    left_join(county_regions_mt)

    return(list(df=df2, county_summary=county_summary))
}
# apply the summary function
dates_mt <- unique(df_mt_d$measure_date)
dfs_list_mt <- lapply(dates_mt, function(d) summarize_fn(df_mt_d %>% filter(measure_date==d) %>% select(-measure_date)))
df_mt_d2 <- do.call(rbind, lapply(1:length(dates_mt), function(i) dfs_list_mt[[i]]$df %>% mutate(measure_date=dates_mt[i])))
county_summary_d_mt <- do.call(rbind, lapply(1:length(dates_mt), function(i) dfs_list_mt[[i]]$county_summary %>% mutate(measure_date=dates_mt[i])))

dfs_mt <- summarize_fn(df_mt_a)
dfa2_mt <- dfs_mt$df
county_summary <- dfs_mt$county_summary

################################################################################
### Let's do some clustering

# construct graph
# by date (not used for now)
gs <- lapply(dates_mt, function(x) graph.data.frame(df_mt_d %>% filter(measure_date == x) %>% select(-measure_date)))

# overall
dfa2n <- cbind(dfa2_mt %>% select(c('origin_county', 'dest_county')), dfa2_mt %>% select(-c('origin_county', 'dest_county')))

# version 1
g <- graph.data.frame(dfa2n)
adjmat1 <- as.matrix(as_adjacency_matrix(g, attr="share_of_origin"))
adjmat2 <- as.matrix(as_adjacency_matrix(g, attr="share_of_destination"))
a <- adjmat1 %*% t(adjmat2)
a <- a + t(a) -2*diag(diag(a))
b <- adjmat1 + adjmat2
b <- b + t(b) -2*diag(diag(b))
g2 <- graph_from_adjacency_matrix(a, mode="undirected", weighted=T, diag=F)
g2b <- graph_from_adjacency_matrix(b, mode="undirected", weighted=T, diag=F)

#Cluster nodes in network
adjs <- list(a,b); adj_labels <- c("all contact", "direct contact"); labs2 <- c("all_contact", "direct_contact")
for(i in 1:length(adjs)){
  g2 <- graph_from_adjacency_matrix(adjs[[i]], mode="undirected", weighted=T, diag=F)
  fgc <- cluster_fast_greedy(g2, weight=E(g2)$weight)
  dend_mt <- as.dendrogram(fgc)
  ordered_labels <- data.frame(name=labels(dend_mt)) %>% left_join(county_summary)
  labels_colors(dend_mt) <- ordered_labels$color
  for(k in 2:6){
    png(paste0("./CSTE/Figures/cluster_mt_", k, "groups_color_by_region_", labs2[i], ".png"), height=15, width=9, units='in', res=300)
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
      labs(title=paste0("Mobility informed regions using ", k, " groups, ", adj_labels[i]))
    p$layers[[2]]$aes_params$size <- 2.0; p
    ggsave(paste0("./CSTE/Figures/new_regions_mt_", k, "groups_", labs2[i], ".png"), height=7, width=7, units="in", dpi=300)
  }
}

png("./CSTE/Figures/dend_mt.png", width=400, height=250, units="mm", res=600)
plot_dendrogram(fgc)
title(adj=0.5, "Montana")
dev.off()


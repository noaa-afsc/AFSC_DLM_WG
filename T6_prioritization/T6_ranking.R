#Weights each of the metrics and ranks species ----
#Updated 4/9/2024 by C. Tribuzio

# Setup ----
# libraries and functions
libs <- c("tidyverse", "janitor", "googlesheets4")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Bring in data ----
# metrics
T6_metrics <- read_csv(paste0(getwd(), "/T6_prioritization/T6_priority_summary_metrics.csv")) %>% 
  select(!c(agency_species_code, cv_drate, mean_ntrip, cv_ntrip, max_ntrip, min_ntrip, cv_pobs_c))
#scale everything between 0 and 1
quan_met <- T6_metrics %>% 
  select(!c(dominance, T6_HCR_data, edge_dist, marketable, Tier_jump, complex, multi_tier, complex_lrg, Inflat_spec)) %>% 
  mutate(cycle_yrs = cycle_yrs/4,
         PSA_new = PSA_new/2.8,
         meancpOFL_new = meancpOFL/max(meancpOFL, na.rm = T)) %>% 
  select(!meancpOFL) %>% 
  rename(meancpOFL = meancpOFL_new) %>% 
  pivot_longer(cols = !c(fmp_area, species_name, species_group_code), names_to = "Metric", values_to = "Value") %>% 
  mutate(Value = replace_na(Value, 0))
qual_met <- T6_metrics %>% 
  select(c(fmp_area, species_name, species_group_code, dominance, T6_HCR_data, edge_dist, 
           marketable, Tier_jump, complex, multi_tier, complex_lrg, Inflat_spec)) %>% 
  pivot_longer(cols = !c(fmp_area, species_name, species_group_code), names_to = "Metric", values_to = "Value")

#weights
wt <- read_sheet('https://docs.google.com/spreadsheets/d/13ScMnmVnbBW1pCsG1zq5XBSWPYPJM9ymDLrIGse1VMk/edit?usp=sharing')

#scale_val <- wt %>% 
#  select(!c(type, yes_value, Description)) %>% 
#  pivot_longer(!Metric, names_to = "Scenario", values_to = "wts") %>% 
#  group_by(Scenario) %>% 
#  summarise(scale_val = sum(wts))

#new_wt <- wt %>% 
#  select(!c(type, yes_value, Description)) %>% 
#  pivot_longer(!Metric, names_to = "Scenario", values_to = "wts") %>% 
#  left_join(scale_val) %>% 
#  mutate(new_wt = wts/scale_val) %>% 
#  select(!c(wts, scale_val)) %>% 
#  pivot_wider(names_from = Scenario, values_from = new_wt)

#wt <- wt %>% 
#  select(c(Metric, type, yes_value, Description)) %>% 
#  left_join(new_wt)


# Quantitative metric weighting----
quan_wt <- wt %>% 
  filter(type == "quantitative") %>% 
  select(!c(Description, type, yes_value)) %>% 
  pivot_longer(cols = !Metric, names_to = "Scenario", values_to = "weighting")

quan2 <- quan_met %>% 
  full_join(quan_wt) %>% 
  mutate(wtd_metric = Value * weighting)

# Qualitative metric weighting-----
yesvals <- wt %>% 
  select(Metric, yes_value)

qual_met2 <- qual_met %>% 
  left_join(yesvals) %>% 
  mutate(Val2 = if_else(Value == "yes", yes_value, 0)) %>% 
  select(!c(Value, yes_value)) %>% 
  rename(Value = Val2)

qual_wt <- wt %>% 
  filter(type == "qualitative") %>% 
  select(!c(Description, type, yes_value)) %>% 
  pivot_longer(cols = !Metric, names_to = "Scenario", values_to = "weighting")

qual2 <- qual_met2 %>% 
  full_join(qual_wt) %>% 
  mutate(wtd_metric = Value * weighting)

# Combine outputs----
 wt_met <- quan2 %>% 
   bind_rows(qual2)
 
# Sensitivity to metrics----
base_dat <- wt_met %>%
  filter(Scenario == "wt_equal") %>% 
  select(fmp_area, species_name, species_group_code, Metric, Value) %>% 
  group_by(fmp_area, species_name, species_group_code, Metric) %>% 
  summarise(base_val = mean(Value)) %>% 
  ungroup() %>% 
  mutate(stock_name = paste0(fmp_area, species_name, species_group_code)) %>% 
  select(Metric, base_val, stock_name) %>% 
  pivot_wider(names_from = stock_name, values_from = base_val)

spec_key <- unique(base[,1:3])

sens_scen <- read_sheet('https://docs.google.com/spreadsheets/d/1jsygfhdl54ZU67_K8DK-EsWXONoDvuKkxp8PZiLXZbE/edit?usp=sharing')
scen_lookup <- read_sheet('https://docs.google.com/spreadsheets/d/14m6dW8FYMqzARZeTtKXUVqS3YgmwQ3318Bg5wQLUwKg/edit?usp=sharing')
  
df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), 
               c("name", "scenario", "scen_value", "rank", "metric"))

for (i in 1:nrow(sens_scen)){ 
  scen_dat <- sens_scen %>% 
      slice(i) %>% 
      pivot_longer(!scenario, names_to = "Metric") %>% 
    left_join(base_dat)
  scen_name <- unique(scen_dat$scenario)
  scen_metric <- scen_lookup %>% 
    filter(scenario == scen_name)
  scen_metric <- unique(scen_metric$metric)
  wts <- scen_dat %>% select(value)
  test_dat <- scen_dat %>% 
    select(!c(scenario, Metric, value))
  test_out <- test_dat %>% 
    mutate_all(function(col){wts*col}) %>% 
    mutate(scenario = scen_name,
           metric = scen_metric) %>% 
    pivot_longer(!c(scenario, metric)) %>% 
    group_by(name, scenario) %>% 
    summarise(scen_value = sum(value$value)) %>%
    ungroup() %>% 
    arrange(-scen_value) %>% 
    mutate(rank = row_number(),
           metric = unique(scen_metric))
  df <- rbind(df, test_out)
}



base_rank <- df %>% 
  filter(scenario == "base") %>% 
  select(name, rank) %>% 
  rename(base_rank = rank)

scen_out <- df %>% 
  left_join(base_rank) %>% 
  mutate(rank_diff = rank - base_rank) %>% 
  left_join(scen_lookup) %>% 
  filter(scenario != "base")
write_csv(scen_out, paste0(getwd(), "/T6_prioritization/T6_sensitivity_runs.csv"))

scen_plot <- ggplot(scen_out, aes(x = xval, y = rank_diff))+
  geom_point()+
  labs(x = "Weight", y = "Rank change from equal weighting")+
  facet_wrap(vars(metric))

ggsave(path = paste0(getwd(), "/T6_prioritization/Figures/Sensitivity"), 
       paste0("metric_sensitivity.png"),
       plot = scen_plot, dpi=300)

# species specific changes
unq_key <- unique(scen_out$name)
for (i in 1:length(unq_key)){ #this kicks out 35 grobs
  lspec <- unq_key[i]
  loop_dat <- scen_out %>%
    filter(name %in% lspec)
  lfig <- ggplot(loop_dat, aes(x = xval, y = rank_diff, fill = metric))+
    geom_point()+
    labs(title = lspec, x = "Weight", y = "Rank change from equal weighting")+
    facet_wrap(vars(metric))
  ggsave(path = paste0(getwd(), "/T6_prioritization/Figures/Sensitivity/Stocks"), 
         paste0(lspec, "_sensitivity.png"),
         plot = lfig, dpi=300)
} 



 

# Ranking ----  
ranks <- wt_met %>% 
   group_by(fmp_area, species_name, species_group_code, Scenario) %>% 
   summarise(tot_rank = sum(wtd_metric)) %>% 
   group_by(Scenario) %>% 
   mutate(ranks = order(order(tot_rank)))
 
 #these figs are kinda meaningless unless I scale everything to 0 - 1
 for (i in 1:nrow(unq_key)){ #this kicks out 8 grobs
   lfmp <- unq_key[i,1]
   lspec <- unq_key[i,2]
   loop_dat <- ranks %>%
     filter(fmp_area %in% lfmp &
              species_name %in% lspec)
   lfig <- ggplot(loop_dat, aes(x = Scenario, y = tot_rank, fill = Scenario))+
     geom_bar(stat = "identity")+
     facet_grid(.~fmp_area+species_name+species_group_code)+
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
   ggsave(path = paste0(getwd(), "/T6_prioritization/Figures"), 
          paste0(lfmp, lspec, "_ranks.png"),
          plot = lfig, dpi=600)
 } 
 
 ukey <- 
 
 write_csv(ranks, paste0(getwd(), "/T6_prioritization/T6_priority_rankings.csv"))
 
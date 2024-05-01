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
quan_met <- T6_metrics %>% 
  select(!c(dominance, T6_HCR_data, edge_dist, marketable, Tier_jump)) %>% 
  pivot_longer(cols = !c(fmp_area, species_name, species_group_code), names_to = "Metric", values_to = "Value")
qual_met <- T6_metrics %>% 
  select(c(fmp_area, species_name, species_group_code,dominance, T6_HCR_data, edge_dist, marketable, Tier_jump)) %>% 
  pivot_longer(cols = !c(fmp_area, species_name, species_group_code), names_to = "Metric", values_to = "Value")

#weights
wt <- read_sheet('https://docs.google.com/spreadsheets/d/13ScMnmVnbBW1pCsG1zq5XBSWPYPJM9ymDLrIGse1VMk/edit?usp=sharing') %>% 
  filter(keep == "Y") %>% 
  select(!(keep))
quan_wt <- wt %>% 
  filter(Metric %nin% c("dominance", "T6_HCR_data", "edge_dist", "marketable", "Tier_jump", "complex")) %>% 
  select(!Description)
qual_wt <- wt %>% 
  filter(Metric %in% c("dominance", "T6_HCR_data", "edge_dist", "marketable", "Tier_jump", "complex")) %>% 
  select(!Description)

# Weight each metric ----
quan2 <- quan_met %>% 
  full_join(quan_wt) %>% 
  mutate(bet_eval = paste0("between(", Value, ",", category, ")")) %>% 
  rowwise %>% 
  mutate(eval_out = eval(parse(text = bet_eval))) %>% 
  filter(eval_out == T) %>% 
  select(!c(bet_eval, eval_out, category, Value))
  
qual2 <- qual_met %>% 
  full_join(qual_wt) %>% 
  mutate(eval_out = (Value == category)) %>% 
  filter(eval_out == T) %>%
  select(!c(eval_out, category, Value))

 wt_met <- quan2 %>% 
   bind_rows(qual2)
 
 unq_key <- wt_met %>% 
   select(fmp_area, species_name) %>% 
   unique()
 
 for (i in 1:nrow(unq_key)){ #this kicks out 8 grobs
   lfmp <- unq_key[i,1]
   lspec <- unq_key[i,2]
   loop_dat <- wt_met %>%
     filter(fmp_area == lfmp,
            species_name == lspec)
   lfig <- ggplot(loop_dat, aes(x = Metric, y = weight))+
     geom_bar(stat = "identity")+
     facet_grid(.~fmp_area+species_name)+
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
   ggsave(path = paste0(getwd(), "/T6_prioritization"), 
          paste0(lfmp, lspec, ".png"),
                 plot = lfig, dpi=600)
 }
  
# Ranking ----  
ranks <- wt_met %>% 
   group_by(fmp_area, species_name, species_group_code) %>% 
   summarise(mean_rank = mean(weight),
             tot_rank = sum(weight))
 write_csv(ranks, paste0(getwd(), "/T6_prioritization/T6_priority_rankings.csv"))
 
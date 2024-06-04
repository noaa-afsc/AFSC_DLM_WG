#Calculates summary metrics for each Tier 6 species ----
#Updated 4/9/2024 by C. Tribuzio

# Setup ----
# libraries and functions
libs <- c("tidyverse", "janitor", "googlesheets4")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Bring in catch data ----
T6_catch <- read_csv(paste0(getwd(), "/Data/confidential_T6_catch.csv")) %>% 
  filter(!(species_group_code == "DEM1" & agency_species_code == 145))

# mean number of trips reporting species over last 10 yrs----
n_trips <- T6_catch %>% 
  group_by(year, fmp_area, agency_species_code, species_name, species_group_code) %>% 
  summarise(n_trip = length(catch_mt)) %>% 
  group_by(fmp_area, agency_species_code, species_name, species_group_code) %>% 
  summarise(mean_ntrip = mean(n_trip, na.rm = T),
            cv_ntrip = sd(n_trip, na.rm = T)/mean_ntrip,
            max_ntrip = max(n_trip, na.rm = T),
            min_ntrip = min(n_trip, na.rm = T)) %>% 
  mutate(cv_ntrip = replace_na(cv_ntrip, 0))

# mean discard rate over last 10 yrs----
tot_c <- T6_catch %>% 
  group_by(year, fmp_area, agency_species_code, species_name, species_group_code) %>% 
  summarise(c_mt = sum(catch_mt))

d_rate <- T6_catch %>% 
  group_by(year, fmp_area, agency_species_code, species_name, species_group_code, retained_or_discarded) %>% 
  summarise(subcatch_mt = sum(catch_mt)) %>% 
  pivot_wider(names_from = retained_or_discarded, values_from = subcatch_mt) %>% 
  mutate(D = replace_na(D,0),
         disc_rate = D/sum(D+R)) %>% 
  select(!c(D, R)) %>% 
  group_by(fmp_area, agency_species_code, species_name, species_group_code) %>% 
  summarise(mean_drate = mean(disc_rate, na.rm = T),
            cv_drate = sd(disc_rate, na.rm = T)/mean_drate) %>% 
  mutate(cv_drate = replace_na(cv_drate, 0))

# Proportion of complex OFL ----
# bring in OFL sheet from google
OFL <- read_sheet('https://docs.google.com/spreadsheets/d/1D8HzYl0cQGuLwLhNiTE-u2d8VVvy1kKWEVOpVnmFG3k/edit?usp=sharing')

# mean proportion that each species is of the complex OFL
pOFL <- OFL %>% 
  mutate(pOFL = subOFL/OFL) %>% 
  group_by(species_name, species_group_code, fmp_area) %>% 
  summarise(meanpOFL = mean(pOFL))

# mean proportion of species specific OFL is caught each year
# NOTE if the mean catch is ABOVE the OFL it is set = 1
cOFL <- OFL %>% 
  left_join(tot_c) %>% 
  mutate(c_mt = replace_na(c_mt, 0),
         cpOFL = c_mt/subOFL) %>% 
  group_by(species_name, species_group_code, fmp_area) %>% 
  summarise(meancpOFL = mean(cpOFL)) %>% 
  mutate(meancpOFL = if_else(meancpOFL > 1, 1, meancpOFL))

# proportion of catch observed ----
OBScatch <- T6_catch %>% 
  group_by(year, fmp_area, species_group_code, species_name, monitoring_status) %>% 
  summarise(c_mt = sum(catch_mt)) %>% 
  pivot_wider(names_from = monitoring_status, values_from = c_mt) %>% 
  mutate(obs_c = sum(AT_SEA, AT_SEA_WITH_SALMON_CENSUS, FIXED_GEAR_EM, TRAWL_EM_WITH_SHORESIDE, TRAWL_EM, na.rm = T),
         pobs_c = obs_c/sum(AT_SEA, AT_SEA_WITH_SALMON_CENSUS, FIXED_GEAR_EM, 
                            TRAWL_EM_WITH_SHORESIDE, TRAWL_EM, NO_MONITORING, na.rm = T)) %>% 
  select(year, fmp_area, species_group_code, species_name, pobs_c) %>% 
  group_by(species_name, species_group_code, fmp_area) %>% 
  summarise(mean_pobs_c = mean(pobs_c, na.rm = T),
            cv_pobs_c = sd(pobs_c, na.rm = T)/mean_pobs_c) %>% 
  mutate(cv_pobs_c = replace_na(cv_pobs_c, 0))

# bring in qualitative information
qual_dat <- read_sheet('https://docs.google.com/spreadsheets/d/1NMz_Bh4CMwULWgHfQ0xahNdhRe77gm2LhUdRyon_vco/edit?usp=sharing')
qdat <- qual_dat %>% 
  select(species_name, species_group_code, agency_species_code, fmp_area, cycle_yrs, 
         dominance, T6_HCR_data, PSA_new, edge_dist, marketable, Tier_jump, complex,
         multi_tier, complex_lrg, Inflat_spec, Teleost)

# summary table ----

out_summ <- d_rate %>% 
  left_join(n_trips) %>% 
  left_join(pOFL) %>% 
  left_join(cOFL) %>% 
  left_join(OBScatch) %>% 
  full_join(qdat)

write_csv(out_summ, paste0(getwd(), "/T6_prioritization/T6_priority_summary_metrics.csv"))


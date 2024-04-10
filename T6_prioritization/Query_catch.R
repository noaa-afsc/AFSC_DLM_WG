#AKRO CAS Data ----
#Updated 4/9/2024 by C. Tribuzio
#This code will pull the data from AKFIN and clean it up for use in the analyses

# Setup ----
# libraries and functions
libs <- c("tidyverse", "janitor", "RODBC", "googlesheets4")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# database connections
dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE))

# Pull in species code list ----
#Read google sheets data into R
# note that this step requires Google to authenticate your account, enter 1 at the prompt, 
# check the boxes and hit "continue" until it gets you there

x <- read_sheet('https://https://docs.google.com/spreadsheets/d/1NMz_Bh4CMwULWgHfQ0xahNdhRe77gm2LhUdRyon_vco/edit#gid=0')
CAS_codes <- unique(x %>% select(fmp_area, agency_species_code))

# Query CAS data ----
# Query FMPs separately using the FMP specific list of species

# BSAI catch data query
# Create a vector of species within the FMP
BSAI_specs <- CAS_codes %>% 
  filter(fmp_area == 'BSAI') %>% 
  select(agency_species_code)
Bs <- paste("", BSAI_specs, "", collapse = ",")
Bs <- gsub('c', '', Bs)

# write query
Bs_query <- paste("
                  select year, fmp_area,	fmp_subarea, reporting_area_code, agency_gear_code, agency_species_code,
                       species_name, species_group_code, retained_or_discarded, weight_posted, gf_harvest_sector, 
                       deployment_trip_pk, monitoring_status,	sampling_strata_deployment_category
                from council.comprehensive_blend_ca
                where year between 2014 and 2023
                and (fmp_area = 'BSAI') 
                and agency_species_code IN ", Bs, sep = "")

#query and format the data
BSAI_catch <- sqlQuery(channel_akfin, Bs_query) %>% 
  clean_names() %>% 
  as.data.frame() %>% 
  group_by(year, fmp_area,	fmp_subarea, reporting_area_code, agency_gear_code, agency_species_code,
           species_name, species_group_code, retained_or_discarded, gf_harvest_sector, 
           deployment_trip_pk, monitoring_status,	sampling_strata_deployment_category) %>% 
  summarise(catch_mt = sum(weight_posted))

# GOA Catch data query
# Create a vector of species within the FMP
GOA_specs <- CAS_codes %>% 
  filter(fmp_area == 'GOA') %>% 
  select(agency_species_code)
Gs <- paste("", GOA_specs, "", collapse = ",")
Gs <- gsub('c', '', Gs)

# write query
Gs_query <- paste("
                  select year, fmp_area,	fmp_subarea, reporting_area_code, agency_gear_code, agency_species_code,
                       species_name, species_group_code, retained_or_discarded, weight_posted, gf_harvest_sector, 
                       deployment_trip_pk, monitoring_status,	sampling_strata_deployment_category
                from council.comprehensive_blend_ca
                where year between 2014 and 2023
                and (fmp_area = 'GOA') 
                and agency_species_code IN ", Gs, sep = "")

#query and format the data
GOA_catch <- sqlQuery(channel_akfin, Gs_query) %>% 
  clean_names() %>% 
  as.data.frame() %>% 
  group_by(year, fmp_area,	fmp_subarea, reporting_area_code, agency_gear_code, agency_species_code,
           species_name, species_group_code, retained_or_discarded, gf_harvest_sector, 
           deployment_trip_pk, monitoring_status,	sampling_strata_deployment_category) %>% 
  summarise(catch_mt = sum(weight_posted))

# Create output data file----
T6_catch <- GOA_catch %>% 
  bind_rows(BSAI_catch)

write_csv(T6_catch, paste0(getwd(), "/Data/confidential_T6_catch.csv"))

# OROX and DSR OFLs----
OROX <- x %>% 
  filter(Stock == "OROX")

OROX_codes <- unique(OROX %>% select(FMP, CAS_code))
OROX_codes <- OROX_codes %>% 
  select(CAS_code)
Os <- paste("", OROX_codes, "", collapse = ",")
Os <- gsub('c', '', Os)
# write query
Os_query <- paste("
                  select year, fmp_area,	fmp_subarea, reporting_area_code, agency_gear_code, agency_species_code,
                       species_name, species_group_code, retained_or_discarded, weight_posted, gf_harvest_sector, 
                       deployment_trip_pk, monitoring_status,	sampling_strata_deployment_category
                from council.comprehensive_blend_ca
                where year between 2013 and 2022
                and (fmp_area = 'GOA') 
                and agency_species_code IN ", Os, sep = "")

#query and format the data
OROX_catch <- sqlQuery(channel_akfin, Os_query) %>% 
  clean_names() %>% 
  as.data.frame() %>% 
  group_by(year, fmp_area, species_name, species_group_code) %>% 
  summarise(catch_mt = sum(weight_posted)) %>% 
  filter(species_group_code != "DEM1") %>% 
  group_by(species_name) %>% 
  summarise(maxC = max(catch_mt))

DSR_catch <- sqlQuery(channel_akfin, Os_query) %>% 
  clean_names() %>% 
  as.data.frame() %>% 
  group_by(year, fmp_area, species_name, species_group_code) %>% 
  summarise(catch_mt = sum(weight_posted)) %>% 
  filter(species_group_code == "DEM1") %>% 
  group_by(species_name) %>% 
  summarise(maxC = max(catch_mt))

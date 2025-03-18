# 2025-03-18

# Assemble inland data for threshold analysis
# Note, this initial threshold analysis was only completed on a subset of data found in the "data-raw" folder

# Raw data does not have preliminary QC applied
# Currently, thresholds are only applicable to river data
# Hourglass Lake and Piper Lake data to be added at a later date

library(data.table)
library(qaqcmar)
library(purrr)
library(here)
library(dplyr)
library(ggplot2)


## read in data ----------------------------------------------------------------
# Note, this will only read in files in the data-raw folder. 
files <- list.files(here("data-raw"), pattern = "rds", full.names = TRUE)

dat_raw <- files %>%
  purrr::map_dfr(readRDS) %>%
  subset(select = -c(dissolved_oxygen_uncorrected_mg_per_l))


# view waterbodies in dataset
unique(dat_raw$waterbody)

# view stations in dataset
unique(dat_raw$station)

# save original thresholds table
thresholds_all <- thresholds

## flag suspected outliers in suspect datsets ----------------------------------
# Stations which may have experienced air exposure:
# Liscomb 1, Liscomb 2 (very suspicious plots)
# LaHave 2 (very suspicious plot)
# Mersey 2 (dislodged and exposed upon retrieval)
# Tusket 3 (exposed on retrieval)
# Possibly Musquodoboit 1 (submerged in mud, water low but sensor not exposed) 
# Possibly Musquodoboit 2 (found in low pool, barely submerged)


# Liscomb 1 --------------------------------------------------------------------
# Liscomb River 1 Data Flagging
raw_liscomb1 <- dat_raw %>%
  filter(station == "Liscomb River 1")

#adjust SD test threshold value
county_i <- raw_liscomb1$county[1]

thresholds <- thresholds_all %>% 
  filter(qc_test == c("rolling_sd"), variable == "temperature_degree_c", county == county_i) 

thresholds$threshold_value = "1.5"

#apply test
qc_liscomb1 <- qc_test_rolling_sd(raw_liscomb1, county = county_i)

#plot
qc_liscomb1_long <- qc_pivot_longer(qc_liscomb1, qc_tests = "rolling_sd")
p <- qc_plot_flags(qc_liscomb1_long, qc_tests = "rolling_sd", var = "temperature_degree_c") 
p

#remove flagged values
clean_liscomb1 <- qc_liscomb1 %>%
  filter(rolling_sd_flag_temperature_degree_c <2) %>%
  mutate(rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_percent_saturation = NA) %>% 
  mutate(sensor_depth_measured_m = NA)



# Liscomb 2 --------------------------------------------------------------------
# Liscomb River 2 Data Flagging
raw_liscomb2 <- dat_raw %>%
  filter(station == "Liscomb River 2")

#adjust SD test threshold value
county_i <- raw_liscomb2$county[1]

thresholds <- thresholds_all %>% 
  filter(qc_test == c("rolling_sd"), variable == "temperature_degree_c", county == county_i) 

thresholds$threshold_value = "1.5"

#apply test
qc_liscomb2 <- qc_test_rolling_sd(raw_liscomb2, county = county_i)

#plot
qc_liscomb2_long <- qc_pivot_longer(qc_liscomb2, qc_tests = "rolling_sd")
p <- qc_plot_flags(qc_liscomb2_long, qc_tests = "rolling_sd", var = "temperature_degree_c") 
p

#remove flagged values
clean_liscomb2 <- qc_liscomb2 %>%
  filter(rolling_sd_flag_temperature_degree_c <2) %>%
  mutate(rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_percent_saturation = NA) %>% 
  mutate(sensor_depth_measured_m = NA)


# LaHave 2 ---------------------------------------------------------------------
# LaHave River 2 Data Flagging
raw_lahave2 <- dat_raw %>%
  filter(station == "LaHave River 2")

#adjust SD test threshold value
county_i <- raw_lahave2$county[1]

thresholds <- thresholds_all %>% 
  filter(qc_test == c("rolling_sd"), variable == "temperature_degree_c", county == county_i) 

thresholds$threshold_value = "1.5"

#apply test
qc_lahave2 <- qc_test_rolling_sd(raw_lahave2, county = county_i)

#plot
qc_lahave2_long <- qc_pivot_longer(qc_lahave2, qc_tests = "rolling_sd")
p <- qc_plot_flags(qc_lahave2_long, qc_tests = "rolling_sd", var = "temperature_degree_c") 
p

#remove flagged values
clean_lahave2 <- qc_lahave2 %>%
  filter(rolling_sd_flag_temperature_degree_c <2) %>%
  mutate(rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_percent_saturation = NA) %>% 
  mutate(sensor_depth_measured_m = NA)


# Mersey 2 ---------------------------------------------------------------------
# Mersey River 2 Data Flagging
raw_mersey2 <- dat_raw %>%
  filter(station == "Mersey River 2")

#adjust SD test threshold value
county_i <- raw_mersey2$county[1]

thresholds <- thresholds_all %>% 
  filter(qc_test == c("rolling_sd"), variable == "temperature_degree_c", county == county_i) 

thresholds$threshold_value = "1.5"

#apply test
qc_mersey2 <- qc_test_rolling_sd(raw_mersey2, county = county_i)

#plot
qc_mersey2_long <- qc_pivot_longer(qc_mersey2, qc_tests = "rolling_sd")
p <- qc_plot_flags(qc_mersey2_long, qc_tests = "rolling_sd", var = "temperature_degree_c") 
p

#remove flagged values
clean_mersey2 <- qc_mersey2 %>%
  filter(rolling_sd_flag_temperature_degree_c <2) %>%
  mutate(rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_percent_saturation = NA) %>% 
  mutate(sensor_depth_measured_m = NA)


# Tusket 3 ---------------------------------------------------------------------
# Tusket River 3 Data Flagging
raw_tusket3 <- dat_raw %>%
  filter(station == "Tusket River 3")

#adjust SD test threshold value
county_i <- raw_tusket3$county[1]

thresholds <- thresholds_all %>% 
  filter(qc_test == c("rolling_sd"), variable == "temperature_degree_c", county == county_i) 

thresholds$threshold_value = "1.5"

#apply test
qc_tusket3 <- qc_test_rolling_sd(raw_tusket3, county = county_i)

#plot
qc_tusket3_long <- qc_pivot_longer(qc_tusket3, qc_tests = "rolling_sd")
p <- qc_plot_flags(qc_tusket3_long, qc_tests = "rolling_sd", var = "temperature_degree_c") 
p

#remove flagged values
clean_tusket3 <- qc_tusket3 %>%
  filter(rolling_sd_flag_temperature_degree_c <2) %>%
  mutate(rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_percent_saturation = NA) %>% 
  mutate(sensor_depth_measured_m = NA)



# produce clean dataset---------------------------------------------------------
#to remove and re-add qc'ed stations
#flag entire dataset to get flag columns

qc_all <- dat_raw %>%
  mutate(rolling_sd_flag_temperature_degree_c = NA) %>%
  mutate(rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_percent_saturation = NA) %>%
  mutate(dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(sensor_depth_measured_m = NA)

#remove flagged values from datasets
dat_rm <- qc_all %>%
  filter(!(station == "Liscomb River 2" | 
             station ==  "Liscomb River 1" | 
             station == "LaHave River 2" |
             station == "Mersey River 2" |
             station == "Tusket River 3"))

dat_clean <- dat_rm %>%
  rbind(clean_lahave2) %>%
  rbind(clean_liscomb1) %>%
  rbind(clean_liscomb2) %>%
  rbind(clean_mersey2) %>%
  rbind(clean_tusket3) %>%
  subset(select = -c(rolling_sd_flag_temperature_degree_c))


## create a dataset of ALL inland data -----------------------------------------
path <- "R:/data_branches/inland_water_quality/processed_data/deployment_data"

rdslist <- list.files(path = path, pattern = ".rds", full.names = TRUE, recursive = TRUE)

dat_all <- rdslist %>%
  purrr::map_dfr(readRDS) %>%
  mutate(rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l = NA) %>%
  mutate(dissolved_oxygen_uncorrected_mg_per_l = NA)


# export rds -------------------------------------------------------------------
# clean dataset of select inland data from which to generate the thresholds:
saveRDS(dat_clean, here("data/2024-12-02_inland_data_prelim_qc_clean.rds"))

# uncleaned dataset with ALL inland data:
saveRDS(dat_all, here("data/2024-12-02_inland_data_raw.rds"))









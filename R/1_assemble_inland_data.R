
# NOTE: THIS IS THE OLD VERSION CREATED PRIOR TO DATA RE-PROCESSING. sEE 1.1_assemble_inland_data_new



# September 5, 2024

# Assemble inland data

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
path <- "R:/data_branches/inland_water_quality/processed_data/deployment_data"

rdslist <- list.files(path = path, pattern = ".rds", full.names = TRUE, recursive = TRUE)

dat_raw <- rdslist %>%
  purrr::map_dfr(readRDS)

# files <- list.files(here("data-raw"), pattern = "rds", full.names = TRUE)
# 
# dat_raw <- files %>%
#   purrr::map_dfr(readRDS) %>%
#   subset(select = -c(dissolved_oxygen_uncorrected_mg_per_l))
  

# View waterbodies in dataset
unique(dat_raw$waterbody)

# View stations in dataset
unique(dat_raw$station)


## flag suspected outliers in suspect datsets ----------------------------------
# Stations which may have experienced air exposure:
# Liscomb 1, Liscomb 2 (very suspicious plots)
# LaHave 2 (very suspicious plot)
# Mersey 2 (dislodged and exposed upon retrieval)
# Tusket 3 (exposed on retrieval)
# Possibly Musquodoboit 1 (submerged in mud, water low but sensor not exposed) 
# Possibly Musquodoboit 2 (found in low pool, barely submerged)


#add a row to the thresholds table
newrow_sd <- list(qc_test = "rolling_sd",
                  variable = "temperature_degree_c",
                  county = "river_data",
                  month = "NA",
                  sensor_type = "NA",
                  threshold = "rolling_sd_max",
                  threshold_value = "2")

thresholds <- thresholds %>% 
  filter(qc_test == c("rolling_sd"), variable == "temperature_degree_c") %>%
  rbind(newrow_sd)

#adjust SD test threshold value
thresholds[16,7] = "1.5"



# Liscomb 1
# Liscomb River 1 Data Flagging
suspect_liscomb1 <- dat_raw %>%
  filter(station == "Liscomb River 1")

#apply test
qc_liscomb1 <- qc_test_rolling_sd(suspect_liscomb1, county = "river_data")

#plot
qc2_liscomb1 <- qc_pivot_longer(qc_liscomb1, qc_tests = "rolling_sd")
p <- qc_plot_flags(qc2_liscomb1, qc_tests = "rolling_sd", var = "temperature_degree_c") 
p

#remove flagged values
clean_liscomb1 <- qc_liscomb1 %>%
  filter(rolling_sd_flag_temperature_degree_c <2)



# Liscomb 2
# Liscomb River 2 Data Flagging
suspect_liscomb2 <- dat_raw %>%
  filter(station == "Liscomb 2")

#adjust SD test threshold value
thresholds[16,7] = "1.5"

#apply test
qc_liscomb2 <- qc_test_rolling_sd(suspect_liscomb2, county = "river_data")

#plot
qc2_liscomb2 <- qc_pivot_longer(qc_liscomb2, qc_tests = "rolling_sd")
p <- qc_plot_flags(qc2_liscomb2, qc_tests = "rolling_sd", var = "temperature_degree_c") 
p

#remove flagged values
clean_liscomb2 <- qc_liscomb2 %>%
  filter(rolling_sd_flag_temperature_degree_c <2)


# LaHave 2
# LaHave River 2 Data Flagging
suspect_lahave2 <- dat_raw %>%
  filter(station == "LaHave 2")

#adjust SD test threshold value
thresholds[16,7] = "1.5"

#apply test
qc_lahave2 <- qc_test_rolling_sd(suspect_lahave2, county = "river_data")

#plot
qc2_lahave2 <- qc_pivot_longer(qc_lahave2, qc_tests = "rolling_sd")
p <- qc_plot_flags(qc2_lahave2, qc_tests = "rolling_sd", var = "temperature_degree_c") 
p

#remove flagged values
clean_lahave2 <- qc_lahave2 %>%
  filter(rolling_sd_flag_temperature_degree_c <2)


# Mersey 2
# Mersey River 2 Data Flagging
suspect_mersey2 <- dat_raw %>%
  filter(station == "Mersey 2")

#adjust SD test threshold value
thresholds[16,7] = "1.5"

#apply test
qc_mersey2 <- qc_test_rolling_sd(suspect_mersey2, county = "river_data")

#plot
qc2_mersey2 <- qc_pivot_longer(qc_mersey2, qc_tests = "rolling_sd")
p <- qc_plot_flags(qc2_mersey2, qc_tests = "rolling_sd", var = "temperature_degree_c") 
p

#remove flagged values
clean_mersey2 <- qc_mersey2 %>%
  filter(rolling_sd_flag_temperature_degree_c <2)


# Tusket 3
# Tusket River 3 Data Flagging
suspect_tusket3 <- dat_raw %>%
  filter(station == "Tusket 3")

#adjust SD test threshold value
thresholds[16,7] = "1.5"

#apply test
qc_tusket3 <- qc_test_rolling_sd(suspect_tusket3, county = "river_data")

#plot
qc2_tusket3 <- qc_pivot_longer(qc_tusket3, qc_tests = "rolling_sd")
p <- qc_plot_flags(qc2_tusket3, qc_tests = "rolling_sd", var = "temperature_degree_c") 
p

#remove flagged values
clean_tusket3 <- qc_tusket3 %>%
  filter(rolling_sd_flag_temperature_degree_c <2)




# produce clean dataset---------------------------------------------------------
# to remove and re-add qc'ed stations
#flag entire dataset to get flag columns
qc_all <- qc_test_rolling_sd(dat_raw, county = "river_data") 

#remove flagged values from datasets
dat_rm <- qc_all %>%
  filter(!(station == "Liscomb 2" | 
           station ==  "Liscomb 1" | 
           station == "LaHave 2" |
           station == "Mersey 2" |
           station == "Tusket 3"))

dat_clean <- dat_rm %>%
  rbind(clean_lahave2) %>%
  rbind(clean_liscomb1) %>%
  rbind(clean_liscomb2) %>%
  rbind(clean_mersey2) %>%
  rbind(clean_tusket3) %>%
  subset(select = -c(rolling_sd_flag_temperature_degree_c))

## read in newer data ----------------------------------------------------------------
path <- "R:/data_branches/inland_water_quality/processed_data/deployment_data"

rdslist <- list.files(path = path, pattern = ".rds", full.names = TRUE, recursive = TRUE)
dat_new <- rdslist %>%
  purrr::map_dfr(readRDS)


# export rds -------------------------------------------------------------------
# saveRDS(dat_raw, here("data/2024-09-05_inland_data_prelim_qc.rds"))
#  
saveRDS(dat_clean, here("data/2024-09-05_inland_data_prelim_qc_clean.rds"))

saveRDS(dat_new, here("data/2024-12-02_inland_data_raw.rds"))









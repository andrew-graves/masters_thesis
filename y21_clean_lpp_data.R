### Author: Andrew Graves
### Date: 3.5.19
### Goal: Clean Y21 LPP data

# Load packages

library(tidyverse)

############
# LPP Data #
############

# Read in data

# lpp_data <- read.table(file = "raw_baseline_single_trial_mast.txt", 
#                        header = TRUE, sep = ",")

lpp_data <- read_delim(file = "raw_baseline_single_trial_mast.txt", 
                       delim = ",") %>%
  filter(time_window == 1700) %>% # specify time window here
  rename(subj = subNo)

# Find identical and adjacent triggers (signifiying extra trigger) and
# remove them

#adjacent_triggers <- sequence(rle(lpp_data$trig_code)$lengths)-1
lpp_data <- #lpp_data[!adjacent_triggers,] %>%
  lpp_data %>% 
  group_by(subj) %>% 
  mutate(trial_number = row_number())

# Create stimulus condition columns from trigger code column

lpp_data$stim_seq <- car::recode(as.numeric(lpp_data$trig_code),
  "1:4 = 'US'; 5:8 = 'CS'") %>%
  factor()

lpp_data$stim_type <- car::recode(as.numeric(lpp_data$trig_code),
  "c(1,2,5,6) = 'face'; c(3,4,7,8) = 'scene'") %>%
  factor()

lpp_data$stim_sep <- car::recode(as.numeric(lpp_data$trig_code),
  "c(1,5) = 'face_cs_face_us'; c(3,6) = 'face_cs_scene_us';
   c(2,7) = 'scene_cs_face_us'; c(4,8) = 'scene_cs_scene_us'") %>%
  factor()

# Create columns with baseline subtracted from channels

lpp_data <- lpp_data %>%
  mutate(Cp1_corrected = Cp1 - baseline_Cp1, Cp2_corrected = Cp2 - baseline_Cp2,
         Cp5_corrected = Cp5 - baseline_Cp5, Cp6_corrected = Cp6 - baseline_Cp6,
         Cz_corrected = Cz - baseline_Cz)

# Create average LPP column

lpp_data$avg_lpp <- apply(data.frame(lpp_data$Cp1_corrected, 
  lpp_data$Cp2_corrected, lpp_data$Cp5_corrected, lpp_data$Cp6_corrected,
  lpp_data$Cz_corrected), 1, mean)

lpp_data <- lpp_data %>%
  select(subj, reject, trial_number, stim_seq, stim_type, stim_sep, 
         Cp1_corrected, Cp2_corrected, Cp5_corrected, Cp6_corrected, 
         Cz_corrected, avg_lpp)

# Write data to file

write.table(lpp_data, "lpp_data.txt")


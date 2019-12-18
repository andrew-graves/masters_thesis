### Author: Andrew Graves
### Date: 2.28.19
### Goal: Clean Y21 Eprime data

# Load packages

library(gdata)
library(tidyverse)

# Read in data

eprime_data <- read_csv(file = "Y21_eprime.csv") %>%
  rename(subj = Subject)

############################
# Behavioral Judgment Data #
############################

judge_vec <- c("StartBlock", "StartBlockCS", "EndBlockCS")

eprime_judge_data <- eprime_data %>%
  filter(Running %in% judge_vec) %>% 
  unite(col = stim_id, GrandImageList, CSImageList, sep = "", 
        remove = TRUE) %>%
  unite(col = arousal_resp, CSJudgment1Arous.RESP, CSJudgment2Arous.RESP,
        USJudgmentArous.RESP, sep = "", remove = TRUE) %>%
  unite(col = arousal_rt, CSJudgment1Arous.RT, CSJudgment2Arous.RT,
        USJudgmentArous.RT, sep = "", remove = TRUE) %>%
  unite(col = valence_resp, CSJudgment1Val.RESP, CSJudgment2Val.RESP,
        USJudgmentVal.RESP, sep = "", remove = TRUE) %>%
  unite(col = valence_rt, CSJudgment1Val.RT, CSJudgment2Val.RT,
        USJudgmentVal.RT, sep = "", remove = TRUE) 

eprime_judge_data$stim_condition <- eprime_judge_data %>%
  select(stim_id) %>%
  str_extract_all("NonSocUS|NonSocCS|SocUS|SocCS") %>%
  unlist()

eprime_judge_data <- eprime_judge_data %>%
  filter(stim_condition %in% c("NonSocUS", "SocUS")) %>%
  group_by(subj) %>% 
  mutate(trial_number = row_number()) %>%
  select(subj, stim_condition, trial_number, stim_id, arousal_resp, arousal_rt,
         valence_resp, valence_rt) %>%
  map_dfr(str_replace_all, "NA", "") %>%
  mutate(subj = as.numeric(subj), trial_number = as.numeric(trial_number),
         arousal_resp = as.numeric(arousal_resp), 
         arousal_rt = as.numeric(arousal_rt), 
         valence_resp = as.numeric(valence_resp),
         valence_rt = as.numeric(valence_rt))

write.table(eprime_judge_data, "eprime_judge_data.txt")

#######################
# Stimulus Trial Data #
#######################

trial_vec <- c("Block1", "Block2", "Block3", "Block4", "Block5", "Block6",
               "Block7", "Block8")

trial_data <- eprime_data %>%
  filter(Running %in% trial_vec) %>%
  unite(col = stim_id, NonSocUS1, NonSocUS2, SocUS1, SocUS2, sep = "", 
      remove = TRUE) %>%
  map_dfr(str_replace_all, "NA", "")

eprime_trial_data <- tibble(subj = as.numeric(
    interleave(matrix(trial_data$subj), matrix(trial_data$subj))),
  stim_id = as.factor(interleave(matrix(trial_data$Procedure), 
    matrix(trial_data$stim_id))),
  block = as.vector(interleave(matrix(trial_data$Running), 
    matrix(trial_data$Running))))

eprime_trial_data <- eprime_trial_data %>%
  group_by(subj) %>%
  mutate(trial_number = row_number()) %>%
  mutate(block_half = factor(if_else(trial_number <= 256, 
    "first_half", "second_half")))

write.table(eprime_trial_data, "eprime_trial_data.txt")

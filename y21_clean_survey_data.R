### Author: Andrew Graves
### Date: 2.27.19
### Goal: Clean Y21 survey data

# Load packages

library(tidyverse)

###############
# Survey Data #
###############

# Read in data

survey_data <- read_csv("Y21_qualtrics.csv") %>%
  select(-Progress, -`Duration (in seconds)`, -Finished, -Q130_6_TEXT) %>%
  rename(subj = Q133, race = Q130, gender = Q132)

# Simplify "Other" category

survey_data$race[grepl("Other", survey_data$race)] <- "Other"

#######
# AQ  #
#######

# Define start of AQ columns

aq_start <- which(colnames(survey_data) == "Q1") - 1

# Abbreviate words

dd <- "Definitely disagree"
sld <- "Slightly disagree"
sla <- "Slightly agree"
da <- "Definitely agree"

# Specifiy forward and reversed coded indices within AQ survey

aq_forward <- aq_start + c(1, 2, 4, 5, 6, 7, 9, 12, 13, 16, 18, 19, 20, 21, 22, 
                           23, 26, 33, 35, 39, 41, 42, 43, 45, 46)
aq_reverse <- aq_start + c(3, 8, 10, 11, 14, 15, 17, 24, 25, 27, 28, 29, 30, 31,
                           32, 34, 36, 37, 38, 40, 44, 47, 48, 49, 50)

# Specifiy AQ indices

aq_all <- sort(c(aq_forward, aq_reverse))

# Recode AQ forward and reverse words to numbers

survey_data %>%
  select(aq_forward)

survey_data[,aq_forward] <-  ifelse(survey_data[,aq_forward] == da, 3, 
                             ifelse(survey_data[,aq_forward] == sla, 2, 
                             ifelse(survey_data[,aq_forward] == sld, 1,
                             ifelse(survey_data[,aq_forward]== dd, 0, 'NA'))))

survey_data[,aq_reverse] <-  ifelse(survey_data[,aq_reverse] == da, 0, 
                             ifelse(survey_data[,aq_reverse] == sla, 1, 
                             ifelse(survey_data[,aq_reverse] == sld, 2,
                             ifelse(survey_data[,aq_reverse]== dd, 3, 'NA'))))

#######
# BAP #
#######

# Define start of BAP columns

bap_start <- which(colnames(survey_data) == "Q51") - 1

# Abbreviate words

vr <- "Very rarely"
rar <- "Rarely"
occ <- "Occasionally"
so <- "Somewhat often"
oft <- "Often"
vo <- "Very Often"

# Specifiy forward and reversed coded indices within BAP survey

bap_forward <- bap_start + c(2, 4, 5, 6, 8, 10, 11, 13, 14, 17, 18, 20, 22, 24, 
                             26, 27, 29, 31, 32, 33, 35)
bap_reverse <- bap_start + c(1, 3, 7, 9, 12, 15, 16, 19, 21, 23, 25, 28, 30, 34, 
                             36)

# Specifiy BAP indices

bap_all <- sort(c(bap_forward, bap_reverse))

# Specifiy sub-score indices within BAP survey

aloof <- length(aq_all) + aq_start + c(1, 5, 9, 12, 16, 18, 23, 25, 27, 28, 31, 
                                       36) 
pragmatic_language <- length(aq_all) + aq_start + c(2, 4, 7, 10, 11, 14, 17, 20, 
                                                    21, 29, 32, 34) 
rigid <- length(aq_all) + aq_start + c(3, 6, 8, 13, 15, 19, 22, 24, 26, 30, 33, 
                                       35)

# Recode BAP forward and reverse words to numbers

survey_data[,bap_forward] <- ifelse(survey_data[,bap_forward] == vr, 0, 
                             ifelse(survey_data[,bap_forward] == rar , 1,
                             ifelse(survey_data[,bap_forward] == occ , 2,
                             ifelse(survey_data[,bap_forward] == so , 3,
                             ifelse(survey_data[,bap_forward] == oft , 4,
                             ifelse(survey_data[,bap_forward] == vo , 5, NA)))))
                             )

survey_data[,bap_reverse] <- ifelse(survey_data[,bap_reverse] == vr, 5, 
                             ifelse(survey_data[,bap_reverse] == rar , 4,
                             ifelse(survey_data[,bap_reverse] == occ , 3,
                             ifelse(survey_data[,bap_reverse] == so , 2,
                             ifelse(survey_data[,bap_reverse] == oft , 1,
                             ifelse(survey_data[,bap_reverse] == vo , 0, NA)))))
                             )

########
# SIAS #
########

# Define start of SIAS columns

sias_start <- which(colnames(survey_data) == "Q87") - 1

# Abbreviate words

not <- 'Not at all'
slight <- 'Slightly'
mod <- 'Moderately'
very <- 'Very'
ext <- 'Extremely'

# Specifiy forward and reversed coded indices within SIAS survey

sias_forward <- sias_start + c(1, 2, 3, 4, 6, 7, 8, 10, 12, 13, 14, 15, 16, 17, 
                               18, 19, 20)
sias_reverse <- sias_start + c(5, 9, 11)

# Specifiy SIAS indices

sias_all <- sort(c(sias_forward, sias_reverse))

# Recode SIAS forward and reverse words to numbers

survey_data[,sias_forward] <- ifelse(survey_data[,sias_forward] == not, 0, 
                              ifelse(survey_data[,sias_forward] == slight , 1,
                              ifelse(survey_data[,sias_forward] == mod , 2,
                              ifelse(survey_data[,sias_forward] == very , 3,
                              ifelse(survey_data[,sias_forward] == ext , 4, NA))
                              )))

survey_data[,sias_reverse] <- ifelse(survey_data[,sias_reverse] == not, 4, 
                              ifelse(survey_data[,sias_reverse] == slight , 3,
                              ifelse(survey_data[,sias_reverse] == mod , 2,
                              ifelse(survey_data[,sias_reverse] == very , 1,
                              ifelse(survey_data[,sias_reverse] == ext , 0, NA))
                              )))

#########
# PANAS #
######### 

# Define start of PANAS columns

panas_start <- which(colnames(survey_data) == "Q107") - 1

# Abbreviate words

lo <- 'Very slightly or not at all'
medLo <- 'A little'
med <- 'Moderately'
medHi <- 'Quite a bit'
hi <- 'Extremely'

# Specifiy positive and negative affect indices within PANAS survey

positive_affect <- panas_start + c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)
negative_affect <- panas_start + c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)

# Specifiy PANAS indices

panas_all <- sort(c(positive_affect, negative_affect))

# Recode PANAS positive and negative words to numbers

survey_data[,positive_affect] <- ifelse(survey_data[,positive_affect] == lo, 0, 
                                 ifelse(survey_data[,positive_affect] == medLo ,
                                 1,
                                 ifelse(survey_data[,positive_affect] == med , 2
                                 ,
                                 ifelse(survey_data[,positive_affect] == medHi ,
                                 3,
                                 ifelse(survey_data[,positive_affect] == hi, 4,
                                 'NA')))))

survey_data[,negative_affect] <- ifelse(survey_data[,negative_affect] == lo, 0, 
                                 ifelse(survey_data[,negative_affect] == medLo ,
                                 1,
                                 ifelse(survey_data[,negative_affect] == med , 2
                                 ,
                                 ifelse(survey_data[,negative_affect] == medHi ,
                                 3,
                                 ifelse(survey_data[,negative_affect] == hi, 4,
                                 'NA')))))

#########################
# Finishing Survey Data #
#########################

# Convert survey data from factor to numeric (suppress warnings for NAs)

survey_data[(aq_start+1):ncol(survey_data)] <- 
  suppressWarnings(sapply(survey_data[(aq_start+1):ncol(survey_data)],
  as.numeric))

# Calculate sum scores for each participant on each survey and sub-score

col_list <- list(aq_all, bap_all, aloof, pragmatic_language, rigid, sias_all,
                 positive_affect, negative_affect)
get_score <- function(data, col_list){
  rowSums(data[,col_list])
}

score_list <- map(col_list, get_score, data = survey_data)
survey_scores <- do.call(cbind, score_list) %>%
  data.frame()
colnames(survey_scores) <- c("aq", "bap", "bap_aloof", "bap_pragmatic_language",
                             "bap_rigid", "sias", "positive_affect", 
                             "negative_affect")
survey_scores$panas <- survey_scores$negative_affect - 
  survey_scores$positive_affect

# Scale continous predictors for modeling

scale_start <- which(colnames(survey_scores) == "aq")
scale_end <- which(colnames(survey_scores) == "panas")
survey_scores[,scale_start:scale_end] <- apply(
  survey_scores[,scale_start:scale_end], 2, scale)
survey_data <- survey_data %>%
  select(subj, race, gender) %>%
  bind_cols(survey_scores)

# Write data to file

write.table(survey_data, "survey_data.txt")

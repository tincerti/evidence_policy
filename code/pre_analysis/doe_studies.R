################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)
library(stargazer)

# Import data
doe = read.csv('data/DoE/WWC-export-archive-2019-May-22-110152/Interventions_Studies_And_Findings.csv')

################################################################################
# General cleaning
################################################################################
# Create intervention-outcome level variable
doe = mutate(doe,
      inter_outcome = paste(i_Intervention_Name, i_Outcome_Domain, sep = '_'))

# Create list of needed variables 
vars = as.vector(c("i_Intervention_Name", "inter_outcome", "i_Protocol", "i_Outcome_Domain",
         "s_Intervention_Name", "s_Protocol", "s_StudyID", "s_Product_Name", 
         "s_Study_Design", "s_Study_Rating", "s_Publication", "s_Rating_Reason", 
         "s_Ineligibility_Reason", "s_interventionID",
         "i_NumStudiesEligible", "i_NumStudiesMeetingStandards", 
         "f_Outcome_Measure", "f_Outcome_MeasureID", "f_Outcome_Domain",
         "f_Finding_Rating", "f_Is_Statistically_Significant", "f_p_Value_Study", 
         "f_p_Value_WWC", "f_ESSA_Rating", "f_Effect_Size_Study", 
         "f_Effect_Size_WWC"))

# Create reduced dataset
doe_reduced = doe %>% select(one_of(vars)) 

# Clean variable levels
doe$s_Study_Rating = str_replace(doe$s_Study_Rating, 
                                 "Meets WWC standards with reservations", 
                                 "Moderate")

doe$s_Study_Rating = str_replace(doe$s_Study_Rating, 
                                 "Meets WWC standards without reservations", 
                                 "High")

doe$s_Study_Rating = str_replace(doe$s_Study_Rating, 
                                 "Does not meet WWC standards", 
                                 "Low")

doe$f_Finding_Rating = str_replace(doe$f_Finding_Rating, 
                                 "Meets WWC standards with reservations", 
                                 "Moderate")

doe$f_Finding_Rating = str_replace(doe$f_Finding_Rating, 
                                 "Meets WWC standards without reservations", 
                                 "High")

doe$f_Finding_Rating = str_replace(doe$f_Finding_Rating, 
                                 "Does not meet WWC standards", 
                                 "Low")

################################################################################
# Locate interventions with high and low quality studies + significant findings
################################################################################
# Counts
interventions = length(unique(doe$i_Intervention_Name))
outcomes = length(unique(doe$i_Outcome_Domain))
intervention_outcome = length(unique(doe$inter_outcome))

# Drop insignificant findings and no intervention name
doe_wwc = doe %>% 
  filter(as.character(s_Study_Rating) == as.character(f_Finding_Rating),
         f_Is_Statistically_Significant == "True",
         s_Study_Rating != "", 
         f_Finding_Rating != "",
         i_Intervention_Name != "") %>%
  select(one_of(vars))

# Create list of all interventions with more than one evidence tier
wwc = doe_wwc %>%
  group_by(inter_outcome, s_Study_Rating) %>%
  summarise(N = n()) %>%
  group_by(inter_outcome) %>% 
  summarise(tiers = n()) %>%
  filter(tiers >= 2)

# Show interventions, outcomes, and tiers of evidence of this subset
wwc_studies = doe_wwc %>%
  group_by(inter_outcome, s_Study_Rating, s_Study_Design, s_Rating_Reason,
           s_Publication) %>%
  summarise()

wwc_studies = left_join(wwc, wwc_studies, by = "inter_outcome")

wwc_studies = wwc_studies %>%
  separate(inter_outcome, c("Intervention", "Outcome"), sep = "_") %>%
  select(-tiers)

################################################################################
# Use ESSA ratings
################################################################################
# Drop insignificant findings and no intervention name
doe_essa = doe %>% 
  filter(f_ESSA_Rating != "", 
         inter_outcome != "_") %>%
  select(one_of(vars))

# ESSA
essa = doe_essa %>%
  group_by(inter_outcome, f_ESSA_Rating) %>%
  summarise(N = n()) %>%
  group_by(inter_outcome) %>% 
  summarise(tiers = n()) %>%
  filter(tiers >= 2)

# Show interventions, outcomes, and tiers of evidence of this subset
essa_studies = doe_essa %>%
  group_by(inter_outcome, f_ESSA_Rating, s_Study_Design, s_Rating_Reason,
           s_Publication) %>%
  summarise()

essa_studies = left_join(essa, essa_studies, by = "inter_outcome")

essa_studies = essa_studies %>%
  separate(inter_outcome, c("Intervention", "Outcome"), sep = "_") %>%
  select(-tiers)

################################################################################
# Output tables to LaTeX
################################################################################
# Clean list of interventions
wwc = wwc %>% 
  separate(inter_outcome, c("Intervention", "Outcome"), sep = "_") %>%
  select(-tiers)

essa = essa %>% 
  separate(inter_outcome, c("Intervention", "Outcome"), sep = "_") %>%
  select(-tiers)

# Output tables to LaTeX
stargazer(wwc, 
          header=FALSE, 
          summary = FALSE,
          #title = "Interventions with all three evidence tiers",
          column.sep.width = "1mm",
          font.size = "tiny",
          label = "all_tiers",
          out = "tables/interventions_wwc.tex")

# Output tables to LaTeX
stargazer(essa, 
          header=FALSE, 
          summary = FALSE,
          #title = "Interventions with all three evidence tiers",
          column.sep.width = "1mm",
          font.size = "tiny",
          label = "all_tiers",
          out = "tables/interventions_essa.tex")

################################################################################
# Locate interventions with high and low quality studies + significant findings
################################################################################

success = doe %>% 
  filter(inter_outcome == "Success for All®_Alphabetics") %>%
  select(one_of(vars))

odyssey = doe %>% 
  filter(inter_outcome == "Odyssey® Math_General Mathematics Achievement") %>%
  select(one_of(vars))

accelerated = doe %>% 
  filter(inter_outcome == "Accelerated Reader_Comprehension") %>%
  select(one_of(vars))

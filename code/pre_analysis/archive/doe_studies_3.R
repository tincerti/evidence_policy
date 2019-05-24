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
# Locate eligible studies
################################################################################
# Create list of needed variables 
vars = as.vector(c("i_Intervention_Name", "i_Protocol", "i_Outcome_Domain",
         "s_Intervention_Name", "s_Protocol", "s_StudyID", "s_Product_Name", 
         "s_Study_Design", "s_Study_Rating", "s_Publication", "s_Rating_Reason", 
         "s_Ineligibility_Reason", "s_interventionID",
         "i_NumStudiesEligible", "i_NumStudiesMeetingStandards", 
         "f_Outcome_Measure", "f_Outcome_MeasureID", "f_Outcome_Domain",
         "f_Finding_Rating", "f_Is_Statistically_Significant", "f_p_Value_Study", 
         "f_p_Value_WWC", "f_ESSA_Rating", "f_Effect_Size_Study", 
         "f_Effect_Size_WWC"))

doe_reduced = doe %>% select(one_of(vars)) 

# Create intervention-outcome level variable
doe = mutate(doe,
      inter_outcome = paste(i_Intervention_Name, i_Outcome_Domain, sep = '_'))

# Total number of intervention - outcome combinations
intervention_outcome = length(unique(doe$inter_outcome))

# Create list of all interventions with all three evidence tiers (findings)
all_tiers_findings = doe %>%
  filter(f_Finding_Rating != "" & i_Intervention_Name != "") %>%
  group_by(inter_outcome, f_Finding_Rating) %>% 
  summarise(N = n()) %>%
  group_by(inter_outcome) %>% 
  summarise(tiers = n()) %>%
  filter(tiers == 3)

all_tiers_findings = as.vector(unlist(all_tiers_findings$inter_outcome))

# Create list of all interventions with all three evidence tiers (studies)
all_tiers_studies = doe %>%
  filter(s_Study_Rating != "") %>%
  group_by(inter_outcome, s_Study_Rating) %>% 
  summarise(N = n()) %>%
  group_by(inter_outcome) %>% 
  summarise(tiers = n()) %>%
  filter(tiers == 3)

all_tiers_studies = as.vector(unlist(all_tiers_studies$inter_outcome))

# Add number of studies conducted to list of interventions with 3 evidence tiers
all_tiers_num_studies = doe %>%
  filter(inter_outcome %in% all_tiers_findings) %>%
  group_by(inter_outcome,
           i_NumStudiesEligible, i_NumStudiesMeetingStandards) %>%
  summarise() %>%
  group_by(inter_outcome) %>%
  summarise(`Eligible Studies` = sum(i_NumStudiesEligible), 
            `Meet Standards` = sum(i_NumStudiesMeetingStandards)) %>%
  separate(inter_outcome, c("Intervention", "Outcome"), sep = "_")

# Look for studies with SIGNIFICANT results in all evidence tiers
all_tiers_sig = doe %>%
  filter(inter_outcome %in% all_tiers_findings) %>%
  group_by(inter_outcome, f_Finding_Rating, f_Is_Statistically_Significant,
           i_NumStudiesEligible, i_NumStudiesMeetingStandards) %>%
  summarise() %>%
  arrange(inter_outcome, f_Finding_Rating, f_Is_Statistically_Significant) %>%
  filter(f_Is_Statistically_Significant == "True") %>%
  group_by(inter_outcome) %>%
  summarise(`Eligible Studies` = mean(i_NumStudiesEligible), 
            `Meet Standards` = mean(i_NumStudiesMeetingStandards),
            tiers = n()) %>%
  filter(tiers == 3) %>%
  separate(inter_outcome, c("Intervention", "Outcome"), sep = "_") %>%
  select("Intervention", "Outcome", "Eligible Studies", "Meet Standards")

# Look at individual studies within interventions
success = doe %>% 
  filter(inter_outcome == "Success for All®_Alphabetics") %>%
  select(one_of(vars))

accelerated = doe %>% 
  filter(inter_outcome == "Accelerated Reader_Comprehension") %>%
  select(one_of(vars))
  


# Output tables to LaTeX
stargazer(all_tiers_num_studies, 
          header=FALSE, 
          summary = FALSE,
          #title = "Interventions with all three evidence tiers",
          column.sep.width = "1mm",
          font.size = "tiny",
          label = "all_tiers",
          out = "tables/all_tiers.tex")

stargazer(all_tiers_sig, 
          header=FALSE, 
          summary = FALSE,
          #title = "Interventions with significant results in all three evidence tiers",
          column.sep.width = "1mm",
          font.size = "footnotesize",
          label = "all_tiers",
          out = "tables/all_tiers_sig.tex")













################################################################################
# Additional exploratory work: Examine studies by topic
################################################################################
# Filter datasets to topics or interventions
lit = doe %>% filter(s_Topic_Literacy == 1)
math = doe %>% filter(s_Topic_Mathematics == 1)
science = doe %>% filter(s_Topic_Science == 1)
behavior = doe %>% filter(s_Topic_Behavior == 1)
success = doe %>% filter(inter_outcome == "Success for All®_Alphabetics")

# Summarize outcome variables
outcomes = doe %>% 
  group_by(i_Outcome_Domain) %>% 
  summarise(N = n()) %>%
  rename(Outcome = i_Outcome_Domain) %>% 
  arrange(desc(N))

# Examine protocols within mathetics achievement
math_overview = doe %>% 
  filter(i_Outcome_Domain == "General Mathematics Achievement") %>%
  group_by(i_Protocol) %>% 
  summarise (N = n()) %>%
  arrange(desc(N))

# Examine studies of primary school mathematics achievement
primary_math = doe %>% 
  filter(i_Outcome_Domain == "General Mathematics Achievement" & 
         i_Protocol == "Primary Mathematics") %>%
  group_by(i_Intervention_Name, f_Intervention_Name,
           f_Finding_Rating, 
           f_Is_Statistically_Significant) %>% 
  summarise (N = n()) %>%
  arrange(desc(N))

# Create list of all studies and their evidence tiers
all_standards = doe %>%
  filter(f_Finding_Rating != "" & i_Intervention_Name != "") %>%
  group_by(i_Intervention_Name, f_Finding_Rating) %>% 
  summarise(N = n())


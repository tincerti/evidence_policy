###############################################################################
# Preamble and Libraries
###############################################################################
# Preamble
rm(list=ls())

# Libraries
library(tidyverse)
library(stargazer)
library(reshape2)
library(ggthemes)
library(DeclareDesign)
library(ri2)
library(commarobust)
library(sandwich)

theme_set(theme_bw())

###############################################################################
# Setup
###############################################################################
# Basic information about experimental setting
sims = 10000  # Number of simulations
N = 1000  # Size of the experimental sample
t = 4 # Number of treatment groups
m = N/t  # Number of units to be assigned to each treatment arm

# Define hypothesized potential outcomes in each treatment group: mean
mean_t1 = 0.3
mean_t2 = 0.3 * .9
mean_t3 = 0.3 * 1.05
mean_t4 = 0.3 * 1.125

# Define hypothesized potential outcomes in each treatment group: standard dev
sd_t1 = 0.08
sd_t2 = 0.08
sd_t3 = 0.08
sd_t4 = 0.08

# Create empty vectors of ATE, p-value, and significant p-values
ate = rep(NA, sims)
pvalue = rep(NA, sims)
success = rep(NA, sims)

for (i in 1:t){ 
 assign(paste("ate_t", i, sep=""), ate) # ATE estimates
 assign(paste("pvalue_t", i, sep=""), pvalue) # P-values
 assign(paste("success_t", i, sep=""), success) # Success/failure of each trial
} 

# Create macro variables
test = tibble(.rows = N)

###############################################################################
# Power calculation
###############################################################################
set.seed(30)

# Power calculation
for (i in 1:sims) {
  
  # Create hypothetical schedule of potential outcomes
  test$t1 = rnorm(N, mean = mean_t1, sd = sd_t1)
  test$t2 = rnorm(N, mean = mean_t2, sd = sd_t2)
  test$t3 = rnorm(N, mean = mean_t3, sd = sd_t3)
  test$t4 = rnorm(N, mean = mean_t4, sd = sd_t4)

  # Assign to treatment or control
  dec = declare_ra(N = N, num_arms = t)
  test$z = conduct_ra(dec)
  
  # Reveal treatment or control outcomes for units
  test$d = with(test, ifelse(z == "T1", t1, 0))
  test$d = with(test, ifelse(z == "T2", t2, d))
  test$d = with(test, ifelse(z == "T3", t3, d))
  test$d = with(test, ifelse(z == "T4", t4, d))
  
  # Estimate the ATE
  model = estimatr::lm_robust(d ~ z, data = test)
  
  ate_t2[i] = summary(model)$coefficients[2]
  ate_t3[i] = summary(model)$coefficients[3]
  ate_t4[i] = summary(model)$coefficients[4]
  
  # Report Outcomes
  pvalue_t2[i] = summary(model)$coefficients[2, 4]
  pvalue_t3[i] = summary(model)$coefficients[3, 4]
  pvalue_t4[i] = summary(model)$coefficients[4, 4]
  
  success_t2[i] = as.numeric(summary(model)$coefficients[2, 4] <= 0.05)
  success_t3[i] = as.numeric(summary(model)$coefficients[3, 4] <= 0.05)
  success_t4[i] = as.numeric(summary(model)$coefficients[4, 4] <= 0.05)
}

# Calculate statistical power 
power_t2 = mean(success_t2)
power_t3 = mean(success_t3)
power_t4 = mean(success_t4)

# Merge p-value information into single data frame
pvalue_t2 = data_frame(pvalue_t2)
pvalue_t3 = data_frame(pvalue_t3)
pvalue_t4 = data_frame(pvalue_t4)

pvalue_t2 = pvalue_t2 %>% rename(pvalue = pvalue_t2) %>% mutate(treatment = 2)
pvalue_t3 = pvalue_t3 %>% rename(pvalue = pvalue_t3) %>% mutate(treatment = 3)
pvalue_t4 = pvalue_t4 %>% rename(pvalue = pvalue_t4) %>% mutate(treatment = 4)

pvalue = rbind(pvalue_t2, pvalue_t3, pvalue_t4)
pvalue$pvalue = round(pvalue$pvalue, 2)

###############################################################################
# Power analysis histogram
###############################################################################
# Define names of treatment groups
treatment_names <- c(
  `2` = "Low quality evidence + Information provision",
  `3` = "High quality evidence + No information provision",
  `4` = "High quality evidence + Information provision"
)

# Define facet groups (i.e. power results)
pvalue$label = round(with(pvalue, ifelse(treatment == 2, power_t2, NA)), 2)
pvalue$label = round(with(pvalue, ifelse(treatment == 3, power_t3, label)), 2)
pvalue$label = round(with(pvalue, ifelse(treatment == 4, power_t4, label)), 2)

# Create plot
power = ggplot(pvalue, aes(pvalue, 
                           color = pvalue > 0.05,
                           fill = pvalue > 0.05,
                           label = paste("Power = ", label))) +
  geom_density(stat = "count", alpha = 0.5) + 
  geom_vline(xintercept = 0.05, linetype = "dashed", size = 0.2) +
  facet_wrap( ~ treatment, ncol = 1, labeller = as_labeller(treatment_names)) +
  scale_y_continuous(limits = c(0, sims)) +
  geom_text(stat = "identity", position = "identity",
            check_overlap = TRUE, x = .5, y = 7500, color = "black") +
  scale_fill_manual(values = c("seagreen2", "firebrick1"),
                     labels = c("True", "False"),
                     name = "P value is less than 0.05:") +
  scale_color_manual(values = c("seagreen2", "firebrick1"),
                    labels = c("True", "False"),
                    name = "P value is less than 0.05:") +
  xlab("p-value") +
  ylab("Simulations") +
  theme_classic() +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=14)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

ggsave("figs/power_state.pdf", power, height = 6, width = 6)
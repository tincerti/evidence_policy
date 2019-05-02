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
# Set a seed
set.seed(30)

# Basic information about experimental setting
sims = 10  # Number of simulations
N = 535  # Size of the experimental sample
t = 8 # Number of treatment groups
m = N/t  # Number of units to be assigned to each treatment arm

# Define hypothesized potential outcomes in each treatment group: mean
mean_t1 = 0.3
mean_t2 = 0.3 * 1.01
mean_t3 = 0.3 * 1.05
mean_t4 = 0.3 * 1.10
mean_t5 = 0.3 * 1.15
mean_t6 = 0.3 * 1.20
mean_t7 = 0.3 * 1.25
mean_t8 = 0.3 * 1.30

# Define hypothesized potential outcomes in each treatment group: standard dev
sd_t1 = 0.01
sd_t2 = 0.01
sd_t3 = 0.01
sd_t4 = 0.01
sd_t5 = 0.01
sd_t6 = 0.01
sd_t7 = 0.01
sd_t8 = 0.01

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
# Power calculation
for (i in 1:sims) {
  
  # Create hypothetical schedule of potential outcomes
  test$t1 = rnorm(N, mean = mean_t1, sd = sd_t1)
  test$t2 = rnorm(N, mean = mean_t2, sd = sd_t2)
  test$t3 = rnorm(N, mean = mean_t3, sd = sd_t3)
  test$t4 = rnorm(N, mean = mean_t4, sd = sd_t4)
  test$t5 = rnorm(N, mean = mean_t5, sd = sd_t5)
  test$t6 = rnorm(N, mean = mean_t6, sd = sd_t6)
  test$t7 = rnorm(N, mean = mean_t7, sd = sd_t7)
  test$t8 = rnorm(N, mean = mean_t8, sd = sd_t8)

  # Assign to treatment or control
  dec = declare_ra(N = N, num_arms = t)
  test$z = conduct_ra(dec)
  
  # Reveal treatment or control outcomes for units
  test$d = with(test, ifelse(z == "T1", t1, 0))
  test$d = with(test, ifelse(z == "T2", t2, d))
  test$d = with(test, ifelse(z == "T3", t3, d))
  test$d = with(test, ifelse(z == "T4", t4, d))
  test$d = with(test, ifelse(z == "T5", t5, d))
  test$d = with(test, ifelse(z == "T6", t6, d))
  test$d = with(test, ifelse(z == "T7", t7, d))
  test$d = with(test, ifelse(z == "T8", t8, d))
  
  # Estimate the ATE
  model = estimatr::lm_robust(d ~ z, data = test)
  
  ate_t2[i] = summary(model)$coefficients[2]
  ate_t3[i] = summary(model)$coefficients[3]
  ate_t4[i] = summary(model)$coefficients[4]
  ate_t5[i] = summary(model)$coefficients[5]
  ate_t6[i] = summary(model)$coefficients[6]
  ate_t7[i] = summary(model)$coefficients[7]
  ate_t8[i] = summary(model)$coefficients[8]
  
  # Report Outcomes
  pvalue_t2[i] = summary(model)$coefficients[2, 4]
  pvalue_t3[i] = summary(model)$coefficients[3, 4]
  pvalue_t4[i] = summary(model)$coefficients[4, 4]
  pvalue_t5[i] = summary(model)$coefficients[5, 4]
  pvalue_t6[i] = summary(model)$coefficients[6, 4]
  pvalue_t7[i] = summary(model)$coefficients[7, 4]
  pvalue_t8[i] = summary(model)$coefficients[8, 4]
  
  success_t2[i] = as.numeric(summary(model)$coefficients[2, 4] <= 0.05)
  success_t3[i] = as.numeric(summary(model)$coefficients[3, 4] <= 0.05)
  success_t4[i] = as.numeric(summary(model)$coefficients[4, 4] <= 0.05)
  success_t5[i] = as.numeric(summary(model)$coefficients[5, 4] <= 0.05)
  success_t6[i] = as.numeric(summary(model)$coefficients[5, 4] <= 0.05)
  success_t7[i] = as.numeric(summary(model)$coefficients[5, 4] <= 0.05)
  success_t8[i] = as.numeric(summary(model)$coefficients[5, 4] <= 0.05)
}

# Calculate statistical power 
power_t2 = mean(success_t2)
power_t3 = mean(success_t3)
power_t4 = mean(success_t4)
power_t5 = mean(success_t5)
power_t6 = mean(success_t6)
power_t7 = mean(success_t7)
power_t8 = mean(success_t8)

# Merge p-value information into single data frame
pvalue_t2 = data_frame(pvalue_t2)
pvalue_t3 = data_frame(pvalue_t3)
pvalue_t4 = data_frame(pvalue_t4)
pvalue_t5 = data_frame(pvalue_t5)
pvalue_t6 = data_frame(pvalue_t6)
pvalue_t7 = data_frame(pvalue_t7)
pvalue_t8 = data_frame(pvalue_t8)

pvalue_t2 = pvalue_t2 %>% rename(pvalue = pvalue_t2) %>% mutate(treatment = 2)
pvalue_t3 = pvalue_t3 %>% rename(pvalue = pvalue_t3) %>% mutate(treatment = 3)
pvalue_t4 = pvalue_t4 %>% rename(pvalue = pvalue_t4) %>% mutate(treatment = 4)
pvalue_t5 = pvalue_t5 %>% rename(pvalue = pvalue_t5) %>% mutate(treatment = 5)
pvalue_t6 = pvalue_t6 %>% rename(pvalue = pvalue_t6) %>% mutate(treatment = 6)
pvalue_t7 = pvalue_t7 %>% rename(pvalue = pvalue_t7) %>% mutate(treatment = 7)
pvalue_t8 = pvalue_t8 %>% rename(pvalue = pvalue_t8) %>% mutate(treatment = 8)

pvalue = rbind(pvalue_t2, pvalue_t3, pvalue_t4, pvalue_t5, pvalue_t6, 
               pvalue_t7, pvalue_t8)
pvalue$pvalue = round(pvalue$pvalue, 2)

###############################################################################
# Power analysis histogram
###############################################################################
treatment_names <- c(
  `2` = "Treatment 2",
  `3` = "Treatment 3",
  `4` = "Treatment 4",
  `5` = "Treatment 5",
  `6` = "Treatment 6",
  `7` = "Treatment 7",
  `8` = "Treatment 8"
)

power = ggplot(pvalue, aes(pvalue, 
                           color = pvalue > 0.05,
                           fill = pvalue > 0.05)) +
  geom_density(stat = "count", alpha = 0.5) + 
  geom_vline(xintercept = 0.05, linetype = "dashed", size = 0.2) +
  facet_wrap( ~ treatment, ncol = 2, labeller = as_labeller(treatment_names)) +
  scale_y_continuous(limits = c(0, sims)) +
  scale_fill_manual(values = c("seagreen2", "firebrick1"),
                     labels = c("True", "False"),
                     name = "P value is less than 0.05:") +
  scale_color_manual(values = c("seagreen2", "firebrick1"),
                    labels = c("True", "False"),
                    name = "P value is less than 0.05:") +
  xlab("p-value") +
  ylab("Count") +
  theme_classic() +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=14)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

ggsave("figs/power.pdf", power, height = 6, width = 7)
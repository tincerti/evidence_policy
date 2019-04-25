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
library(randomizr)
library(fabricatr)
library(estimatr)
library(ri2)
library(commarobust)
library(sandwich)

theme_set(theme_bw())

###############################################################################
# Power calculations
###############################################################################
# Set a seed
set.seed(30)

# Basic information
sims = 10000  # Number of simulations
N = 535  # Size of the experimental sample
t = 8 # Number of treatment groups
m = N/t  # Number of units to be assigned to each treatment arm

# Create empty vectors of ATE, p-value, and significant p-values
ate_t = list()
for(i in 1:t){ate_t[[i]] <- rep(NA, sims) } 
names(ate_t) <- paste0("vect", 1:1000)


ate_t = c()
for (i in 1:t) {
  ate_t[[i]] = rep(NA, sims) 
}

ate_t1 = rep(NA, sims)  # ATE estimate from T1
pvalue_t1 = rep(NA, sims)  # p-value from T1
success_t1 = rep(NA, sims)  # report the success of each experiment here

ate_t2 = rep(NA, sims)  # ATE estimate from T2
pvalue_t2 = rep(NA, sims)  # p-value from T1
success_t2 = rep(NA, sims)  # report the success of each experiment here

ate_t3 = rep(NA, sims)  # ATE estimate from T3
pvalue_t3 = rep(NA, sims)  # p-value from T1
success_t3 = rep(NA, sims)  # report the success of each experiment here

ate_t4 = rep(NA, sims)  # ATE estimate from T4
pvalue_t4 = rep(NA, sims)  # p-value from T1
success_t4 = rep(NA, sims)  # report the success of each experiment here

# Create macro variables
test = data_frame(id = c(1:(N)))

# Power calculation
for (i in 1:sims) {
  
  # Create hypothetical schedule of potential outcomes
  test$c1 = rnorm(N, mean = .05, sd = .01)
  test$t1 = rnorm(N, mean = test$c1*1.01, sd = .01)
  test$t2 = rnorm(N, mean = test$c1*1.05, sd = .01)
  test$t3 = rnorm(N, mean = test$c1*1.10, sd = .01)
  test$t4 = rnorm(N, mean = test$c1*1.25, sd = .01)

  # Assign to treatment or control
  dec = declare_ra(N = N, num_arms = 5, 
                   conditions = c("C1", "T1", "T2", "T3", "T4"))
  test$z = conduct_ra(dec)
  
  # Reveal treatment or control outcomes for units
  test$d = with(test, ifelse(z == "C1", c1, 0))
  test$d = with(test, ifelse(z == "T1", t1, d))
  test$d = with(test, ifelse(z == "T2", t2, d))
  test$d = with(test, ifelse(z == "T3", t3, d))
  test$d = with(test, ifelse(z == "T4", t4, d))
  
  # Estimate the ATE
  model = estimatr::lm_robust(d ~ z, data = test)
  
  ate_t1[i] = summary(model)$coefficients[2]
  ate_t2[i] = summary(model)$coefficients[3]
  ate_t3[i] = summary(model)$coefficients[4]
  ate_t4[i] = summary(model)$coefficients[5]
  
  # Report Outcomes
  pvalue_t1[i] = summary(model)$coefficients[2, 4]
  pvalue_t2[i] = summary(model)$coefficients[3, 4]
  pvalue_t3[i] = summary(model)$coefficients[4, 4]
  pvalue_t4[i] = summary(model)$coefficients[5, 4]
  
  success_t1[i] = as.numeric(summary(model)$coefficients[2, 4] <= 0.05)
  success_t2[i] = as.numeric(summary(model)$coefficients[3, 4] <= 0.05)
  success_t3[i] = as.numeric(summary(model)$coefficients[4, 4] <= 0.05)
  success_t4[i] = as.numeric(summary(model)$coefficients[5, 4] <= 0.05)
}

# Calculate statistical power 
power_t1 = mean(success_t1)
power_t2 = mean(success_t2)
power_t3 = mean(success_t3)
power_t4 = mean(success_t4)

# Merge p-value information into single data frame
pvalue_t1 = data_frame(pvalue_t1)
pvalue_t2 = data_frame(pvalue_t2)
pvalue_t3 = data_frame(pvalue_t3)
pvalue_t4 = data_frame(pvalue_t4)

pvalue_t1 = pvalue_t1 %>% rename(pvalue = pvalue_t1) %>% mutate(treatment = 1)
pvalue_t2 = pvalue_t2 %>% rename(pvalue = pvalue_t2) %>% mutate(treatment = 2)
pvalue_t3 = pvalue_t3 %>% rename(pvalue = pvalue_t3) %>% mutate(treatment = 3)
pvalue_t4 = pvalue_t4 %>% rename(pvalue = pvalue_t4) %>% mutate(treatment = 4)

pvalue = rbind(pvalue_t1, pvalue_t2, pvalue_t3, pvalue_t4)
pvalue$pvalue = round(pvalue$pvalue, 2)

###############################################################################
# Power analysis histogram
###############################################################################
treatment_names <- c(
  `1` = "Treatment 1",
  `2` = "Treatment 2",
  `3` = "Treatment 3",
  `4` = "Treatment 4"
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
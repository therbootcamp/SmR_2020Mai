
library(speff2trial)
library(tidyverse)

trial <- as_tibble(ACTG175)

trial <- trial %>%
  mutate(hemo = factor(hemo, levels = c(0, 1), labels = c("No", "Yes")),
         drugs = factor(drugs, levels = c(0, 1), labels = c("No", "Yes")),
         oprior = factor(oprior, levels = c(0, 1), labels = c("No", "Yes")),
         z30 = factor(z30, levels = c(0, 1), labels = c("No", "Yes")),
         homo = factor(homo, levels = c(0, 1), labels = c("No", "Yes")),
         zprior = factor(zprior, levels = c(0, 1), labels = c("No", "Yes")),
         race = factor(race, levels = c(0, 1), labels = c("White", "Non-White")),
         gender = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
         str2 = factor(str2, levels = c(0, 1), labels = c("Naive", "Experienced")),
         strat = factor(strat, levels =  c(1, 2, 3), labels = c("None", "Between 1 and 52 Weeks",
                                                           "More than 52 Weeks")),
         symptom = factor(symptom, levels = c(0, 1), labels = c("Asymptomatic", "Symptomatic")),
         treat = factor(treat, levels = c(0, 1), labels = c("Zidovudine only", "Other")),
         offtrt = factor(offtrt, levels = c(0, 1), labels = c("No", "Yes")),
         r = factor(r, levels = c(0, 1), labels = c("Missing", "Observed")),
         arms = factor(arms, levels = c(0, 1, 2, 3), labels = c("zidovudine", "zidovudine and didanosine", 
                                                                "zidovudine and zalcitabine", "didanosine"))) %>%
  mutate_if(is.factor, paste)
         
         

write_csv(trial, path = "1_Data/trial_ACTG175.csv")

#HW2
library(tidyverse)
library(ggplot2)
library(readr)
#readindata
BioNTech <- read_csv("C:/desktop/biontech_adolescents (1).csv")
#Separate into separate groups#
Treatment <- filter(BioNTech, group == "vaccine")
Control <- filter(BioNTech, group == "placebo")
#Sum positive COVID tests
vax <- sum(Treatment$outcome == "COVID-19")
unvax <- sum(Control$outcome == "COVID-19")
#Choose tail and perform binomial exact test
binom.test(0,1131,18/1129,alternative = "less")
treatment_covid <- sum(Treatment$outcome == "COVID-19")
treatment_no_covid <- sum(Treatment$outcome != "COVID-19")
control_covid <- sum(Control$outcome == "COVID-19")
control_no_covid <- sum(Control$outcome != "COVID-19")

data <- data.frame(
  Group = rep(c("Treatment", "Control"), each = 2),
  Outcome = c("COVID", "No COVID", "COVID", "No COVID"),
  Count = c(treatment_covid, treatment_no_covid, control_covid, control_no_covid)
)
ggplot(data, aes(x = Group, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "COVID Outcomes by Treatment and Control Groups",
       x = "Group",
       y = "Count",
       fill = "Outcome")


#H0: π = p (The population proportion of positve COVID tests among vaccinated indivudals is the same as the control group)
#H0: π < p (The population proportion of positve COVID tests among vaccinated indivudals is less than the control group)
#P = 1.276e-08
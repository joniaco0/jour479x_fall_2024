#HW2
library(tidyverse)
#readindata
BioNTech<- read.csv("biontech_adolescents.csv")
#Separate into separate groups#
Treatment <- filter(BioNTech, group == "vaccine")
Control <- filter(BioNTech, group == "placebo")
#Sum positive COVID tests
vax <- sum(Treatment$outcome == "COVID-19")
unvax <- sum(Control$outcome == "COVID-19")
#Choose tail and perform binomial exact test
binom.test(0,1131,18/1129,alternative = "less")
library(ggplot2)
chart<-ggplot(data=BioNTech, aes(x=dose, y=len)) +
  geom_bar(stat="identity")

#H0: π = p (The population proportion of positve COVID tests among vaccinated indivudals is the same as the control group)
#H0: π < p (The population proportion of positve COVID tests among vaccinated indivudals is less than the control group)
#P = 1.276e-08
#Because P < 0.05, we can reject the null that the control group and vaccinated group 
#are the same probablity and there is statstically signifcant evidence that 
#that the probablity of COVID in the vaccine group is less than the control
#6- Assuming no random error, there is a true control group and the
# only difference is recieving the vaccine which means there is grounds for causation
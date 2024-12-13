library(googlesheets4)
library(tidyverse)
library(dplyr)
library(ggplot2)
NBAplayers = read_sheet("https://docs.google.com/spreadsheets/d/1eJ5BV-S4vy0DPsrENh_L-Q9VYgwmnTQiOeSHWawqNhc/edit?gid=0#gid=0")
NBAteams = read_sheet("https://docs.google.com/spreadsheets/d/1lHVKe28VY8DfM-UaQNchnHNAeOHTo9jSMybWMEoEVJk/edit?gid=0#gid=0")
View(NBAplayers)
NBAplayers3 = NBAplayers[-c(1,4,5,9,10,11,12,13,14,15,16,17,18,19,20,21,24,29)]
View(NBAplayers3)
Ageppg = NBAplayers3 %>% group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgppg = mean(PTS))
Ageast = NBAplayers3 %>% group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgast = mean(AST))
Agemp = NBAplayers3 %>% group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgmp = mean(MP))
Ageorb = NBAplayers3 %>% group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgorb = mean(ORB))
Agedrb = NBAplayers %>% group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgdrb = mean(DRB))
Agestl = NBAplayers3 %>% group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgstl = mean(STL))
Ageblk = NBAplayers3 %>% group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgblk = mean(BLK))
Agetov = NBAplayers3 %>% group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgtov = mean(TOV))

Agestat = data.frame(Ageppg,Ageast[-c(1)],
Agemp[-c(1)],Ageorb[-c(1)],Agedrb[-c(1)],Agestl[-c(1)],
Ageblk[-c(1)],Agetov[-c(1)])
View(Agestat)


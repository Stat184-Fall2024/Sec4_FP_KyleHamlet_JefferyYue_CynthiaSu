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




NBAplayers2 = NBAplayers[-c(1,3,4,5,6,7,8,9,10,11,12,13,15,16,
                  17,18,19,20,22,23,24,25,26,27,28,29,30,31)]
NBAplayers2 = na.omit(NBAplayers2)
NBAplayers22 = as.numeric(unlist(NBAplayers2[2]))
NBAplayers23 = as.numeric(unlist(NBAplayers2[3]))
NBAplayers21 = data.frame(NBAplayers2[1],NBAplayers22,NBAplayers23)
Thrperc = NBAplayers21 %>% group_by(Player) %>%
  summarise(Thrperc = mean(NBAplayers22))
FTperc = NBAplayers21 %>% group_by(Player) %>%
  summarise(FTperc = mean(NBAplayers23))

Shotcomp = data.frame(Thrperc,FTperc[-c(1)])
View(Shotcomp)
ggplot(
  data = Shotcomp, 
  aes(x=as.numeric(unlist(Shotcomp[3])),y=as.numeric(unlist(Shotcomp[2])))) +  
    geom_point() + 
    labs(x = "% of Free Throws Made", 
         y = "% of 3s Made", 
         title = "Free Throw% Compared to 3 Point% in NBA Players") +
    theme_bw() + stat_smooth(method = "lm", 
                             formula = y ~ x)


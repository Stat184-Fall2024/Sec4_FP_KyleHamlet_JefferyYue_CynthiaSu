#load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

#read csv files
NBAplayers = read.csv("NBA Players 1999-2023 - Sheet1.csv")
NBAteams = read.csv("NBA Teams 1999-2023 - Sheet1.csv")
NBAteamsA = read.csv("NBA Teams ADvanced 1999-2023 - Sheet1.csv")

View(NBAteamsA)
NBAplayers3 <- NBAplayers %>%
  # Drop unnecessary columns
  select(-c(1, 4, 5, 9:21, 24, 29))

# Avg points per game
Ageppg <- NBAplayers3 %>%
  group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgppg = mean(PTS, na.rm = TRUE))

# Avg Assists Per Game
Ageast <- NBAplayers3 %>%
  group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgast = mean(AST, na.rm = TRUE))

# Avg Minutes Per Game
Agemp <- NBAplayers3 %>%
  group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgmp = mean(MP, na.rm = TRUE))

# Avg Offensive Rebounds
Ageorb <- NBAplayers3 %>%
  group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgorb = mean(ORB, na.rm = TRUE))

# Avg Defensive Rebounds
Agedrb <- NBAplayers3 %>%
  group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgdrb = mean(DRB, na.rm = TRUE))

# Avg Steals
Agestl <- NBAplayers3 %>%
  group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgstl = mean(STL, na.rm = TRUE))

# Avg Blocks
Ageblk <- NBAplayers3 %>%
  group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgblk = mean(BLK, na.rm = TRUE))

# Avg Turnovers
Agetov <- NBAplayers3 %>%
  group_by(NBAplayers3[c(2)]) %>%
  summarise(Avgtov = mean(TOV, na.rm = TRUE))

#Agestat dataframe
Agestat = data.frame(Ageppg,Ageast[-c(1)],
Agemp[-c(1)],Ageorb[-c(1)],Agedrb[-c(1)],Agestl[-c(1)],
Ageblk[-c(1)],Agetov[-c(1)])
View(Agestat)

#Visual 1
ggplot(
  data = Agestat,
  aes(x=Age..,y=Avgppg,fill=Age..)
) + geom_bar(stat = "identity", width = 0.5) +
geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "blue") +
labs(x = "Player Age",y = "Average Points Per Game", 
title = "Average Points Scored at Different Ages in the NBA")

#Visual 2
ggplot(data = Agestat,
       aes(x=Age..,y=Avgast,fill=Age..)
) + geom_bar(stat = "identity", width = 0.5) + 
geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "blue") +
  labs(x = "Player Age", y = "Average Assists Per Game",
       title = "Average Assists Made at Different Ages in the NBA")

#Visual 3
ggplot(data = Agestat,
       aes(x=Age..,y=(Avgorb+Avgdrb),fill=Age..)
) + geom_bar(stat = "identity", width = 0.5) + 
geom_smooth(aes(group = 1, y = (Avgorb + Avgdrb)), method = "lm", se = TRUE, color = "blue") +
  labs(x = "Player Age", y = "Average Rebounds Per Game",
       title = "Average Rebounds at Different Ages in the NBA")


NBAplayers2 <- NBAplayers %>%
  # Drop unnecessary columns
  select(-c(1, 3:13, 15:20, 22:31))
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

#Visual 4
ggplot(
  data = Shotcomp, 
  aes(x=as.numeric(unlist(Shotcomp[3])),y=as.numeric(unlist(Shotcomp[2])))
  ) +  
    geom_point() + 
    labs(x = "% of Free Throws Made", 
         y = "% of 3s Made", 
         title = "Free Throw% Compared to 3 Point% in NBA Players") +
    theme_bw() + stat_smooth(method = "lm", 
                             formula = y ~ x)


WinRate = (NBAteamsA[4]/(NBAteamsA[5]+NBAteamsA[4]))
WinRateComp = data.frame(NBAteamsA[2],WinRate,NBAteamsA[6],NBAteamsA[7],NBAteamsA[16])
View(WinRateComp)

#Visual 5
ggplot(
  data = WinRateComp,
  aes(x=as.numeric(unlist(WinRateComp[3])),y=as.numeric(unlist(WinRateComp[2])))
) +
  geom_point() + 
  labs(x = "Points Scored(Per 100 Possessions)",
       y = "Win Rate",
       title = "Correlation Between Win Rate and Points Scored")+
 theme_bw() + stat_smooth(method = "lm",
                          formula = y ~ x)

#Visual 6
ggplot(
  data = WinRateComp,
  aes(x=as.numeric(unlist(WinRateComp[4])),y=as.numeric(unlist(WinRateComp[2])))
) +
  geom_point() + 
  labs(x = "Points Allowed(Per 100 Possessions)",
       y = "Win Rate",
       title = "Correlation Between Win Rate and Points Allowed")+
  theme_bw() + stat_smooth(method = "lm",
                           formula = y ~ x)

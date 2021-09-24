title: "homework 2" 
author: ""
date: "09/20/2021"

#study group names (Taulant, Rimo, Joaqin)

library(dplyr)
library(ggplot2)

summary(acs2017_ny$DEGFIELD)
summary(acs2017_ny$HHINCOME)
summary(acs2017_ny$AGE)
summary(acs2017_ny$RACED)

mean(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Agriculture") ])
mean(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Psychology") ])
sd(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Agriculture") ])
sd(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Psychology") ])
var(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Agriculture") ])
var(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Psychology") ])
median(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Agriculture") ])
median(acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Psychology") ])

mean(acs2017_ny$RACED[ (acs2017_ny$DEGFIELD == "Agriculture") ])
mean(acs2017_ny$RACED[ (acs2017_ny$DEGFIELD == "Psychology") ])
var(acs2017_ny$RACED[ (acs2017_ny$DEGFIELD == "Agriculture") ])
var(acs2017_ny$RACED[ (acs2017_ny$DEGFIELD == "Psychology") ])
median(acs2017_ny$RACED[ (acs2017_ny$DEGFIELD == "Agriculture") ])
median(acs2017_ny$RACED[ (acs2017_ny$DEGFIELD == "Psychology") ])
sd(acs2017_ny$RACED[ (acs2017_ny$DEGFIELD == "Agriculture") ])
sd(acs2017_ny$RACED[ (acs2017_ny$DEGFIELD == "Psychology") ])

mean(acs2017_ny$HHINCOME[ (acs2017_ny$DEGFIELD == "Agriculture") ])
mean(acs2017_ny$HHINCOME[ (acs2017_ny$DEGFIELD == "Psychology") ])
sd(acs2017_ny$HHINCOME[ (acs2017_ny$DEGFIELD == "Agriculture") ])
sd(acs2017_ny$HHINCOME[ (acs2017_ny$DEGFIELD == "Psychology") ])
median(acs2017_ny$HHINCOME[ (acs2017_ny$DEGFIELD == "Agriculture") ])
median(acs2017_ny$HHINCOME[ (acs2017_ny$DEGFIELD == "Psychology") ])
var(acs2017_ny$HHINCOME[ (acs2017_ny$DEGFIELD == "Agriculture") ])
var(acs2017_ny$HHINCOME[ (acs2017_ny$DEGFIELD == "Psychology") ])

agriculture <- acs2017_ny$DEGFIELD[(acs2017_ny$DEGFIELD == "Agriculture")]
psychology <- acs2017_ny$DEGFIELD[(acs2017_ny$DEGFIELD == "Psychology")]

summary(agriculture)
summary(psychology)

acs2017_ny$Covid_risk <- ((acs2017_ny$PUMA > 4600) & acs2017_ny$PUMA < 6000) |
  ((acs2017_ny$PUMA > 8500) & (acs2017_ny$PUMA < 8700))
agriculture_employed_under66 <- (acs2017_ny$AGE[ (acs2017_ny$DEGFIELD == "Agriculture" &
                                               acs2017_ny$AGE < 66 & acs2017_ny$EMPSTAT == 2)])

library(dplyr)

ddply(acs2017_ny, .(DEGFIELD), summarize, mean = round(mean(AGE), 2), sd = round(sd(AGE), 2), n_obsv = length(PUMA))
ddply(acs2017_ny, .(DEGFIELD), summarize, mean = round(mean(HHINCOME), 2)
      ddply(acs2017_ny, .(DEGFIELD), summarize, mean = round(mean(HHINCOME), 2), sd = round(sd(HHINCOME), 2), n_obsv = length(PUMA))
      ddply(acs2017_ny, .(DEGFIELD), summarize, mean = round(mean(HHINCOME), 2), sd = round(sd(HHINCOME), 2), n_obsv = length(PUMA))
      dat_use1 <- subset(acs2017_ny,((HHINCOME > 0) & in_NYC))
      ddply(dat_use1, .(PUMA), summarize, inc90 = quantile(HHINCOME,probs = 0.9), inc10 = quantile(HHINCOME,probs = 0.1), n_obs = length(HHINCOME))
      table(DEGFIELD_index,female)
      ddply(acs2017_ny, .(DEGFIELD), summarize, mean = round(mean(RENT), 2), sd = round(sd(RENT), 2), n_obsv = length(PUMA))




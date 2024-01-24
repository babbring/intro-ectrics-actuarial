library(tidyverse)
library(stargazer)
setwd("C:/Users/Bram/Desktop/econometrie")
load("nlsy97.rdata ")
view(nlsy97)

#creating a vector of all the ranges of white collar job codes
white_collar_ranges <- c(seq(10, 430), seq(500, 950), seq(1000, 1240), seq(1300, 1530),
                         seq(1540, 1560), seq(1600, 1760), seq(1800, 1860), seq(1900, 1960),
                         seq(2000, 2060), seq(2100, 2150), seq(2200, 2340), seq(2400, 2550),
                         seq(2800, 2960), seq(3000, 3260), seq(3300, 3650), seq(4700, 4960),
                         seq(5000, 5930))

data <- nlsy97 %>% 
  subset(wage > 0) %>% 
  subset(ASVAB > 0) %>% 
  mutate(ASVAB = ASVAB / 1000) %>% 
  subset(jobcode > 0 & jobcode < 9950) %>% 
  subset(tenure2017 > 0) %>% 
  subset(hgrade2 >= 0 & hgrade <= 20) %>% 
  mutate(lwage = log(wage)) %>% 
  mutate(exp = tenure2017 + tenure2017^2) %>% 
  mutate(wcollar = ifelse(jobcode %in% white_collar_ranges, 1, 0))

nt <- nrow(data)
print(nt)

#drawing a random sample
n <- 4000
rs <- round(1000000000*runif(1), d=0)
set.seed(rs, kind=NULL)
print(rs)
iid <- sample(1:nt,n,replace=FALSE)
sample <- data[iid,]
attach(sample)

reg1 <- lm(lwage ~ 1 + ASVAB)
reg2 <- lm(lwage ~ 1 + ASVAB + hgrade2 + exp + wcollar + male)
reg3 <- lm(lwage ~ 1 + ASVAB + hgrade2 + exp + wcollar + male + wcollar*ASVAB + male*ASVAB)

stargazer(reg1, reg2, reg3)

library(stargazer)
setwd("C:/Users/Bram/Desktop/econometrie")

load("mysample.rdata")
mysample <- mysample[, c("white", "male", "black", "hispanic", "mixed", "ASVAB", "hgrade2", "wcollar", "exp", "wage")]
attach(mysample)

reg1 <- lm(log(wage) ~ 1 + ASVAB)
reg2 <- lm(log(wage) ~ 1 + ASVAB + hgrade2 + exp + wcollar + male + black + hispanic + mixed)
reg3 <- lm(log(wage) ~ 1 + ASVAB + hgrade2 + exp + wcollar + male + black + hispanic + mixed+ wcollar*ASVAB + male*ASVAB)
stargazer(reg1, reg2, reg3, omit.stat=c("LL","ser","f","adj.rsq"), type="text")
stargazer(as.data.frame(mysample), type="text", nobs=FALSE, min.max=FALSE)

wcollar <- subset(mysample, wcollar==1)
bcollar <- subset(mysample, wcollar==0)
male <- subset(mysample, male==1)
female <- subset(mysample, male==0)


stargazer(wcollar, type="text", nobs=FALSE, min.max=FALSE)
stargazer(bcollar, type="text", nobs=FALSE, min.max=FALSE)
stargazer(subset(mysample, mixed==1), type="text", nobs=FALSE, min.max=FALSE)

cor(ASVAB, hgrade2)

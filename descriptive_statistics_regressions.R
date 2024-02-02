library(stargazer) #importing a library that allows exporting
#the tables to LaTeX
setwd("C:/Users/Bram/Desktop/econometrie") #setting working directory

load("mysample.rdata") #loading data
mysample <- mysample[, c("ASVAB", "wage", "hgrade2", "wcollar", "exp"
                         , "male", "white", "black", "hispanic")]
#including only the variables used in the sample
attach(mysample) #attaching the sample

#estimating the 3 regression models
reg1 <- lm(log(wage) ~ 1 + ASVAB)
reg2 <- lm(log(wage) ~ 1 + ASVAB + hgrade2 +
             wcollar + exp + male + black + hispanic)
reg3 <- lm(log(wage) ~ 1 + ASVAB + hgrade2 + wcollar + exp +
             male + black + hispanic + wcollar*ASVAB + male*ASVAB)


stargazer(reg1, reg2, reg3, omit.stat=c("LL","ser","f","adj.rsq"))
#making a table for all the regression

stargazer(as.data.frame(mysample), nobs=FALSE,
          min.max=FALSE, title="Full Sample")
#making a table for the statistics of the full sample

#making tables for some different categories
stargazer(as.data.frame(subset(mysample, male==0)),
          nobs=FALSE, min.max=FALSE, title="Female")
stargazer(as.data.frame(subset(mysample, male==1)),
          nobs=FALSE, min.max=FALSE, title="Male")
stargazer(as.data.frame(subset(mysample, wcollar==0)),
          nobs=FALSE, min.max=FALSE, title="Blue-collar")
stargazer(as.data.frame(subset(mysample, wcollar==1))
          nobs=FALSE, min.max=FALSE, title="White-collar")
stargazer(as.data.frame(subset(mysample, white==0)),
          nobs=FALSE, min.max=FALSE, title="White")
stargazer(as.data.frame(subset(mysample, black==0)),
          nobs=FALSE, min.max=FALSE, title="Black")
stargazer(as.data.frame(subset(mysample, hispanic==1)),
          nobs=FALSE, min.max=FALSE, title"Hispanic")

#calculating the correlation between ASVAB score and the highest 
#grade completed
cor(ASVAB, hgrade2)

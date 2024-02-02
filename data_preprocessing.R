library(tidyverse) #importing a popular set of R libraries
                  #that include some useful functions
setwd("C:/Users/Bram/Desktop/econometrie")#setting the working directory
load("nlsy97.rdata ") #loading the data
view(nlsy97) #this view function give us a full view of 
             #the dataset in a new window

#creating a vector of job codes that are in the white collar category
white_collar_ranges <- c(seq(10, 430), seq(500, 950), seq(1000, 1240),
                      seq(1300, 1530), seq(1540, 1560), seq(1600, 1760), 
                      seq(1800, 1860), seq(1900, 1960), seq(2000, 2060),
                      seq(2100, 2150), seq(2200, 2340), seq(2400, 2550),
                      seq(2800, 2960), seq(3000, 3260), seq(3300, 3650), 
                      seq(4700, 4960), seq(5000, 5930))

data <- nlsy97 %>% # this "%>%" is called a pipe and 
                   #is part of the tidyverse
  subset(wage >= 0) %>% #delete all negative values for wage
  mutate(wage = wage/100) %>%  #scaling the wage
  subset(ASVAB >= 0) %>% #deleting all negative values for ASVAB
  mutate(ASVAB = ASVAB / 1000) %>% #scaling the ASVAB score
  subset(jobcode > 0 & jobcode < 9950) %>% #deleting negative job codes
                                        #and the special jobs category
  subset(exp > 0) %>% #deleting all negative values for experience
  subset(hgrade2 >= 0 & hgrade <= 20) %>% 
  #deleting all invalid values for schooling
  mutate(wcollar = ifelse(jobcode %in% white_collar_ranges, 1, 0)) %>% 
  #defining the wcollar dummy variable as 1 for all observations that
  #have ajobcode in the white_collar_ranges vector, and 0 for the rest
  subset(mixed != 1) #deleting the mixed race category

nt <- nrow(data) # assigning the amount of observations to variable nt
print(nt) #printing nt

#drawing a random sample
n <- 4000 #setting the sample size
rs <- round(1000000000*runif(1), d=0) 
#drawing a random seed form the uniform distribution
set.seed(rs, kind=NULL) #setting the random seed
print(rs) #printing the random seed
iid <- sample(1:nt,n,replace=FALSE) 
#defining the indices of the observations that 
#are included in the sample
mysample <- data[iid,] #defining the sample
save(mysample,file="mysample.rdata") #saving the sample locally

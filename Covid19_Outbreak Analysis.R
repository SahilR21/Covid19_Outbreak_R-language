#Project - 1
#Name - Covid19 Cases Outbreak Analysis

#Removes all varibles stored previously
rm(list = ls())
#install.packages("Hmisc")
library(Hmisc)  # use for different purpose of data analysis

#Data
covid19_data = read.csv("COVID19_line_list_data.csv")

describe(covid19_data) #Hmisc command

#Look the info and find that dealth column contain 3 different type of data i.e. 0,1 and date itself so we have to clear this data
#creating the diffferent column for this - clean up the death column
covid19_data$death_dummy = as.integer(covid19_data$death != 0)

#calculating dealth rate
# look for how many people die from the number of people affected
sum(covid19_data$death_dummy) / nrow(covid19_data)

#Age
#claim according to media : avg age of person die from corona visus older than person who servive
dead = subset(covid19_data,death_dummy == 1)
alive = subset(covid19_data,death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
#IS THIS STATISTICALLY SIGNIFICANT? - as difference between dead and alive is 20 year
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.95)
#normally, if p-values < 0.05, we reject null hypothesis
#here, p ~ 0, so we reject the null hypothesis and conclude that data statistically significant.

#Gender
#Claim - gender has no effect
men = subset(covid19_data, gender == "male")
women = subset(covid19_data, gender == "female")
mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)
#is this statistically significant
t.test(men$death_dummy,women$death_dummy,alternative = "two.sided",conf.level = 0.95)
# here p = 0.002105 which is  << 0.05 so hence statistically signicance and we conclude
# hence men have likley high dealth rate compared to women.




































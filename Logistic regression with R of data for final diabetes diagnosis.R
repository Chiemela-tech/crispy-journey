g <- read.csv(file = "final-diabetes-data-for-R-csv.csv", header = TRUE, sep = ',')

### McFadden's r-squared
#design your logistic regression
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit))

#check your model
summary(full_model)


#run a null model
null_model <- glm(dm ~ 1, family = binomial(link = logit))

#check
summary(null_model)

#calculate McFadden's R-square
R2 <- 1-logLik(full_model)/logLik(null_model)

#print it
R2




### C- Statistic
#design your logistic regression
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit))

#check your model
summary(full_model)

Cstat(full_model)



### Hosmer-Lemeshow Statistic and test

#install package "ResourceSelection"
install.packages("ResourceSelection")
require(ResourceSelection)

#design your logistic regression
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit))

full_model$y

#run Hosmer-Lemeshow test
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10)

HL

#plot the observed vs expected number of cases for each of the 10 groups
plot(HL$observed[, "y1"], HL$expected[, "yhat1"])

#plot the observed vs expected number of noncases for each of the 10 groups
plot(HL$observed[, "y0"], HL$expected[, "yhat0"])

#plot the observed vs expected prevalence for each of the 10 groups
plot(x = HL$observed[, "y1"]/(HL$observed[, "y1"]+HL$observed[, "y0"]),
     y = HL$expected[, "yhat1"]/(HL$expected[, "yhat1"]+HL$expected[, "yhat0"])


##Hosmer and Lemeshow goodness of fit (GOF) test
##data:  full_model$y, fitted(full_model)
##X-squared = 8.4533, df = 8, p-value = 0.3905
#Verify resultwith another package

#install package("generalhoslem")
#load package
require(generalhoslem)

#run Hosmer-Lemeshow test
logitgof(obs = full_model$y, exp = fitted(full_model), g = 10)

#design your logistic regression
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit))

#analyse table of deviance
anova(full_model, test = "Chisq")

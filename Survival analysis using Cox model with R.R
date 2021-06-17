g <- read.csv(file = "simulated-HF-mort-data-for-GMPH-_1K_-final-_2_.csv", header = TRUE, sep = ',')

install.packages("survival")
library("survival")

cox <- coxph(Surv(fu_time, death) ~ age, data=g)
summary(cox)

install.packages("survminer")

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = g) #take variables stright from g
summary(cox)

ethnicgroup <- as.factor(g[,"ethnicgroup"]) #can also use "as.factor" rather than "factor"
fu_time <- g[,"fu_time"]
death <- g[,"death"]

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

##Return to those missing ethnic groups
levels(ethnicgroup) <- c(levels(ethnicgroup), "8") #add level 8 to the factor
ethnicgroup[is.na(ethnicgroup)] <- "8" #change NA to "None"

#rerun the Cox model
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

##Run the descriptives on the five variables to go into the model; "summary" for continuous variables and "table" for categorical ones

##Age - a continuous variable
age <- g[, "age"]
summary(age)

#gender
gender <- g[, "gender"]
t <- table(gender, exclude = NULL)
addmargins(t) #adds the total (a "sum" column)
round(100*prop.table(t), digits = 1) #to get %s rounded to 1dp

#copd (chronic obstructive pulmonary disease)
copd <- g[,"copd"]
t <- table(copd, exclude = NULL)
addmargins(t) #adds the total (a "sum" column)
round(100*prop.table(t), digits = 1) #get &s rounded to 1dp

#prior_dnas (Prior OPD appointments missed)
prior_dnas <- g[, "prior_dnas"]
t <- table(prior_dnas, exclude = NULL)
addmargins(t) #adds the total (a "sum" column)
round(100*prop.table(t), digits = 1) #get %s rounded to 1dp

#ethnoc group
ethnicgroup <- g[,"ethnicgroup"]
t <- table(ethnicgroup, exclude = NULL)
addmargins(t) #adds the total (a "sum" column)
round(100*prop.table(t), digits = 1)


##HOW TO RUN MULTIPLE COX MODEL
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + prior_dnas + ethnicgroup)
summary(cox)

ethnicgroup <- as.factor(g[,"ethnicgroup"])
quintile <- as.factor(g[, "quintile"])

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup)
summary(cox)

table(quintile, exclude = NULL)
t <- table(quintile, death)
t #just the counts 

round(100*prop.table(t,1), digits = 1) #row &s

levels(ethnicgroup) <- c(levels(ethnicgroup), "8") #add level 8 to the factor
ethnicgroup[is.na(ethnicgroup)] <- "8" #change NA to "None"

levels(quintile) <- c(levels(quintile), "0") #add level 0 to the factor
quintile[is.na(quintile)] <- "0" #change NA to "None"

###Feedback on fixing a non-converging model
##1.Change the reference category
quintile <- relevel(quintile, ref = 2) #quintile 1 as the ref cat again
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup)
summary(cox)

## 2. Combine categories as there's a problem with quintile 0, then combine with quintile 5
quintile_5groups <- g[, "quintile"] #best start with the original data set, not from "quintile"
quintile_5groups[quintile_5groups==0] <- 5
quintile_5groups <- factor(quintile_5groups)
table(quintile_5groups, exclude = NULL)

#now run the model with this new variable
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_5groups + ethnicgroup)
summary(cox)

##3.Drop the quintile zero patients
quintile_5groups <- g[, "quintile"]
quintile_5groups[quintile_5groups==0] <- NA #set the zeroes to missing
quintile_5groups <- factor(quintile_5groups)
table(quintile_5groups, exclude = NULL)

#run the same model
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_5groups + ethnicgroup) 
summary(cox)

##4. Drop the offending variable
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + ethnicgroup)
summary(cox)

#Checking the proportionality hazard
cox.zph(fit, transform = "km", global = TRUE)
fit <- g[, "fit"]

##Plotting martingale residuals
fit <- coxph(Surv(fu_time, death) ~ gender) # fit the desired model
temp <- cox.zph(fit) # apply the cox.zph function to the desired model)
print(temp) # display the results                
plot(temp) # plot the curves

##Code and KM plot for gender
km_fit <- survfit(Surv(fu_time, death) ~ gender)
autoplot(km_fit)
plot(km_fit, xlab = "time", ylab = "Survival probability") #label the axes


##Using Deviance residuals
res.cox <- coxph(Surv(fu_time, death) ~ age)
ggcoxdiagnostics(res.cox, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())


res.cox <- coxph(Surv(fu_time, death) ~ age)
ggcoxdiagnostics(res.cox, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())

ggcoxfunctional(Surv(fu_time, death) ~ age + log(age) + sqrt(age))

fit <- coxph(Surv(fu_time, death) ~ copd) # fit the desired model
temp <- cox.zph(fit) # apply the cox.zph function to the desired model)
print(temp) # display the results                
plot(temp) # plot the curves

res.cox <- coxph(Surv(fu_time, death) ~ copd)
ggcoxdiagnostics(res.cox, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())


res.cox <- coxph(Surv(fu_time, death) ~ copd)
ggcoxdiagnostics(res.cox, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())

ggcoxfunctional(Surv(fu_time, death) ~ copd + log(copd) + sqrt(copd))

km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit)

##If the proportionality hazard is not met
fit <- coxph(Surv(fu_time, death) ~ gender + tt(gender)) #"tt" is the time-transform funtion
summary(fit)


##Model selection and Backwards elimination
ihd <- factor(g[, "ihd"])
valvular <- factor(g[, "valvular_disease"])
pvd <- factor(g[,"pvd"])
stroke <- factor(g[,"stroke"])
copd <- factor(g[, "copd"])
pneumonia <- factor(g[, "pneumonia"])
ht <- factor(g[, "hypertension"])
renal <- factor(g[, "renal_disease"])
ca <- factor(g[, "cancer"])
mets <- factor(g[, "metastatic_cancer"])
mental_health <- factor(g[, "mental_health"])
los <- g[,"los"]
prior_dna <- g[, "prior_dnas"]

#generate cognitive impairment variable (sentility and dementia combined)
cog_imp <- as.factor(ifelse(g$dementia == 1 | g$senile == 1, 1, 0))

#run the model
cox <- coxph(Surv(fu_time, death) ~ age + gender + ethnicgroup + ihd + valvular + pvd + stroke + copd + pneumonia + ht + renal + ca + mets + mental_health + cog_imp + los + prior_dna)
summary(cox)           

##Application of backwards elimination
cox <- coxph(Surv(fu_time, death) ~ age + gender + valvular + pneumonia + mets + cog_imp)
summary(cox)


table(cog_imp)
t <- table(cog_imp, death)
t
round(100*prop.table(t,1), digits = 1)

#Testing the proportionality assumption on the remaining variables
fit <- coxph(Surv(fu_time, death) ~ age + gender + valvular + pneumonia + mets + cog_imp) #test them all in the same model
temp <- cox.zph(fit)
print(temp)







gender <- factor(g[,"gender"])
fu_time <- g[,"fu_time"]
death <- g[, "death"]
age <- g[,"age"]
copd <- factor(g[,"copd"])
ethnicgroup <- factor(g[,"ethnicgroup"])
quintile <- factor(g[,"quintile"])
ihd <- factor(g[, "ihd"])
valvular <- factor(g[, "valvular_disease"])
pvd <- factor(g[,"pvd"])
stroke <- factor(g[,"stroke"])
copd <- factor(g[, "copd"])
pneumonia <- factor(g[, "pneumonia"])
ht <- factor(g[, "hypertension"])
renal <- factor(g[, "renal_disease"])
ca <- factor(g[, "cancer"])
mets <- factor(g[, "metastatic_cancer"])
mental_health <- factor(g[, "mental_health"])
los <- g[,"los"]
prior_dnas <- g[, "prior_dnas"]
cog_imp <- factor(g[, "senile"])

######### Plotting a kaplan-Meier curve

#Generate the survival curve
km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit) 

#alternative
autoplot(km_fit) + theme_bw() #theme_bw is a predesigned "theme" which makes the plot prettier

#Output the probability of survival at certain times after hospital admission
summary(km_fit, times = c(1:7,30,60,90*(1:10)))

#Plotting a Kaplan_Meier curve by gender

#1. Generate the survival curve
km_gender_fit <- survfit(Surv(fu_time, death) ~ gender)

#plot the curve
plot(km_gender_fit)
#or
autoplot(km_gender_fit + theme_bw() 

         

###Perform log rank test to see whether survival varies by gender
survdiff(Surv(fu_time, death) ~ gender, rho = 0)

##Testing whether those over the age of 65 have different survival to those under it

#1. DIchotomise age into categorical (binar in this case) variable
age_65plus <- ifelse(g[,"age"]>=65, 1, 0)

#2. Perform log rank test
survdiff(Surv(fu_time, death) ~ age_65plus, rho = 0)

###Plot survival curve by age above or below 65

#1. Generate survival curve
km_old_fit <- survfit(Surv(fu_time, death) ~ age_65plus)

#Plot
plot(km_old_fit)
#or
autoplot(km_old_fit) + theme_bw()|


##Run COx regression model with age as predictor (continuous variable)
  
#1. Generate model
cox <- coxph(Surv(fu_time, death) ~ age, data = g)

#2. Summarise model
summary(cox)

##Run cox regression model with quintile as predictor (categorical variable)
#changing the reference group to first quintile
#removing the zero quintile altogether

#1. Summarise the variable
table(quintile, exclude = NULL)

#2. Check levels
levels(quintile)

#3. Generate model
cox <- coxph(Surv(fu_time, death) ~ quintile) #warning

#4. Summarise model
summary(cox)

#5. Make the first quintilethe reference group
quintile <- relevel(quintile, ref = "1")

#6. Regenerate and summarise model
cox <- coxph(Surv(fu_time, death) ~ quintile) #warning

summary(cox)

#7. Inspecting quintile variable
table(quintile, g$death) #only 4 entries for quintile = 0 and 100% did'nt die

#8. Removing quintile = 0 entries as there are only 4 of them
quintile_5groups <- quintile
quintile_5groups[quintile_5groups ==0] <- NA #set the zeroes to missing
quintile_5groups <- factor(quintile_5groups) #this removes 0 as a level as it is an emoty category

#regenerating the model and summarizing
cox <- coxph(Surv(fu_time, death) ~ quintile_5groups)
summary(cox)

##Run cox regression model with ethnic group as predictor (categorical variable)
#including missing values as another category

#1. Summarise variable
table(ethnicgroup, exclude = NULL)

#2. Generate and summarise model
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

#3. Add another category (8)
levels(ethnicgroup) <- c(levels(ethnicgroup), "8")

#4. Redefine NA as another group 8
ethnicgroup[is.na(ethnicgroup)] <- "8"

#5. regenerate and summarise model
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)


####Investigating our variables in order to best perform a Cox model witj multiple predictors
##checking for missing values
##RUnning a multiple cox regression

#1. Summarising age
summary(g$age) #no NAS

hist(g$age)

#2. Gender
gender_table <- table(gender, exclude = NULL)
addmargins(gender_table) #no NAS

round(100*prop.table(gender_table), digits = 1) # percentages rounded to 1 decimal place

#3. Chronic obstructive pulmonary disease copd
copd_table <- table(copd, exclude = NULL)
addmargins(copd_table) #no NAS

round(100*prop.table(copd_table), digits = 1) # percentages rounded to 1 decimal place

#4. Prior OPd appointments missed
prior_dnas_table <- table(prior_dnas, exclude = NULL)
addmargins(prior_dnas_table) #no NAS

round(100*prop.table(prior_dnas_table), digits = 1) # percentages rounded to 1 decimal place

#5. Ethnic group
ethnicgroup_table <- table(ehtnicgroup, exclude = NULL)
addmargins(ethnicgroup_table) #no NAS

round(100*prop.table(ethnicgroup_table), digits = 1) # percentages rounded to 1 decimal place

#6. Generate and summarise model
cox <- coxph(Surv(fu_time, death) ~ age +gender + copd + prior_dnas + ethnicgroup)
summary(cox)


###Investigating whether the assumptions of the Cox model are being broken
#Testing for proportional hazards assumption (with gender as predictor variable)

#1. Generate model fit
fit <- coxph(Surv(fu_time, death) ~ gender)

#2. Apply the test to the model
temp <- cox.zph(fit)

#3. Display results
print(temp)

#4.Plot the curves
plot(temp)

#or
ggcoxzph(temp)

##Generating other diagnostic plots for cox proportional hazards model
#1. Define model
res.cox <- coxph(Surv(fu_time, death) ~ age)

##Generate diagnostic plots
# 2. Plotting the estimated changes in the regression coefficients on deleting each patient
ggcoxdiagnostics(res.cox, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())

#3. Plotting deviance residuals
ggcoxdiagnostics(res.cox, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())

#4. Plotting Martingale residuals
fit <- coxph(Surv(fu_time, death) ~ age + log(age) + sqrt(age))
ggcoxfunctional(fit, data = g) #note we must specify ordinal dataframe


###Testing proportionality assumption
##Testing for a statistical relationship between gender and time

#1. Generate model with time-transform function (tt)
fit <- coxph(Surv(fu_time, death) ~ gender + tt (gender))

#2. Summarise
summary(fit)


##Backwards elimination to choose predictors for Cox regression
#1.Run the full model with all of your predictors
cox <- coxph(Surv(fu_time, death) ~ age + gender + ethnicgroup + ihd + valvular + pvd + stroke + copd + pneumonia + ht + renal + ca + mets + mental_health + cog_imp + los + prior_dna)
summary(cox)

# 2. Run the model with only significant predictors
cox <- coxph(Surv(fu_time, death) ~ age + gender + valvular + pneumonia + mets + cog_imp)
summary(cox)

#3. Test proportionality assumption on these predictors
cox.zph(cox)


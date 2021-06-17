g <- read.csv(file = "final-diabetes-data-for-R-csv.csv", header = TRUE, sep = ',')
dim(g)
dimnames(g)[[2]]
chol <- g["chol"] #cholesterol is continuous , so it's easy
gender <- as.factor(g["gender"]) #but gender isn't
dm <- as.factor(g[,"dm"]) #neither is dm
t <- table(gender) #store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total and print the results
round(prop.table(t),digits = 3) #get proportions rounded to 3dp
gender

round(100*prop.table(t),digits = 1) # get %s rounded to 1dp
c("female", "male")

dm2 <- factor(dm, exclude = NULL) #make new factor from the old one
table(dm2) #display the counts including the missings (NAs)

summary(chol)

height <- g['height']
weight <- g['weight']
summary(height)
summary(weight)
height.si <- height *0.0254
weight.si <- weight*0.453592
bmi <- weight.si/height.si^2
summary(bmi)

bmi_categorised <- ifelse(bmi < 18.5, "underweight",
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal",
                                 ifelse(bmi > 25 & bmi <= 30, "overwight",
                                        ifelse(bmi > 30, "obese", NA))))

# check that the bmi_categorised variable has worked
table(bmi_categorised, exclude = NULL)

##bmi_categorised
bmi_categorised
normal       obese   overwight underweight        <NA> 
  113         152         123           9           6 

####Using cross tabulation
#frequesncies of diabetes by BMI category
dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL
                          
dm_by_bmi_category         ##check

# with the row percentages
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1)

round(100 * prop.table(dm_by_bmi_category, margin = 2), digits = 1)



#### My solutionsss
age <- g['age']
summary(age)
age_categorised <- ifelse(age < 45, "youngest",
                          ifelse(age >= 45 & age <= 64, "normal",
                                 ifelse(age >= 65 & age <= 74, "older",
                                        ifelse(age > 75, "oldest", NA))))
table(age_categorised, exclude = NULL)
age_categorised
gender_by_age_category <- table(age_categorised, gender, exclude = NULL
gender_by_age_category                               

gender_by_age_category <- table(age_categorised, gender(c("female", "male")), exclude = NULL
gender_by_age_category   

gender_by_age_category <- table(age_categorised, female, exclude = NULL
gender_by_age_category
          

####Correct solutions

age <- g[,"age"] #creating "age variable"

#creating a categorical variable "age_grouped"
age_grouped <- ifelse(age < 45, "under 45",
                      ifelse(age >= 45 & age < 65, "45 -64",
                             ifelse(age >= 65 & age < 75, "65 - 74",
                                    ifelse(age >= 75, "75 or over", NA))))

#displaying new variable in a table
table(age_grouped, exclude = NULL)

# cross tabulating with gender
age_group_by_gender <- table(age_grouped, gender, exclude = NULL)

#display the cross tabulation
age_group_by_gender

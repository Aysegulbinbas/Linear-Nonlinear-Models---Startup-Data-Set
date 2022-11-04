startup<- read.csv("C:/Users/90551/Desktop/SONDÖNEM/stat364/Stat364-Proje/startup.csv", header = T, sep = ",")
head(startup)
str(startup)
summary(startup)

startup$Dependent <- as.factor(startup$Dependent)
startup$Founders_Popularity <- as.factor(startup$Founders_Popularity)
startup$Company_Location <- as.factor(startup$Company_Location)
startup$Company_Founder_Patent <- as.factor(startup$Company_Founder_Patent)
startup$Company_difficulty_obtaining_workforce <- as.factor(startup$Company_difficulty_obtaining_workforce)
startup$Founder_highest_degree_type <- as.factor(startup$Founder_highest_degree_type)
startup$Company_subscription_offering <- as.factor(startup$Company_subscription_offering)
startup$Company_big_data <- as.factor(startup$Company_big_data)
startup$Company_Product_or_service <- as.factor(startup$Company_Product_or_service)
startup$Company_crowdfunding <- as.factor(startup$Company_crowdfunding)
startup$Company_crowdsourcing <- as.factor(startup$Company_crowdsourcing)
startup$Company_incubation_investor <- as.factor(startup$Company_incubation_investor)
startup$Founders_publications <- as.factor(startup$Founders_publications)
startup$Founders_profile_similarity <- as.factor(startup$Founders_profile_similarity)
startup$Founder_education <- as.factor(startup$Founder_education)
startup$Founders_Industry_exposure <- as.factor(startup$Founders_Industry_exposure)
startup$Founders_global_exposure <- as.factor(startup$Founders_global_exposure)
startup$Founders_experience <- as.factor(startup$Founders_experience)
startup$Company_business_model <- as.factor(startup$Company_business_model)
startup$Founders_big_5_experience <- as.factor(startup$Founders_big_5_experience)
startup$Founders_startup_experience <- as.factor(startup$Founders_startup_experience)
startup$Founders_previous_company_employee_count <- as.factor(startup$Founders_previous_company_employee_count)
startup$Founders_top_company_experience <- as.factor(startup$Founders_top_company_experience)
startup$Company_top_Angel_VC_funding <- as.factor(startup$Company_top_Angel_VC_funding)
startup$Company_mobile_app <- as.factor(startup$Company_mobile_app)
startup$Company_Industry_count <- as.factor(startup$Company_Industry_count)
startup$Company_raising_fund <- as.factor(startup$Company_raising_fund)
startup$Founder_university_quality <- as.factor(startup$Founder_university_quality)
startup$Company_analytics_score <- as.factor(startup$Company_analytics_score)

str(startup)
summary(startup)

sum(is.na(startup))
sum(is.null(startup))




s.data=startup[,-1]

# Which variables have relation with the success of the company?




set.seed(123) # setting seed to generate a reproducible random sampling
# creating training data as 80% of the dataset
random_sample <- createDataPartition(s.data$Dependent, p = 0.8, list = FALSE)
train2  <- s.data[random_sample, ] # generating training dataset from the random_sample
test2<- s.data[-random_sample, ] # generating testing dataset from rows which are not included in random_sample

#Founders_Entrepreneurship_skills_score  and    Founders_Business_Strategy_skills_score ,corr  0.51398467

prop.table(table(train2$Dependent))
prop.table(table(test2$Dependent))

#the proportion of the levels of the response are close to each other. Now, let us fit the model with Logistic regression

summary(train2)
cor(train2[,c(6:10,12,24,27:37,39,40,41)])

library(olsrr)


startupmodel<- glm(Dependent ~ ., data=train2, family="binomial")
summary(startupmodel)

#then by using significant variable new model was createed.
sm<- glm(Dependent ~ Company_Location+Company_investor_count_seed +Company_investor_count_Angel_VC+
           Company_senior_team_count+Founders_startup_experience+Founders_big_5_experience+Company_business_model+
           Founder_education+Founder_university_quality+Founders_Popularity+Founders_profile_similarity+
           Founders_publications+Founders_skills_score+Founders_Entrepreneurship_skills_score+
           Founders_Data_Science_skills_score + Founders_Product_Management_skills_score +Company_incubation_investor+
           Company_competitor_count +Company_crowdsourcing +Company_analytics_score +
           Company_subscription_offering + Founder_highest_degree_type + Company_difficulty_obtaining_workforce
         
         
         
         , data=train2, family="binomial")
summary(sm)

# Then, by using stepwise method, best model found



library(MASS)
step.model <- sm %>% stepAIC(trace = FALSE)
coef(step.model)



l.model = glm(Dependent ~ +Company_investor_count_seed +
                Company_senior_team_count+Company_business_model+
                Founder_education+Founder_university_quality+Founders_Popularity+Founders_profile_similarity+
                Founders_publications+Founders_skills_score+Founders_Entrepreneurship_skills_score+
                Founders_Data_Science_skills_score + Founders_Product_Management_skills_score +
                Company_competitor_count +Company_analytics_score +
                Company_subscription_offering + Company_difficulty_obtaining_workforce
              
              
              
              , data=train2, family="binomial")
summary(l.model)

#to interpret odd ratio

exp(l.model$coefficients)

# Odds ratio and 95% CI
exp(cbind(OR = coef(l.model), confint(l.model)))
#Interp. odds ratios:


# # for every one unit increase in age, the odds of having regular blood pressure (vs non regular) increases by a factor of 1.058
# # changes in gender from ... to ..., the odds of having regular blood pressure (vs non regular) increases by a factor of 1.058
library(car)

vif(l.model)  # all vif are less than 5 ,there is no multi collinearity.

library(cutpointr)

#for prediction step :
library("AER")

pred.prob3 <- l.model %>% predict(test2, type = "response")
head(pred.prob3)

# this is the predicted score. these scores are between 0 and 1
contrasts(test2$Dependent)


#tekrar bak optimalCutoff fnc library :
optCutOff3 <- optimalCutoff(test2$Dependent, pred.prob3)[1]   #  0.789
pred.test3 = ifelse(pred.prob3 > optCutOff3, 1, 0)
head(pred.test3)
library(pROC)
library(ResourceSelection)
test_tab3 = table(predicted = pred.test3,actual = test2$Dependent)
library(confusionMatrix)
k=confusionMatrix(test_tab3)


##################################3333
#Is there any significant difference between the means of company's 1st investment time by company location?
anova.data=s.data[,c(40,2)]
summary(anova.data)
library(ggplot2)
qplot(as.factor(anova.data$Company_Location),anova.data$Company_1st_investment_time,geom = "boxplot")
+labs(title="Box plot of time w.r.t location",x="program", y = "time")

summary(s.data)
#the means of investmen time so close to each other .

anova <- aov(Company_1st_investment_time ~ Company_Location, data = anova.data)
summary(anova)

#According to ANOVA test, we cannot reject the null hypothesis which is H0:µ1=µ2=µ3=0 because the p value is greater than 0.05.
# It indicates that  group means are equal to each other.
bcPower(s.data$Company_1st_investment_time)

anova2 <- aov((Company_1st_investment_time) ~ Company_Location, data = anova.data)
str(anova.data)

#Assumptions of ANOVA

#Errors are normally distributed
summary(anova.data)
nor.test(Company_1st_investment_time ~ Company_Location, data = anova.data)
#We can apply Box-cox transformation in order to satisfy the normality assumption
library(MASS)
nor.test(log(anova.data$Company_1st_investment_time) ~ Company_Location, data = anova.data)


shapiro.test(residuals(anova))


#The shapiro wilk test shows that the residuals are not normally distributed,after transformation

library(AID)
library(onewaytests)
plot(anova,2)
library(car)
# boxcoxfr method was used to get normal data, but normality assumptions did not satisfied.

#out <- boxcoxfr(anova.data$Company_1st_investment_time, anova.data$Company_Location)

bartlett.test(Company_1st_investment_time ~ Company_Location, anova.data)
#p-value greater than the alpa,so heterogenty of variance satisfied.

#Errors are independent from one another


# then :

kruskal.test(Company_1st_investment_time ~ Company_Location, data = anova.data)

pairwise.wilcox.test(anova.data$Company_1st_investment_time, anova.data$Company_Location,
                     p.adjust.method = "BH")




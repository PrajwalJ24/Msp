setwd("D:/PGDM/2nd Year/Term 4/DAR/RCode")
getwd()

library(readxl)
install.packages("ResourceSelection")
library(ResourceSelection)


Mortagage=read_excel(file.choose(), sheet ="Data Coded" )
attach(Mortagage)
dec_mort=factor(Mortagage$Decision, levels=c(1,0))
dec_mort
Mort_logit=glm(dec_mort~Selfcon_Code+Tier_1+Tier_2
               +AccoClass+LoanType+Sex+Etype+Doc_pf
               +Marstat+Emp_Type_1+Emp_Type_2
               +Emp_Type_3+Educlass_1+Educlass_2
               +Oriclass_1+Oriclass_2+Age+YrsAdd+YrsJob
               +MarVal+Term, data=Mortagage, 
               family = binomial("logit"))
summary(Mort_logit)
coef(Mort_logit)
exp(coef(Mort_logit))
fitted(Mort_logit)
hoslem.test(Mortagage$Decision,fitted(Mort_logit))
install.packages("glmtoolbox")
library(glmtoolbox)
hltest(Mort_logit)


install.packages("InformationValue")
library(InformationValue)
install.packages("ISLR")
library(ISLR)
install.packages("caret")
library(caret)
pred=predict(Mort_logit, Mortagage, type="response")
Mortagage$default <- ifelse(Mortagage$Decision==0, 1, 0)
optimal <- optimalCutoff(Mortagage$default, pred)[1]
optimal
confusionMatrix(Mortagage$default,pred)
sensitivity(Mortagage$default, pred)*100
specificity(Mortagage$default, pred)*100
misClassError(Mortagage$default, pred, threshold=optimal)*100
detach(Mortagage)
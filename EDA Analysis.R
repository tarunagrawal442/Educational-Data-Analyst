# This is the analysis for online course.

library(tidyverse)
install.packages("fastDummies")
library("fastDummies")
data<-read_csv(file.choose())
summary(data)
str(data)

data1<-data[,c(5,12,19,20,21,22,23,24)]
data1$useSRLUI<-as.factor(data1$useSRLUI)
data1$drop_out<-as.factor(data1$drop_out)
# Basic checks on the data
which(is.na(data1))
which(data1$degree_all>data1$strength_all)
summary(data1)
str(data1)
#Degree all - representing how many people a learner reached out on the 
#discussion forum 

#Strength all - representing how many times 
# a learner reached out on the discussion forum 
# Experiment = Received the treatmeent
# Control = Did not receive the treatment.

#Column S - treatment means if students are being offered the treatment 
#SRLUI (Intent to treat)

#Column T- means if people in the treatment group actually interact with the 
#treatment ( Treatment on the Treated)

#Column U, W and X represents the learning outcomes: grades, dropout and 
#survival days

data2<- data1 %>% filter(useSRLUI=="0")
data3<- data1 %>% filter(useSRLUI=="1")
ggplot(data1) + geom_boxplot(mapping = aes(x=Grade)) + facet_wrap("useSRLUI")
quantile(data2$Grade, probs = seq(0,1,0.25))
quantile(data3$Grade, probs = seq(0,1,0.25))
attach(data1)
mean(data2$Grade)
mean(data3$Grade)
ggplot(data1) + geom_bar(mapping = aes(x=drop_out, fill=useSRLUI))
#quantile(data2$drop_out, probs = seq(0,1,0.25))
#quantile(data3$drop_out, probs = seq(0,1,0.25))
attach(data1)

ggplot(data1) + geom_boxplot(mapping = aes(x=survival_days)) + facet_wrap("useSRLUI")
quantile(data2$survival_days, probs = seq(0,1,0.10))
quantile(data3$survival_days, probs = seq(0,1,0.10))
attach(data1)

cor(data1$Grade,data1$survival_days)

# Logistic Regression for the drop out rate
data1<-dummy_cols(data1,select_columns = "treatment")
logit1<-glm(drop_out~ Grade+survival_days, data = data1[,c(1,2,5,7,8,9,10)], family = "binomial")
summary(logit1)
predicted<-predict(logit1, type="response")
plot(predicted,drop_out)

# Linear Model for the Grades
reg<-lm(Grade~., data = data1[,c(2,4,5)])
summary(reg)
cor(degree_all,strength_all) # Take care of this correlation

# Multicollinearity - Ridge Regression- Grade
library(glmnet)
cvridge1 <- cv.glmnet(x = as.matrix(data1[,c(1,2,4,9)]), y = unlist(data1[,5]), alpha = 0)
?cv.glmnet
summary(cvridge1)

# Linear Model for the Survival Days
reg<-lm(survival_days~., data = train[,c(2,4,5,8)])
summary(reg)

# Multicollinearity - Ridge Regression- Survival Days
library(glmnet)
cvridge1 <- cv.glmnet(x = as.matrix(data1[,c(1,2,4,9)]), y = unlist(data1[,5]), alpha = 0)
?cv.glmnet
summary(cvridge1)

#This study implemented and evaluated the effectiveness of an alternative 
#intervention, the self-regulated learning user interface (SRLUI), 
#to support students' self-regulated learning (SRL) strategies in a 
#MOOC environment. SRLUI is based on Zimmerman’s (2000) SRL model 
#and develops learner’s SRL skills through longitudinal, 
#recurring practice of multiple SRL dimensions activities 
#(i.e., goal setting, self-evaluation, task planning, setting reminders) 
#with content-specific information.

# Impact on SRLUI on the drop out rate:
data6<- data1 %>% select(c("drop_out","useSRLUI")) %>% filter(useSRLUI==0 & drop_out==1)
data6<- data1 %>% select(c("drop_out","useSRLUI")) %>% filter(useSRLUI==0 & drop_out==1)
data6<- data1 %>% select(c("drop_out","useSRLUI")) %>% filter(drop_out==1)


set.seed(617)
split = sample(1:nrow(data1), nrow(data1)*0.80)
train = data1[split,]
test = data1[-split,]

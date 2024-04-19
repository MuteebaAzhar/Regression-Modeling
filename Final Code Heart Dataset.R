#  Setting Work Directory
setwd("C:/Users/hafiz/Downloads/EDA")

#  Loading of the Data set
heart <- read.csv("heart.csv" , header = T)

#  Calling libraries
library(ggplot2)
library(dbplyr)


#  Data and its Structure Visualization 
heart
str(heart)

 #  Checking for NA entries in Data set
is.na(heart)

#  Conversion of categorical variables to factors
heart$sex <- as.factor(heart$sex)
heart$cp <- as.factor(heart$cp)
heart$fbs <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exang <- as.factor(heart$exang)
heart$slope <- as.factor(heart$slope)
heart$ca <- as.factor(heart$ca)
heart$thal <- as.factor(heart$thal)
heart$target <- as.factor(heart$target)

#  Structure Review
str(heart)
ggplot(heart)

#  Checking the number of Heart Disease Individuals in the dataset 
table(heart$target)
## This explains that there are 499 individuals with no heart disease and 526 with heart diseases in our dataset
### Plot a graph for these Results
ggplot(heart, aes(x=target)) + 
  ggtitle("Heart Disease Status of Dataset")+
  geom_bar(col="red" , fill = "blue") +
  xlab("Heart Diseases: 0 (No Heart Disease) , 1 (Heart Diesease)") +
  ylab("Number of Indiciduals") +
  geom_text(stat="count" , aes(label=..count..) , col="white" , size=5 , position = position_stack(0.5))



########################################################################################
## 1                          Statistical Visualization of Each                        #
#                                     and Every Variable                               #
########################################################################################


########################################################################################
## 1.1                            Age Distribution Statistics                          #
########################################################################################

#    1.1.1 Basic Structure
str(heart$age)
summary(heart$age)
sd(heart$age)

#    1.1.2 Histogram Plot
ggplot(heart , aes(x=age)) +
  ggtitle("Age Distribution") +
  geom_histogram(col="black" , fill="red" , binwidth = 1) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.1.3 Relationship b/w Age and Heart Diseases using histogram Plot
ggplot(heart , aes(x=age , fill=target)) +
  ggtitle("Relationship b/w Age and Heart Diseases") +
  geom_histogram(col="black", binwidth = 1) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.1.4 Sub-setting on the basis of Plot Analysis (above 40 years)
age40 <- heart %>% dplyr::filter(age > 40)
age40

#      1.1.4.1 Diseased Individuals
age40_d <- heart %>% dplyr::filter(age > 40 , target==1)
age40_d

#        1.1.4.1.1 Bar Plot
ggplot(age40_d , aes(x=age , fill=target)) +
  ggtitle("Individuals with Heart Diseases above 40") +
  geom_bar(col="white" , fill="blue") +
  xlab("Age of Individuals") + ylab("Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=4 , position = position_stack(0.5))

#      1.1.4.2 Normal Individuals
age40_n <- heart %>% dplyr::filter(heart$age>40 & target==0)
age40_n

#        1.1.4.2.1 Bar Plot
ggplot(age40_n , aes(x=age , fill=target)) +
  ggtitle("Normal Individuals above 40") +
  geom_bar(col="white" , fill="red") +
  xlab("Age of Individuals") + ylab("Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=4 , position = position_stack(0.5))




########################################################################################
## 1.2             Statistical Analysis of Sex Distribution in Data set                #
########################################################################################

#    1.2.1 Basic Structure
str(heart$sex)
table(heart$sex)

#    1.2.2 Bar Plot
ggplot(heart , aes(x=sex)) +
  ggtitle("Sex Distribution") +
  geom_bar(col="black" , fill="cyan3") +
  xlab("Sex: 0(Female), 1(Male)") + ylab("Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="black" , size=4 , position = position_stack(0.5))

#    1.2.3 Relationship b/w Sex and Heart Diseases
ggplot(heart , aes(x=sex , fill=target)) +
  ggtitle("Relationship b/w Sex and Heart Diseases") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Sex: 0(Female), 1(Male)") + ylab("Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=4 , position = position_stack(0.5))

#    1.2.3 Relationship b/w Age and Heart Diseases w.r.t Sex
ggplot(heart , aes(x=age , fill=target)) +
  ggtitle("Relationship b/w Age and Heart Diseases w.r.t Sex") +
  geom_histogram(col="white" , bins = 30) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Age of Individuals") + ylab("Number of Individuals") + labs(fill="Heart Diseases") +
  facet_wrap(~sex)




########################################################################################
## 1.3                   Basic Statistical Analysis of Chest Pains                     #
########################################################################################

#    1.3.1 Basic Structure
str(heart$cp)
table(heart$cp)

#    1.2.2 Bar Plot
ggplot(heart , aes(x=cp)) +
  ggtitle("Chest Pains Distribution") +
  geom_bar(col="black" , fill="green") +
  xlab("Chest Pains: 0(Typical Angina), 1(Atypical Angina), 2(Non-Anginal pain), 3(Asymptomatic)") +
  ylab("Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="black" , size=4 , position = position_stack(0.5))

#    1.3.2 Relationship b/w Chest Pains and Heart Diseases using Bar plot
ggplot(heart , aes(x=cp , fill=target)) +
  ggtitle("Relationship b/w Chest Pains and Heart Diseases") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Chest Pains: 0(Typical Angina), 1(Atypical Angina), 2(Non-Anginal pain), 3(Asymptomatic)") + 
  ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=4 , position = position_stack(0.5))

#    1.3.3 Relationship b/w Age and Chest Pains Types w.r.t Heart Diseases
ggplot(heart , aes(x=age , fill=cp)) +
  ggtitle("Relationship b/w Age, Chest Pains and Heart Diseases") +
  geom_histogram(col="black", bins=30) +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Chest Pains Types") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.3.4 Sub-setting the Chest Pains and Heart Diseases from the dataset on basis of Age
#      1.3.4.1 For Chest Pains Type 0
agecp0_40 <- heart %>% dplyr::filter(age> 40, cp==0)
agecp0_40
table(agecp0_40$target)

#        1.3.4.1.1 Bar Plot
ggplot(agecp0_40 , aes(x=age , fill=cp)) +
  ggtitle("Relationship b/w Age and Chest Pains Type 0") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Chest Pains Types") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#      1.3.4.2 For Chest Pains Type 1
agecp1_40 <- heart %>% dplyr::filter(age> 40, cp==1)
agecp1_40
table(agecp1_40$target)

#        1.3.4.2.1 Bar Plot
ggplot(agecp1_40 , aes(x=age , fill=cp)) +
  ggtitle("Relationship b/w Age and Chest Pains Type 1") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Chest Pains Types") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#      1.3.4.3 For Chest Pains Type 2
agecp2_40 <- heart %>% dplyr::filter(age> 40, cp==2)
agecp2_40
table(agecp2_40$target)

#        1.3.4.3.1 Bar Plot
ggplot(agecp2_40 , aes(x=age , fill=cp)) +
  ggtitle("Relationship b/w Age and Chest Pains Type 2") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Chest Pains Types") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#      1.3.4.4 For Chest Pains Type 3
agecp3_40 <- heart %>% dplyr::filter(age> 40, cp==3)
agecp3_40
table(agecp3_40$target)

#        1.3.4.4.1 Bar Plot
ggplot(agecp3_40 , aes(x=age , fill=cp)) +
  ggtitle("Relationship b/w Age and Chest Pains Type 3") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Chest Pains Types") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

##### These Subsets explain the proficiency of heart diseases in individuals with Chest Pains type 2. So, let's
# have some sub-setting of this data on the basis of Diseased and Normal individuals.

## Diseased Individuals
agecp2_40d <- agecp2_40 %>% dplyr::filter(target==1)
agecp2_40d

## Normal Individuals
agecp2_40n <- agecp2_40 %>% dplyr::filter(target==0)
agecp2_40n

#    1.3.5 Relationship b/w Chest Pains and Sex w.r.t Heart Diseases 
ggplot(heart , aes(x=cp , fill=sex)) +
  ggtitle("Relationship b/w Chest Pains and Sex w.r.t Heart Diseases") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Chest Pains Types") + ylab("Total Number of Individuals") + labs(fill="Sex") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)




########################################################################################
## 1.4            Statistical Analysis of Resting Blood Pressure                       #
########################################################################################

#    1.4.1 Basic Structure
str(heart$trestbps)
summary(heart$trestbps)
sd(heart$trestbps)

#    1.4.2 Histogram Plot
ggplot(heart , aes(x=trestbps)) +
  ggtitle("Resting Blood Pressure Distribution") +
  geom_histogram(col="black" , fill="yellow" , binwidth = 2) +
  xlab("Resting Blood Pressure") + ylab("Total Number of Individuals") +
  geom_text(stat = "count" , aes(label=..count..) , color="red" , size=3 , position = position_stack(0.5))

#    1.4.3 Relationship b/w Resting Blood Pressure and Heart Diseases
ggplot(heart , aes(x=trestbps , fill=target)) +
  ggtitle("Relationship b/w Resting Blood Pressure and Heart Diseases") +
  geom_histogram(col="white", bins = 30) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Resting Blood Pressure") + ylab("Total Number of Individuals") + 
  labs(fill="Heart Diseases")

#    1.4.4 Sub-setting on the basis of Blood Pressure above and below normal~120
#      1.4.4.1 High Blood Pressure
rbp120h <- heart %>% dplyr::filter(trestbps > 120)
rbp120h
table(rbp120h$target)

#        1.4.4.1.1 Histogram Plot
ggplot(rbp120h , aes(x=trestbps , fill=target)) +
  ggtitle("Relationship b/w High Blood Pressure and Heart Diseases") +
  geom_histogram(col="black", binwidth = 4) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Resting Blood Pressure") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases")

#      1.4.4.2 Low Blood Pressure
rbp120l <- heart %>% dplyr::filter(trestbps < 120)
rbp120l
table(rbp120l$target)

#        1.4.4.2.1 Histogram Plot
ggplot(rbp120l , aes(x=trestbps , fill=target)) +
  ggtitle("Relationship b/w High Blood Pressure and Heart Diseases") +
  geom_histogram(col="black", binwidth = 4) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Resting Blood Pressure") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases")

#    1.4.5 Relationship b/w Age and Chest Pains w.r.t Heart Diseases
ggplot(heart , aes(x=age , fill=cp)) +
  ggtitle("Relationship b/w Resting Blood Pressure and Chest Pains") +
  geom_histogram(col="black" , bins = 30) +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Chest Pains Types") +
  facet_wrap(~target)

#    1.4.5 Relationship b/w Sex and Chest Pains w.r.t Heart Diseases
ggplot(heart , aes(x=sex , fill=cp)) +
  ggtitle("Relationship b/w Sex and Chest Pains w.r.t Heart Diseases") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Sex: 0(Female), 1(Male)") + ylab("Total Number of Individuals") + labs(fill="Chest Pains Types") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.4.6 Relationship b/w Resting Blood Pressure and Chest Pains w.r.t Heart Diseases
ggplot(heart , aes(x=trestbps , fill=cp)) +
  ggtitle("Relationship b/w Resting Blood Pressure and Chest Pains") +
  geom_histogram(col="black" , bins = 30) +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Resting Blood Pressure") + ylab("Total Number of Individuals") + labs(fill="Chest Pains Types") +
  facet_wrap(~target)

#      1.4.7.1 Sub-setting on the basis of Chest Pains
#        1.4.7.1.1 For Chest Pains Type 0
#                  High Blood Pressure
rbp120h_cp0 <- heart %>% dplyr::filter(trestbps > 120 , cp==0)
rbp120h_cp0
table(rbp120h_cp0$target)
#                  Low Blood Pressure
rbp120l_cp0 <- heart %>% dplyr::filter(trestbps < 120 , cp==0)
rbp120l_cp0
table(rbp120l_cp0$target)

#        1.4.7.1.2 For Chest Pains Type 1
#                  High Blood Pressure
rbp120h_cp1 <- heart %>% dplyr::filter(trestbps > 120 , cp==1)
rbp120h_cp1
table(rbp120h_cp1$target)
#                  Low Blood Pressure
rbp120l_cp1 <- heart %>% dplyr::filter(trestbps < 120 , cp==1)
rbp120l_cp1
table(rbp120l_cp1$target)

#        1.4.7.1.3 For Chest Pains Type 2
#                  High Blood Pressure
rbp120h_cp2 <- heart %>% dplyr::filter(trestbps > 120 , cp==2)
rbp120h_cp2
table(rbp120h_cp2$target)
#                  Low Blood Pressure
rbp120l_cp2 <- heart %>% dplyr::filter(trestbps < 120 , cp==2)
rbp120l_cp2
table(rbp120l_cp2$target)

#        1.4.7.1.4 For Chest Pains Type 3
#                  High Blood Pressure
rbp120h_cp3 <- heart %>% dplyr::filter(trestbps > 120 , cp==0)
rbp120h_cp3
table(rbp120h_cp3$target)
#                  Low Blood Pressure
rbp120l_cp3 <- heart %>% dplyr::filter(trestbps < 120 , cp==3)
rbp120l_cp3
table(rbp120l_cp3$target)

###The Chest Pains Type 2 serves as the cause of Maximum heart diseases.
#Sub-setting on the Basis of Chest Pains type 2
rbp120_cp2 <- heart %>% dplyr::filter(trestbps > 120 , cp==2)
rbp120_cp2
table(rbp120_cp2$target)

ggplot(rbp120_cp2 , aes(x=trestbps , fill=target)) +
  ggtitle("Relationship b/w Resting Blood Pressure and Non-Anginal Chest Pains") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Resting Blood Pressure") + ylab("Total Number of Individuals") + labs("Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

###  High Blood Pressure
####	Diseased Individuals
rbp120h_dcp2 <- heart %>% dplyr::filter(trestbps > 120 , cp==2 , target==1)
rbp120h_dcp2
table(rbp120h_dcp2$target)

####	Normal Individuals
rbp120h_ncp2 <- heart %>% dplyr::filter(trestbps > 120 , cp==2 ,  target==0)
rbp120h_ncp2
table(rbp120h_ncp2$target)

###  Low Blood Pressure
####	Diseased Individuals
rbp120l_dcp2 <- heart %>% dplyr::filter(trestbps < 120 , cp==2 ,  target==1)
rbp120l_dcp2
table(rbp120l_dcp2$target)

####	Normal Individuals
rbp120l_ncp2 <- heart %>% dplyr::filter(trestbps < 120 , cp==2 ,  target==0)
rbp120l_ncp2
table(rbp120l_ncp2$target)




########################################################################################
## 1.5               Statistical Analysis of Serum Cholesterol Level                   #
########################################################################################

#    1.5.1 Basic Structure
str(heart$chol)
summary(heart$chol)
sd(heart$chol)

#    1.5.2 Histogram Plot
ggplot(heart , aes(x=chol)) +
  ggtitle("Serum Cholesterol Level Distribution") +
  geom_histogram(col="black" , fill="cyan" , binwidth = 20) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals")

#    1.5.3 Relationship b/w Serum Cholesterol Level and Heart Diseases
ggplot(heart , aes(x=chol , fill=target)) +
  ggtitle("Relationship b/w Serum Cholesterol Level and Heart Diseases") +
  geom_histogram(col="white", bins = 30) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases")

#    1.5.4 Sub-setting for the Number of Individuals on the basis of normal serum level~190-200
#      1.5.4.1 Above 190
chol190h <- heart %>% dplyr::filter(chol > 190)
chol190h
table(chol190h$target)

#        1.5.4.1.1 Histogram Plot
ggplot(chol190h , aes(x=chol , fill=target)) +
  ggtitle("Relationship b/w High Serum Cholesterol Level and Heart Diseases") +
  geom_histogram(col="black", bins= 30) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases")

#        1.5.4.1.2 Diseased Individuals
chol190h_d <- heart %>% dplyr::filter(chol > 190 , target == 1)
chol190h_d
table(chol190h_d$target)

#        1.5.4.1.3 Normal Individuals
chol190h_n <- heart %>% dplyr::filter(chol > 190 , target == 0)
chol190h_n
table(chol190h_n$target)

#      1.5.4.2 Below 190
chol190l <- heart %>% dplyr::filter(chol < 190)
chol190l
table(chol190l$target)

#        1.5.4.2.1 Histogram Plot
ggplot(chol190l , aes(x=chol , fill=target)) +
  ggtitle("Relationship b/w High Serum Cholesterol Level and Heart Diseases") +
  geom_histogram(col="black", bins = 30) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases")

#        1.5.4.2.2 Diseased Individuals
chol190l_d <- heart %>% dplyr::filter(chol < 190 , target == 1)
chol190l_d
table(chol190l_d$target)

#        1.5.4.2.3 Normal Individuals
chol190l_n <- heart %>% dplyr::filter(chol < 190 , target == 0)
chol190l_n
table(chol190l_n$target)

#    1.5.5 Relationship b/w Age and Serum Cholesterol Level w.r.t Heart Diseases
ggplot(heart , aes(x=age , y=chol)) +
  ggtitle("Relationship b/w Age and Serum Cholesterol Level w.r.t Heart Diseases") +
  geom_point(col="red") +
  xlab("Age of Individuals") + ylab("Serum Cholesterol Level") +
  facet_wrap(~target)

#    1.5.6 Relationship b/w Serum Cholesterol Level and Heart Diseases w.r.t Sex
ggplot(heart , aes(x=chol , fill=target)) +
  ggtitle("Relationship b/w Serum Cholesterol Level and Heart Diseases w.r.t Sex") +
  geom_histogram(col="black" , bins = 30) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  facet_wrap(~sex)

#    1.5.7 Relationship b/w Serum Cholesterol Level and Chest Pains w.r.t Heart Diseases
ggplot(heart , aes(x=chol , fill=cp)) +
  ggtitle("Relationship b/w Serum Cholesterol Level and Chest Pains w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins = 30) +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Chest Pains") +
  facet_wrap(~target)

#    1.5.8 Relationship b/w High Serum Cholesterol Level and Chest Pains Type 2
chol190h_cp2 <- heart %>% dplyr::filter(chol > 190 , cp==2)
chol190h_cp2
table(chol190h_cp2$target)
ggplot(chol190h_cp2 , aes(x=chol , fill=target)) +
  ggtitle("Relationship b/w Serum Cholesterol Level and Chest Pains Type 2") +
  geom_histogram(col="black" , binwidth = 30) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="black" , size=2 , position = position_stack(2))

####	Diseased Individuals
chol190h_dcp2 <- heart %>% dplyr::filter(chol > 190 , cp==2 , target==1)
chol190h_dcp2
table(chol190h_dcp2$target)

####	Normal Individuals
chol190h_ncp2 <- heart %>% dplyr::filter(chol > 190 , cp==2 , target==0)
chol190h_ncp2
table(chol190h_ncp2$target)

#    1.5.9 Relationship b/w Resting Blood Pressure and Serum Cholesterol Level w.r.t Heart Diseases
ggplot(heart , aes(x=trestbps , y=chol)) +
  ggtitle("Relationship b/w Blood Pressure and Serum Cholesterol Level w.r.t Heart Diseases") +
  geom_point(col="navy") +
  xlab("Resting Blood Pressure") + ylab("Serum Cholesterol Level") +
  facet_wrap(~target)





########################################################################################
## 1.6            Statistical Analysis of Fasting Blood Sugar Level Stats              #
########################################################################################

#    1.6.1 Basic Structure
str(heart$fbs)
table(heart$fbs)

#    1.6.2 Bar Plot
ggplot(heart , aes(x=fbs)) +
  ggtitle("Fasting Blood Sugar Level Distribution") +
  geom_bar(col="black" , fill="red") +
  xlab("Diabetic Status") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.6.3 Relationship b/w Fasting Glucose Level and Heart Diseases
ggplot(heart , aes(x=fbs , fill=target)) +
  ggtitle("Relationship b/w Diabestic Status and Heart Diseases") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Diabetic Status") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.6.4 Relationship b/w Age and Fasting Glucose Level w.r.t Heart Diseases
ggplot(heart , aes(x=age , fill=fbs)) +
  ggtitle("Relationship b/w Age and Fasting Glucose Level w.r.t Heart Diseases") +
  geom_histogram(col="black") +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Diabetic Status") +
  facet_wrap(~target)

#    1.6.5 Relationship b/w Fasting Glucose Level and Heart Diseases w.r.t Sex
ggplot(heart , aes(x=fbs , fill=target)) +
  ggtitle("Relationship b/w Fasting Glucose Level and Heart Diseases w.r.t Sex") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Diabetic Status") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~sex)

#    1.6.6 Relationship b/w Fasting Glucose Level and Chest Pains w.r.t Heart Diseases
ggplot(heart , aes(x=fbs , fill=cp)) +
  ggtitle("Relationship b/w Fasting Glucose Level and Chest Pains w.r.t Heart Diseases") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Diabetic Status") + ylab("Total Number of Individuals") + labs(fill="Chest Pains") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.6.7 Relationship b/w Blood Pressure and Glucose Level w.r.t Heart Diseases
ggplot(heart , aes(x=trestbps , fill=fbs)) +
  ggtitle("Relationship b/w Blood Pressure and Glucose Level w.r.t Heart Diseases") +
  geom_histogram(col="black" , binwidth = 30) +
  xlab("Resting Blood Pressure") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  facet_wrap(~target)

#    1.6.8 Relationship b/w Cholesterol Level and Glucose Level w.r.t Heart Diseases
ggplot(heart , aes(x=chol , fill=target)) +
  ggtitle("Relationship b/w Cholesterol Level and Heart Diseases w.r.t Glucose Level") +
  geom_histogram(col="black" , binwidth = 30) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  facet_wrap(~fbs)




########################################################################################
## 1.7         Statistical Analysis of Resting Electrocardiograph Results              #
########################################################################################

#    1.7.1 Basic Structure
str(heart$restecg)
table(heart$restecg)

#    1.7.2 Bar Plot
ggplot(heart , aes(x=restecg)) +
  ggtitle("Resting Electrocardiograph Distribution") +
  geom_bar(col="black" , fill="blue") +
  xlab("Resting Electrocardiograph Readings") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.7.3 Relationship b/w Resting Electrocardiograph Results and Heart Diseases
ggplot(heart , aes(x=restecg , fill=target)) +
  ggtitle("Relationship b/w Resting Electrocardiograph Results and Heart Diseases") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Resting Electrocardiograph Readings") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.7.4 Relationship b/w Age and Heart Diseases w.r.t Resting Electrocardiograph Results
ggplot(heart , aes(x=age , fill=restecg)) +
  ggtitle("Relationship b/w Age and Heart Diseases w.r.t Resting Electrocardiograph Results") +
  geom_histogram(col="black" , bins=30) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Electrocardiograph Readings") +
  facet_wrap(~target)

#    1.7.5 Relationship b/w Resting Electrocardiograph Results and Heart Diseases w.r.t Sex
ggplot(heart , aes(x=restecg , fill=target)) +
  ggtitle("Relationship b/w Electrocardiograph Results and Heart Diseases w.r.t Sex") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Resting Electrocardiograph Readings") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~sex)

#    1.7.6 Relationship b/w Resting Electrocardiograph Results and Chest Pains w.r.t Heart Diseases 
ggplot(heart , aes(x=restecg , fill=cp)) +
  ggtitle("Relationship b/w Electrocardiograph Results and Chest Pains w.r.t Heart Diseases ") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Resting Electrocardiograph Readings") + ylab("Total Number of Individuals") + labs(fill="Chest Pains") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.7.7 Relationship b/w Serum Cholesterol Level and Resting Electrocardiograph Results w.r.t Heart Diseases
ggplot(heart , aes(x=chol , fill=restecg)) +
  ggtitle("Relationship b/w Cholesterol Level and Electrocardiograph Results w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=30) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Electrocardiograph Results") +
  facet_wrap(~target)

#    1.7.8 Relationship b/w Fasting Blood Glucose Level and Resting Electrocardiograph Results w.r.t Heart Diseases 
ggplot(heart , aes(x=fbs , fill=restecg)) +
  ggtitle("Relationship b/w Glucose Level and Electrocardiograph Results w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Resting Electrocardiograph Readings") + ylab("Total Number of Individuals") + labs(fill="Electrocardiograph Results") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=4 , position = position_stack(0.75)) +
  facet_wrap(~target)




########################################################################################
## 1.8         Statistical Analysis of Maximum Heart Rate Achieved                     #
########################################################################################

#    1.8.1 Basic Structure
str(heart$thalach)
summary(heart$thalach)
sd(heart$thalach)

#    1.8.2 Histogram Plot
ggplot(heart , aes(x=thalach)) +
  ggtitle("Maximum Heart Rate Distribution") +
  geom_histogram(col="white" , fill="navy" , bins=30) +
  xlab("Maximum Heart Rate") + ylab("Total Number of Individuals") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=2 , position = position_stack(2))

#    1.8.3 Relationship b/w Maximum Heart Rate and Heart Diseases
ggplot(heart , aes(x=thalach , fill=target)) +
  ggtitle("Relationship b/w Maximum Heart Rate and Heart Diseases") +
  geom_histogram(col="white", binwidth = 20) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Maximum Heart Rate") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases")

#    1.8.4 Relationship b/w Age and Maximum Heart Rate w.r.t Heart Diseases
ggplot(heart , aes(x=age , y=thalach)) +
  ggtitle("Relationship b/w Age and Maximum Heart Rate w.r.t Heart Diseases") +
  geom_point(col="navyblue") +
  xlab("Age of Individuals") + ylab("Serum Cholesterol Level") +
  facet_wrap(~target)

#    1.8.5 Relationship b/w Maximum Heart Rate and Heart Diseases w.r.t Sex
ggplot(heart , aes(x=thalach , fill=target)) +
  ggtitle("Relationship b/w Maximum Heart Rate and Heart Diseases w.r.t Sex") +
  geom_histogram(col="black" , binwidth = 20) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Maximum Heart Rate") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  facet_wrap(~sex)

#    1.8.6 Relationship b/w Maximum Heart Rate and Chest Pains w.r.t Heart Diseases
ggplot(heart , aes(x=thalach , fill=cp)) +
  ggtitle("Relationship b/w Maximum Heart Rate and Chest Pains w.r.t Heart Diseases") +
  geom_histogram(col="black" , binwidth = 20) +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Maximum Heart Rate") + ylab("Total Number of Individuals") + labs(fill="Chest Pains") +
  facet_wrap(~target)

#    1.8.7 Relationship b/w Resting Blood Pressure and Maximum Heart Rate w.r.t Heart Diseases
ggplot(heart , aes(x=trestbps , y=thalach)) +
  ggtitle("Relationship b/w Blood Pressure and Maximum Heart Rate w.r.t Heart Diseases") +
  geom_point(col="darkorchid4") +
  xlab("Resting Blood Pressure") + ylab("Maximum Heart Rate")+ 
  facet_wrap(~target)

#    1.8.8 Relationship b/w Serum Cholesterol Level and Maximum Heart Rate w.r.t Heart Diseases
ggplot(heart , aes(x=chol , y=thalach)) +
  ggtitle("Relationship b/w Serum Cholesterol Level and Maximum Heart Rate w.r.t Heart Diseases") +
  geom_point(col="darkred") +
  xlab("Serum Cholesterol Level") + ylab("Maximum Heart Rate")+ 
  facet_wrap(~target)

#    1.8.9 Relationship b/w Maximum Heart Rate and Heart Diseases w.r.t Fasting Glucose Level
ggplot(heart , aes(x=thalach , fill=target)) +
  ggtitle("Relationship b/w Maximum Heart Rate and Heart Diseases w.r.t Glucose Level") +
  geom_histogram(col="black" , binwidth = 20) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Maximum Heart Rate") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  facet_wrap(~fbs)

#    1.8.10 Relationship b/w Maximum Heart Rate and Electrocardiograph Results w.r.t Heart Diseases
ggplot(heart , aes(x=thalach , fill=restecg)) +
  ggtitle("Relationship b/w Maximum Heart Rate and Electrocardiograph Results w.r.t Heart Diseases") +
  geom_histogram(col="black" , binwidth = 20) +
  xlab("Maximum Heart Rate") + ylab("Total Number of Individuals") + labs(fill="Chest Pains") +
  facet_wrap(~target)




########################################################################################
## 1.9         Statistical Analysis of Resting Exercise Induced Angina                #
########################################################################################

#    1.9.1 Basic Structure
str(heart$exang)
table(heart$exang)

#    1.9.2 Bar Plot
ggplot(heart , aes(x=exang)) +
  ggtitle("Exercise Induced Angina Distribution") +
  geom_bar(col="black" , fill="blue") +
  xlab("Exercise Induced Angina") + ylab("Total Number of Individuals") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.9.3 Relationship b/w Resting Exercise Induced Angina and Heart Diseases
ggplot(heart , aes(x=exang , fill=target)) +
  ggtitle("Relationship b/w Exercise Induced Angina and Heart Diseases") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Exercise Induced Angina") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.9.4 Relationship b/w Age and Heart Diseases w.r.t Exercise Induced Angina
ggplot(heart , aes(x=age , fill=target)) +
  ggtitle("Relationship b/w Age and Heart Diseases w.r.t Exercise Induced Angina") +
  geom_histogram(col="black" , bins=30) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  facet_wrap(~exang)

#    1.9.5 Relationship b/w Exercise Induced Angina and Heart Diseases w.r.t Sex
ggplot(heart , aes(x=exang , fill=target)) +
  ggtitle("Relationship b/w Exercise Induced Angina and Heart Diseases w.r.t Sex") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Exercise Induced Angina") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~sex)

#    1.9.6 Relationship b/w Exercise Induced Angina and Chest Pains w.r.t Heart Diseases 
ggplot(heart , aes(x=exang , fill=cp)) +
  ggtitle("Relationship b/w Exercise Induced Angina and Chest Pains w.r.t Heart Diseases ") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Exercise Induced Angina") + ylab("Total Number of Individuals") + labs(fill="Chest Pains") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.9.7 Relationship b/w Resting Blood Pressure and Exercise Induced Angina w.r.t Heart Diseases
ggplot(heart , aes(x=trestbps , fill=exang)) +
  ggtitle("Relationship b/w Blood Pressure and Exercise Induced Angina w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=30) +
  xlab("Resting Blood Pressure") + ylab("Total Number of Individuals") + labs(fill="Exercise Induced Angina") +
  facet_wrap(~target)

#    1.9.8 Relationship b/w Serum Cholesterol Level and Exercise Induced Angina w.r.t Heart Diseases
ggplot(heart , aes(x=chol , fill=exang)) +
  ggtitle("Relationship b/w Cholesterol Level and Exercise Induced Angina w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=30) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Exercise Induced Angina") +
  facet_wrap(~target)

#    1.9.9 Relationship b/w Fasting Blood Glucose Level and Exercise Induced Angina w.r.t Heart Diseases 
ggplot(heart , aes(x=fbs , fill=exang)) +
  ggtitle("Relationship b/w Glucose Level and Exercise Induced Angina w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Exercise Induced Angina") + ylab("Total Number of Individuals") + labs(fill="Exercise Induced Angina") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=4 , position = position_stack(0.75)) +
  facet_wrap(~target)

#    1.9.10 Relationship b/w Resting Electrocardiograph Results and Exercise Induced Angina w.r.t Heart Diseases 
ggplot(heart , aes(x=restecg , fill=exang)) +
  ggtitle("Relationship b/w Electrocardiograph Results and Exercise Induced Angina w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Resting Electrocardiograph Results") + ylab("Total Number of Individuals") + labs(fill="Exercise Induced Angina") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=4 , position = position_stack(0.75)) +
  facet_wrap(~target)

#    1.9.11 Relationship b/w Maximum Heart Rate and Exercise Induced Angina w.r.t Heart Diseases
ggplot(heart , aes(x=thalach , fill=exang)) +
  ggtitle("Relationship b/w Maximum Heart Rate and Exercise Induced Angina w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=20) +
  xlab("Maximum Heart Rate") + ylab("Total Number of Individuals") + labs(fill="Exercise Induced Angina") +
  facet_wrap(~target)




########################################################################################
## 1.10        Statistical Analysis of ST depression Variable                          #
########################################################################################

#    1.10.1 Basic Structure
str(heart$oldpeak)
table(heart$oldpeak)
summary(heart$oldpeak)
sd(heart$oldpeak)

#    1.10.2 Histogram Plot
ggplot(heart , aes(x=oldpeak)) +
  ggtitle("ST depression Distribution") +
  geom_histogram(col="white" , fill="blue4" , bins=5) +
  xlab("ST depression") + ylab("Total Number of Individuals") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=2 , position = position_stack(2))

#    1.10.3 Relationship b/w Old Peak and Heart Diseases
ggplot(heart , aes(x=oldpeak , fill=target)) +
  ggtitle("Relationship b/w ST depression and Heart Diseases") +
  geom_histogram(col="white", bins =10) +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Maximum Heart Rate") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases")




########################################################################################
## 1.11         Statistical Analysis of Resting Slope of ST                            #
########################################################################################

#    1.11.1 Basic Structure
str(heart$slope)
table(heart$slope)

#    1.11.2 Bar Plot
ggplot(heart , aes(x=slope)) +
  ggtitle("Slope of ST Distribution") +
  geom_bar(col="black" , fill="blue") +
  xlab("Slope of ST") + ylab("Total Number of Individuals") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.11.3 Relationship b/w Resting Slope of ST and Heart Diseases
ggplot(heart , aes(x=slope , fill=target)) +
  ggtitle("Relationship b/w Slope of ST and Heart Diseases") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Slope of ST") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.11.4 Relationship b/w Age and Slope of ST w.r.t Heart Diseases
ggplot(heart , aes(x=age , fill=slope)) +
  ggtitle("Relationship b/w Age and Heart Diseases w.r.t Slope of ST") +
  geom_histogram(col="black" , bins=30) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Slope of ST") +
  facet_wrap(~target)

#    1.11.5 Relationship b/w Slope of ST and Heart Diseases w.r.t Sex
ggplot(heart , aes(x=slope , fill=target)) +
  ggtitle("Relationship b/w Slope of ST and Heart Diseases w.r.t Sex") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Slope of ST") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~sex)

#    1.11.6 Relationship b/w Slope of ST and Chest Pains w.r.t Heart Diseases 
ggplot(heart , aes(x=slope , fill=cp)) +
  ggtitle("Relationship b/w Slope of ST and Chest Pains w.r.t Heart Diseases ") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Slope of ST") + ylab("Total Number of Individuals") + labs(fill="Chest Pains") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.11.7 Relationship b/w Resting Blood Pressure and Slope of ST w.r.t Heart Diseases
ggplot(heart , aes(x=trestbps , fill=slope)) +
  ggtitle("Relationship b/w Blood Pressure and Slope of ST w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=30) +
  xlab("Resting Blood Pressure") + ylab("Total Number of Individuals") + labs(fill="Slope of ST") +
  facet_wrap(~target)

#    1.11.8 Relationship b/w Serum Cholesterol Level and Slope of ST w.r.t Heart Diseases
ggplot(heart , aes(x=chol , fill=slope)) +
  ggtitle("Relationship b/w Cholesterol Level and Slope of ST w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=30) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Slope of ST") +
  facet_wrap(~target)

#    1.11.9 Relationship b/w Fasting Blood Glucose Level and Slope of ST w.r.t Heart Diseases 
ggplot(heart , aes(x=fbs , fill=slope)) +
  ggtitle("Relationship b/w Glucose Level and Slope of ST w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Diabetic Status") + ylab("Total Number of Individuals") + labs(fill="Slope of ST") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.11.10 Relationship b/w Resting Electrocardiograph Results and Slope of ST w.r.t Heart Diseases 
ggplot(heart , aes(x=restecg , fill=slope)) +
  ggtitle("Relationship b/w Electrocardiograph Results and Slope of ST w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Resting Electrocardiograph Results") + ylab("Total Number of Individuals") + labs(fill="Slope of ST") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.11.11 Relationship b/w Maximum Heart Rate and Slope of ST w.r.t Heart Diseases
ggplot(heart , aes(x=thalach , fill=slope)) +
  ggtitle("Relationship b/w Maximum Heart Rate and Slope of ST w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=20) +
  xlab("Maximum Heart Rate") + ylab("Total Number of Individuals") + labs(fill="Slope of ST") +
  facet_wrap(~target)

#    1.11.12 Relationship b/w Exercise Induced Angina and Slope of ST w.r.t Heart Diseases
ggplot(heart , aes(x=exang , fill=slope)) +
  ggtitle("Relationship b/w Slope of ST and Slope of ST w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Exercise Induced Angina") + ylab("Total Number of Individuals") + labs(fill="Slope of ST") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)




########################################################################################
## 1.12         Statistical Analysis of Coronary Arteries                            #
########################################################################################

#    1.12.1 Basic Structure
str(heart$ca)
table(heart$ca)

#    1.12.2 Bar Plot
ggplot(heart , aes(x=ca)) +
  ggtitle("Coronary Arteries Distribution") +
  geom_bar(col="black" , fill="blue") +
  xlab("Coronary Arteries") + ylab("Total Number of Individuals") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.12.3 Relationship b/w Resting Coronary Arteries and Heart Diseases
ggplot(heart , aes(x=ca , fill=target)) +
  ggtitle("Relationship b/w Coronary Arteries and Heart Diseases") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Coronary Arteries") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.12.4 Relationship b/w Age and Coronary Arteries w.r.t Heart Diseases
ggplot(heart , aes(x=age , fill=ca)) +
  ggtitle("Relationship b/w Age and Coronary Arteries w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=30) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Coronary Arteries") +
  facet_wrap(~target)

#    1.12.5 Relationship b/w Coronary Arteries and Heart Diseases w.r.t Sex
ggplot(heart , aes(x=ca , fill=target)) +
  ggtitle("Relationship b/w Coronary Arteries and Heart Diseases w.r.t Sex") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Coronary Arteries") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~sex)

#    1.12.6 Relationship b/w Coronary Arteries and Chest Pains w.r.t Heart Diseases 
ggplot(heart , aes(x=ca , fill=cp)) +
  ggtitle("Relationship b/w Coronary Arteries and Chest Pains w.r.t Heart Diseases ") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Coronary Arteries") + ylab("Total Number of Individuals") + labs(fill="Chest Pains") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.12.7 Relationship b/w Resting Blood Pressure and Coronary Arteries w.r.t Heart Diseases
ggplot(heart , aes(x=trestbps , fill=ca)) +
  ggtitle("Relationship b/w Blood Pressure and Coronary Arteries w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=30) +
  xlab("Resting Blood Pressure") + ylab("Total Number of Individuals") + labs(fill="Coronary Arteries") +
  facet_wrap(~target)

#    1.12.8 Relationship b/w Serum Cholesterol Level and Coronary Arteries w.r.t Heart Diseases
ggplot(heart , aes(x=chol , fill=ca)) +
  ggtitle("Relationship b/w Cholesterol Level and Coronary Arteries w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=30) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Coronary Arteries") +
  facet_wrap(~target)

#    1.12.9 Relationship b/w Fasting Blood Glucose Level and Coronary Arteries w.r.t Heart Diseases 
ggplot(heart , aes(x=fbs , fill=ca)) +
  ggtitle("Relationship b/w Glucose Level and Coronary Arteries w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Coronary Arteries") + ylab("Total Number of Individuals") + labs(fill="Coronary Arteries") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.12.10 Relationship b/w Resting Electrocardiograph Results and Coronary Arteries w.r.t Heart Diseases 
ggplot(heart , aes(x=restecg , fill=ca)) +
  ggtitle("Relationship b/w Electrocardiograph Results and Coronary Arteries w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Resting Electrocardiograph Results") + ylab("Total Number of Individuals") + labs(fill="Coronary Arteries") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.12.11 Relationship b/w Maximum Heart Rate and Coronary Arteries w.r.t Heart Diseases
ggplot(heart , aes(x=thalach , fill=ca)) +
  ggtitle("Relationship b/w Maximum Heart Rate and Coronary Arteries w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=20) +
  xlab("Maximum Heart Rate") + ylab("Total Number of Individuals") + labs(fill="Coronary Arteries") +
  facet_wrap(~target)

#    1.12.12 Relationship b/w Exercise Induced Angina and Coronary Arteries w.r.t Heart Diseases
ggplot(heart , aes(x=exang , fill=ca)) +
  ggtitle("Relationship b/w Exercise Induced Angina and Coronary Arteries w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Exercise Induced Angina") + ylab("Total Number of Individuals") + labs(fill="Coronary Arteries") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.12.13 Relationship b/w Slope of ST and Coronary Arteries w.r.t Heart Diseases
ggplot(heart , aes(x=slope , fill=ca)) +
  ggtitle("Relationship b/w Slope of ST and Coronary Arteries w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Slope of ST") + ylab("Total Number of Individuals") + labs(fill="Coronary Arteries") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)




########################################################################################
## 1.13                 Thalassemia Distribution Statistics                            #
########################################################################################

#    1.13.1 Basic Structure
str(heart$thal)
table(heart$thal)

#    1.13.2 Bar Plot
ggplot(heart , aes(x=thal)) +
  ggtitle("Thalassemia Distribution") +
  geom_bar(col="black" , fill="red") +
  xlab("Thalassemia") + ylab("Total Number of Individuals") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.13.3 Relationship b/w Resting Thalassemia and Heart Diseases
ggplot(heart , aes(x=thal , fill=target)) +
  ggtitle("Relationship b/w Thalassemia and Heart Diseases") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Thalassemia") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5))

#    1.13.4 Relationship b/w Age and Thalassemia w.r.t Heart Diseases
ggplot(heart , aes(x=age , fill=thal)) +
  ggtitle("Relationship b/w Age and Thalassemia w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=30) +
  xlab("Age of Individuals") + ylab("Total Number of Individuals") + labs(fill="Thalassemia") +
  facet_wrap(~target)

#    1.13.5 Relationship b/w Thalassemia and Heart Diseases w.r.t Sex
ggplot(heart , aes(x=thal , fill=target)) +
  ggtitle("Relationship b/w Thalassemia and Heart Diseases w.r.t Sex") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("firebrick", "darkslateblue","green4", "cyan3")) +
  xlab("Thalassemia") + ylab("Total Number of Individuals") + labs(fill="Heart Diseases") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~sex)

#    1.13.6 Relationship b/w Thalassemia and Chest Pains w.r.t Heart Diseases 
ggplot(heart , aes(x=thal , fill=cp)) +
  ggtitle("Relationship b/w Thalassemia and Chest Pains w.r.t Heart Diseases ") +
  geom_bar(col="black") +
  scale_fill_manual(values=c("green4", "cyan3","magenta4", "gold3")) +
  xlab("Thalassemia") + ylab("Total Number of Individuals") + labs(fill="Chest Pains") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.13.7 Relationship b/w Resting Blood Pressure and Thalassemia w.r.t Heart Diseases
ggplot(heart , aes(x=trestbps , fill=thal)) +
  ggtitle("Relationship b/w Blood Pressure and Thalassemia w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=30) +
  xlab("Resting Blood Pressure") + ylab("Total Number of Individuals") + labs(fill="Thalassemia") +
  facet_wrap(~target)

#    1.13.8 Relationship b/w Serum Cholesterol Level and Thalassemia w.r.t Heart Diseases
ggplot(heart , aes(x=chol , fill=thal)) +
  ggtitle("Relationship b/w Cholesterol Level and Thalassemia w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=30) +
  xlab("Serum Cholesterol Level") + ylab("Total Number of Individuals") + labs(fill="Thalassemia") +
  facet_wrap(~target)

#    1.13.9 Relationship b/w Fasting Blood Glucose Level and Thalassemia w.r.t Heart Diseases 
ggplot(heart , aes(x=fbs , fill=thal)) +
  ggtitle("Relationship b/w Glucose Level and Thalassemia w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Thalassemia") + ylab("Total Number of Individuals") + labs(fill="Thalassemia") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.13.10 Relationship b/w Resting Electrocardiograph Results and Thalassemia w.r.t Heart Diseases 
ggplot(heart , aes(x=restecg , fill=thal)) +
  ggtitle("Relationship b/w Electrocardiograph Results and Thalassemia w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Resting Electrocardiograph Results") + ylab("Total Number of Individuals") + labs(fill="Thalassemia") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.13.11 Relationship b/w Maximum Heart Rate and Thalassemia w.r.t Heart Diseases
ggplot(heart , aes(x=thalach , fill=thal)) +
  ggtitle("Relationship b/w Maximum Heart Rate and Thalassemia w.r.t Heart Diseases") +
  geom_histogram(col="black" , bins=20) +
  xlab("Maximum Heart Rate") + ylab("Total Number of Individuals") + labs(fill="Thalassemia") +
  facet_wrap(~target)

#    1.13.12 Relationship b/w Exercise Induced Angina and Thalassemia w.r.t Heart Diseases
ggplot(heart , aes(x=exang , fill=thal)) +
  ggtitle("Relationship b/w Exercise Induced Angina and Thalassemia w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Exercise Induced Angina") + ylab("Total Number of Individuals") + labs(fill="Thalassemia") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.13.13 Relationship b/w Slope of ST and Thalassemia w.r.t Heart Diseases
ggplot(heart , aes(x=slope , fill=thal)) +
  ggtitle("Relationship b/w Slope of ST and Thalassemia w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Slope of ST") + ylab("Total Number of Individuals") + labs(fill="Thalassemia") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)

#    1.13.14 Relationship b/w Coronary Arteries and Thalassemia w.r.t Heart Diseases
ggplot(heart , aes(x=ca , fill=thal)) +
  ggtitle("Relationship b/w Coronary Arteries and Thalassemia w.r.t Heart Diseases") +
  geom_bar(col="black") +
  xlab("Coronary Arteries") + ylab("Total Number of Individuals") + labs(fill="Thalassemia") +
  geom_text(stat = "count" , aes(label=..count..) , color="white" , size=3 , position = position_stack(0.5)) +
  facet_wrap(~target)




########################################################################################
#                     Diseases Modeling Using Random Forest Algorithm                  #
########################################################################################

# Set random seed to make results reproducible:
set.seed(1234)

# Calculate the size of each of the data set:
dataset_size <- floor(nrow(heart)/2)

# Generate a random sample of "dataset_size" indexes
indexes <- sample(1:nrow(heart), size = dataset_size)

# Assign the data to the correct sets
train <- heart[indexes,]
validate <- heart[-indexes,]
test <- validate[1:nrow(validate), c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal")]

# Label setting
rf.label <-  train$target[1:nrow(train)]


########################################################################################
#                                 Feature Selection                                    #
########################################################################################

#Model 1
rf.train1 <- train[1:nrow(train), c("age", "sex")]

#Model 2
rf.train2 <- train[1:nrow(train), c("age", "sex", "cp")]

#Model 3
rf.train3 <- train[1:nrow(train), c("age", "sex", "cp", "trestbps")]

#Model 4
rf.train4 <- train[1:nrow(train), c("age", "sex", "cp", "trestbps", "chol")]

#Model 5
rf.train5 <- train[1:nrow(train), c("age", "sex", "cp", "trestbps", "chol", "fbs")]

#Model 6
rf.train6 <- train[1:nrow(train), c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg")]

#Model 7
rf.train7 <- train[1:nrow(train), c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach")]

#Model 8
rf.train8 <- train[1:nrow(train), c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang")]

#Model 9
rf.train9 <- train[1:nrow(train), c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak")]

#Model 10
rf.train10 <- train[1:nrow(train), c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope")]

#Model 11
rf.train11 <- train[1:nrow(train), c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope")]

#Model 12
rf.train12 <- train[1:nrow(train), c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca")]

#Model 13
rf.train13 <- train[1:nrow(train), c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal")]

#Model 14
rf.train14 <- train[1:nrow(train), c( "age", "sex", "cp", "trestbps", "chol", "ca", "thal")]




########################################################################################
#                                   Model Training                                     #
########################################################################################

library(randomForest)

# Lets train our model 1
set.seed(1234)
rf.1 <- randomForest(x=rf.train1, y=rf.label, importance = TRUE, ntree=2000)
rf.1
varImpPlot(rf.1)

#Train Model 2
set.seed(1234)
rf.2 <- randomForest(x=rf.train2, y=rf.label, importance = TRUE, ntree=2000)
rf.2
varImpPlot(rf.2)

#Train Model 3
set.seed(1234)
rf.3 <- randomForest(x=rf.train3, y=rf.label, importance = TRUE, ntree=2000)
rf.3
varImpPlot(rf.3)

#Train Model 4
set.seed(1234)
rf.4 <- randomForest(x=rf.train4, y=rf.label, importance = TRUE, ntree=2000)
rf.4
varImpPlot(rf.4)

#Train Model 5
set.seed(1234)
rf.5 <- randomForest(x=rf.train5, y=rf.label, importance = TRUE, ntree=2000)
rf.5
varImpPlot(rf.5)

#Train Model 6
set.seed(1234)
rf.6 <- randomForest(x=rf.train6, y=rf.label, importance = TRUE, ntree=2000)
rf.6
varImpPlot(rf.6)

#Train Model 7
set.seed(1234)
rf.7 <- randomForest(x=rf.train7, y=rf.label, importance = TRUE, ntree=2000)
rf.7
varImpPlot(rf.7)

#Train Model 8
set.seed(1234)
rf.8 <- randomForest(x=rf.train8, y=rf.label, importance = TRUE, ntree=2000)
rf.8
varImpPlot(rf.8)

#Train Model 9
set.seed(1234)
rf.9 <- randomForest(x=rf.train9, y=rf.label, importance = TRUE, ntree=2000)
rf.9
varImpPlot(rf.9)

#Train Model 10
set.seed(1234)
rf.10 <- randomForest(x=rf.train10, y=rf.label, importance = TRUE, ntree=2000)
rf.10
varImpPlot(rf.10)

#Train Model 11
set.seed(1234)
rf.11 <- randomForest(x=rf.train11, y=rf.label, importance = TRUE, ntree=2000)
rf.11
varImpPlot(rf.11)

#Train Model 12
set.seed(1234)
rf.12 <- randomForest(x=rf.train12, y=rf.label, importance = TRUE, ntree=2000)
rf.12
varImpPlot(rf.12)

#Train Model 13
set.seed(1234)
rf.13 <- randomForest(x=rf.train13, y=rf.label, importance = TRUE, ntree=2000)
rf.13
varImpPlot(rf.13)

#Train Model 14
set.seed(1234)
rf.14 <- randomForest(x=rf.train14, y=rf.label, importance = TRUE, ntree=2000)
rf.14
varImpPlot(rf.14)

#Trying different number of trees
set.seed(1234)
rf.15 <- randomForest(x=rf.train14, y=rf.label, importance = TRUE, ntree=1850)
rf.15
varImpPlot(rf.15)


########################################################################################
#                                 Cross Validation                                     #
########################################################################################

library(caret)
library(doSNOW)

# For cross validation, the caret package will be used. 

# In the first step, Multi-Fold models will be generated 
set.seed(23456)
cv.10.folds <- createMultiFolds(rf.label, k=10, times = 20)


# Now lets check the stratification
table(rf.label)

table(rf.label[cv.10.folds[[33]]])

# Now let's train Control using Caret
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)

#  now lets set the cores using doSNOW package

c1 <- makeCluster(4, type = "SOCK")
registerDoSNOW(c1)


# Now lets start 10 fold training 

set.seed(23456)
rf.15.cv10 <- train(x=rf.train14, y=rf.label, method = "rf", tuneLength = 3, ntree=2000, 
                    trControl = ctrl.1)
stopCluster(c1)
rf.15.cv10

#####
# After 10 fold analysis let do 5 fold analysis to check more accuracy. 

# In the first step, Multi-Fold models will be generated 
set.seed(23456)
cv.5.folds <- createMultiFolds(rf.label, k=5, times = 20)


# Now lets check the stratification
table(rf.label)

table(rf.label[cv.5.folds[[33]]])

# Now let's train Control using Caret
ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

#  now lets set the cores using doSNOW package

c1 <- makeCluster(4, type = "SOCK")
registerDoSNOW(c1)


# Now lets start 5 fold training 

set.seed(1234)
rf.15.cv5 <- train(x=rf.train14, y=rf.label, method = "rf", tuneLength = 3, ntree=2000, 
                   trControl = ctrl.2)
stopCluster(c1)
rf.15.cv5

#####
# After 5 fold analysis let do 3 fold analysis to check more accuracy. 

# In the first step, Multi-Fold models will be generated 
set.seed(23456)
cv.3.folds <- createMultiFolds(rf.label, k=3, times = 20)


# Now lets check the stratification
table(rf.label)

table(rf.label[cv.3.folds[[33]]])

# Now let's train Control using Caret
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

#  now lets set the cores using doSNOW package

c1 <- makeCluster(4, type = "SOCK")
registerDoSNOW(c1)


# Now lets start 3 fold training 

set.seed(1234)
rf.15.cv3 <- train(x=rf.train14, y=rf.label, method = "rf", tuneLength = 3, ntree=2000, 
                   trControl = ctrl.3)
stopCluster(c1)
rf.15.cv3



########################################################################################
#                                    Testing Model                                     #
#                                 Feature Selection                                    #
########################################################################################

#Using Validating Data set as our test data, Let's make some predictions
# Lets make predictions with Reference Models

rf.15.preds <- predict(rf.15, test)
table(rf.15.preds)

table(validate$target)

table(heart$target[1:512])



########################################################################################
#                                    Testing Model                                     #
#                                   Cross Validated                                    #
########################################################################################

# Lets have the predictions again with 10 fold training 

rf.15.cv10.preds <- predict(rf.15.cv10, test)
table(rf.15.cv10.preds)

table(validate$target)

# Lets have the predictions again with 5 fold training 

rf.15.cv5.preds <- predict(rf.15.cv5, test)
table(rf.15.cv5.preds)

table(validate$target)

# Lets have the predictions again with 3 fold training 

rf.15.cv3.preds <- predict(rf.15.cv3, test)
table(rf.15.cv3.preds)

table(validate$target)





########################################################################################
#                                     Saving Plots                                     #
########################################################################################

plots.dir.path  <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from = plots.png.paths, to=("C:/Users/hafiz/Downloads/EDA/EDA Plots"))
plots.png.detials <- file.info(plots.png.paths)
plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
sorted.png.names <- gsub(plots.dir.path, "C:/Users/hafiz/Downloads/EDA/EDA Plots", row.names(plots.png.detials), fixed=TRUE)
numbered.png.names <- paste0("C:/Users/hafiz/Downloads/EDA/EDA Plots", 1:length(sorted.png.names), ".png")

# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
file.rename(from=sorted.png.names, to=numbered.png.names)
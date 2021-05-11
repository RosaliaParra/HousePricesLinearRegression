####ASSIGNMENT 2

####GROUP 1

#### CONTRIBUTORS: ROSALIA PARRA

####PART 1: CONTRIBUTORS: ROSALIA PARRA 
setwd("C:/Users/Rosalia/Documents/Spring 2021/FI Data Analytics/Group Assignment 2")
getwd()

hp=read.csv("HousePrices.csv", header=TRUE)

#### i. Contruct a summary stat for all the variables in the HousePrices data.

summary(hp)

#### ii. What is the percentage of houses in the data with Driveway, Gas-Heat and Air-conditioning present?  

fix(hp)
attach(hp)


### Create dummy variables for Driveway, Gas-Heat and Air-Conditioning
names(hp)
dim(hp)

### Driveway dummy
names(hp)
dim(hp)
numrows=nrow(hp)
numrows
driveway_dummy<-matrix(nrow=numrows,ncol=1)
head(driveway_dummy)
head(driveway)

for (i in 1:numrows)
    {

	if (driveway[i]=='yes') {
   	driveway_dummy [i,]=1
	} else {
   	driveway_dummy[i,]=0
	}
}


head(driveway_dummy)
mean(driveway_dummy)
colnames(driveway_dummy)="driveway_dummy"



### Gasheat dummy

gasheat_dummy<-matrix(nrow=numrows,ncol=1)
head(gasheat_dummy)
head(gasheat)

for (i in 1:numrows)
    {

	if (gasheat[i]=='yes') {
   	gasheat_dummy [i,]=1
	} else {
   	gasheat_dummy[i,]=0
	}
}


head(gasheat_dummy)
mean(gasheat_dummy)
colnames(gasheat_dummy)="gasheat_dummy"

### Air-Con dummy

aircon_dummy<-matrix(nrow=numrows,ncol=1)
head(aircon_dummy)
head(aircon)

for (i in 1:numrows)
    {

	if (aircon[i]=='yes') {
   	aircon_dummy [i,]=1
	} else {
   	aircon_dummy[i,]=0
	}
}


head(aircon_dummy)
mean(aircon_dummy)
colnames(aircon_dummy)="aircon_dummy"

### Mean of dummy variables

install.packages("fBasics")
library(fBasics)
names(hp)

new_hp=cbind( driveway_dummy, gasheat_dummy, aircon_dummy )

m1=apply(new_hp,2,mean)
m1

#### iii.	Construct a linear regression model to test whether number of bedrooms influence house prices.  Provide a summary of the linear regression model using summary() function.  	
lm.fit=lm(price~bedrooms)
summary(lm.fit)

#### iv.	Construct a multiple linear regression model by including all variables as predictors of house prices (response variable) and observe the effect on the house prices. 
#### Provide a summary of the regression model using summary() function.  
#### Recreation dummy

rec_dummy<-matrix(nrow=numrows,ncol=1)
head(rec_dummy)
head(recreation)

for (i in 1:numrows)
    {

	if (recreation[i]=='yes') {
   	rec_dummy [i,]=1
	} else {
   	rec_dummy[i,]=0
	}
}


head(rec_dummy)
mean(rec_dummy)
colnames(rec_dummy)="rec_dummy"

#### Fullbase dummy
fb_dummy<-matrix(nrow=numrows,ncol=1)
head(fb_dummy)
head(fullbase)

for (i in 1:numrows)
    {

	if (fullbase[i]=='yes') {
   	fb_dummy [i,]=1
	} else {
   	fb_dummy[i,]=0
	}
}


head(fb_dummy)
mean(fb_dummy)
colnames(fb_dummy)="fb_dummy"

#### Prefer dummy

pre_dummy<-matrix(nrow=numrows,ncol=1)
head(pre_dummy)
head(prefer)

for (i in 1:numrows)
    {

	if (prefer[i]=='yes') {
   	pre_dummy [i,]=1
	} else {
   	pre_dummy[i,]=0
	}
}


head(pre_dummy)
mean(pre_dummy)
colnames(pre_dummy)="pre_dummy"

#### Multiple linear regression and summary of all variables on House Prices data

lm.fit1=lm(price~lotsize+bedrooms+bathrooms+stories+driveway_dummy+rec_dummy+fb_dummy+gasheat_dummy+aircon_dummy+garage+pre_dummy)
summary(lm.fit1)


####PART 2: CONTRIBUTORS: ROSALIA PARRA
####SECTION A
#### i. Attach the Credit data to the R environment

setwd("C:/Users/Rosalia/Documents/Spring 2021/FI Data Analytics/Group Assignment 2")

getwd()

cr=read.csv("Credit.csv", header=TRUE)

#### ii. Observe the number of rows in the Credit data. Observe the dimension of the Credit data.

numrows=nrow(cr)
numrows
dim(cr)

#### iii. Provide a summary stat for the variables in Credit data.	

summary(cr)

#### iv.	What is the percentage of Student in the Credit data?  What is the percentage of Female in the Credit data? 

### Student dummy

fix(cr)
attach(cr)
names(cr)
dim(cr)
numrows=nrow(cr)
numrows
student_dummy<-matrix(nrow=numrows,ncol=1)
head(student_dummy)
head(Student)

for (i in 1:numrows)
    {

	if (Student[i]=='Yes') {
   	student_dummy [i,]=1
	} else {
   	student_dummy[i,]=0
	}
}

#### Mean of Student in Credit Data
head(student_dummy)
mean(student_dummy)
colnames(student_dummy)="student_dummy"

#### Gender dummy

gender_dummy<-matrix(nrow=numrows,ncol=1)
head(gender_dummy)
head(Gender)

for (i in 1:numrows)
    {

	if (Gender[i]=='Female') {
   	gender_dummy [i,]=1
	} else {
   	gender_dummy[i,]=0
	}
}

#### Mean of Female in Credit Data
head(gender_dummy)
mean(gender_dummy)
colnames(gender_dummy)="gender_dummy"

####SECTION B

#### B. Construct a linear regression model as follows: 	
#### Response variable: Credit Card Balance 
#### Predictors: Credit Rating, Student, Credit Rating * Student (interaction terms)   

lm.fit=lm(Balance~Rating+Student+Rating*Student)

#### Provide a summary of the model using summary() function. 

summary(lm.fit)

#### PART 3 CONTRIBUTORS: ROSALIA PARRA

#### i. Test whether Age influence Credit Card Balance on the basis of simple linear regression.
####(Provide a summary of the model using summary() function). 

lm.fit2=lm(Balance~Age)
summary(lm.fit2)

#### ii. Use Age and Credit Rating as predictors of Credit Card Balance (response variable) in a multiple linear regression setting. 
lm.fit3=lm(Balance~Age+Rating)
summary(lm.fit3)

#### iii.	Compare effect of Age from part (i) and (ii).
#### In part (i.) when age was the sole variable for credit balance its' p-value was
high (.971) meaning the effect on credit card balance is not significant. In part (ii.)
the p-value for age lowered to .00048 meaning that its significance increased.  For every
1 % increase in age, it will result in -2.35% decrease in credit card balance, meaning age 
has a negative effect on balance.
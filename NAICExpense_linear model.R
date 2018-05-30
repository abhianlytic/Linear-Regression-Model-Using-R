#set the directory location where the data is stored
setwd("G://analytics//ISB CBA//Residency//Residency3&4//B9MT2SA2//SA 2 Assignment 1-20180206")

#Read the data from the location above
naic_data = read.csv("NAICExpense.csv",header = TRUE, stringsAsFactors = FALSE)

#lets check the structure of the data variable
str(naic_data)
# Data structure Type of Data
class(naic_data)
#summary statistics of the varibles
summary(naic_data)
#RBC,EXPENSE contains outlier data, which we will verify further, whether it is
# required or not.
#STAFWAGE is normally distributed
#AGENTWAGE has 19 missing values, and all are normally distributed
#Longloss, shortloss, GPWPERSONAL,GPWCOMM, Assets, Cash have outliers, which we
#will investigate further.

#shows missing value if any in any attributes
colSums(is.na(naic_data))
#There are 19 missing values in AGENTWAGE

nrow(naic_data)#total records available is 384

#now we will deal with missing value, There may be different ways to impute the
# misssing values, as the AGENTWAGE is normally distributed, we will impute the
# missing values with the mean of the data.

for (i in 1:ncol(naic_data[,c(5:15)])){
  if (!(is.factor(naic_data))){
    naic_data[is.na(naic_data[,i]),i]<- mean(naic_data[,i],na.rm = TRUE)
  }
}

#we can also impute by building a model by keeping AGENTWISE as response variable
# for new model and then use those data in our current mode, as of now we will
# stick to basic imputation.

#Check NA in the data
naic_data$AGENTWAGE[naic_data$AGENTWAGE=="NA"]
mean(naic_data$AGENTWAGE,na.rm = TRUE)
colSums(is.na(naic_data))#check the sum again to cross verify
# that we have handled the missing values

#Here our goal is to fit a suitable linear regression model that explains the 
#expenses of the insurance companies and to suggest business on how to minimize
#expenses.

#now we will split the data into train and test
#on train data we will fit the model
#on Test data we will evaluate the model

set.seed(777) ## To control the randomness of the sampling.
nrowdata = sample(1:nrow(naic_data),.8*nrow(naic_data))
naictrain = naic_data[nrowdata,] ## Trainset 80%
naictest = naic_data[-nrowdata,] ## Testset 20%
# Check the number of rows in each dataset
nrow(naictrain)
nrow(naictest)
colnames(naic_data)

#now we will analyse the quantitative data seperately i.e univariate analysis
# on train data
library(tidyverse)

naictrain %>% gather(RBC: LIQUIDRATIO, key = "variable", value = "value") %>%
  ggplot(aes(x=variable,y = value)) + 
  geom_boxplot() + facet_wrap(~ variable, scales = 'free_y')

#it is clear that there are outliers in each of the properties, so we will analyse
# the boxplot seperatly

for (i in 1:ncol(naictrain)){
  if((is.numeric(naictrain[,i]))){
    #print(ggplot(naictrain,aes(x=naictrain[,i]))+geom_boxplot(fill="gold1")+labs(x=colnames(naictrain)[i]))
    boxplot(naictrain[,i],main = colnames(naictrain)[i],col="blue")
  }
}


#now examine the response attribute
boxplot(naictrain$EXPENSES,main = "EXPENSES",col = "red")

#As the expenses are not normalize, so we need to normalize the expenses

#1st Method for normalization is to take straight log of the expenses
boxplot(log(naictrain$EXPENSES),main = "EXPENSES",col = "red")#able to normalize
#this transformation produces NaN, as there are negative expenses as well,thus
#we need to handle those negative expenses, though it normalizes the expenses.


#2nd method- now we will handle the negative of log, we will remove those records from
# train data set, and down the line analyse the values after building the model
# to make the expense normal is to remove the negative expenses, as done below
which(is.nan(log(naictrain$EXPENSES)))
#so the row no 98 216 243 have negative values, thus removing those from our analysis 

naictrainnew<-naictrain[!naictrain$EXPENSES<0,]
boxplot(log(naictrainnew$EXPENSES),main = "EXPENSES",col = "red")
#successfully able to normalize at the expense of few records, this may impact
# the model and if those values are legitimate values then we are in trouble. So
# we will consider those down the line.(and also business involvement is required.)

##3rd Method - we use absolute method i.e sign(x)*log(abs(x))
boxplot(sign(naictrain$EXPENSES)*log(abs(naictrain$EXPENSES)),main = "EXPENSES",col = "red")
#This transformation does not normalize the expense, so cant use this.


#4th Method - we have added .0021(we may add min(expenses)) to the expenses 
#to make it positive and then taken the log transformation
# to make it normal. Adding any value may require business explanaiton, which we 
# decide after we stabalize our model.
boxplot(log(naictrain$EXPENSES+.0021),main = "EXPENSES",col = "red")
#it does contain outliers, so is not fully normalized.

#5th method to make it normal 
boxplot(log((naictrain$EXPENSES)^2),main = "EXPENSES",col = "red")
#this contain fewer outliers and also normalizes the data best.

#so as of now, the best transformation we have are 1st,2nd and 5th method.
# we will see further if we require any of the above transformation for our model


#so the boxplot presents below analysis
#a. There are outliers in few variables like :RBC,EXPENSES,STAFFWAGE,AGENTWAGE,
# LONGLOSS,SHORTLOSS,GPWPERSONAL,GPWCOMM,ASSETS,CASH,LIQUIDRATIO.
#b.Few of the variables are not symmetric and they are : STAFFWAGE

#As the linear regression does not make any assumption on the distribution of
# the data, but understanding the data is useful, we will analyse the influencial 
# point later on after we build the model.

#now we will further analyse the distribution of Expenses to make it normal,
#We will check distribution of all the transformation we have seen earlier

library(ggplot2)
ggplot(naictrain,aes(x=naictrain$EXPENSES))+geom_density(fill="red")+labs(x="Expenses")
#Expenses are highly right skewed, now we will try log transformation

#1st Method Distribution - log(expenses)
ggplot(naictrain,aes(x=log(naictrain$EXPENSES)))+geom_density(fill="red")+labs(x="log(Expenses")
#seems normal

#2nd Method Distribution - after removing negative expenses and taking log
ggplot(naictrainnew,aes(x=log(naictrainnew$EXPENSES)))+geom_density(fill="red")+labs(x="Only Positive Expenses")
#This also seems bit normal

#3rd Method Distribution - absolute method
ggplot(naictrain,aes(x=sign(naictrain$EXPENSES)*log(abs(naictrain$EXPENSES))))+geom_density(fill="red")+labs(x="sign(Expenses)*log(abs(Expenses))")
#seems normal, but few values are outliers and skewed right

#4th Method Distribution - adding value to make log positive
ggplot(naictrain,aes(x=log(naictrain$EXPENSES+.0021)))+geom_density(fill="red")+labs(x="Log(Expenses+.0021)")
#not at all normal.

#5th Method Distribution - log of square of expenses
ggplot(naictrain,aes(x=log((naictrain$EXPENSES)^2)))+geom_density(fill="red")+labs(x="Log(square(Expenses)")
#seems normal

#now we will compare the distribution of all the transformation with normal
#distribution with similar mean and Standard deviation

#Normal
Normal<-rnorm(307,mean=mean(naictrain$EXPENSES),sd=sd(naictrain$EXPENSES))
naictrainnn<-data.frame(label = factor(rep(c("EXPENSES","Normal"),each=307)),x=c(naictrain$EXPENSES,Normal))
ggplot(naictrainnn,aes(x,fill=label))+geom_density(alpha=.2)
#The distribution is normal

#1st Method
LogNormal<-rnorm(307,mean=mean(log(naictrain$EXPENSES)),sd=sd(log(naictrain$EXPENSES)))
naictrainln<-data.frame(label = factor(rep(c("Log(EXPENSES)","LogNormal"),each=307)),x=c(log(naictrain$EXPENSES),LogNormal))
ggplot(naictrainln,aes(x,fill=label))+geom_density(alpha=.3)
#This does present the clear picture because of negative expenses.

#2nd Method
LogNormalPositive<-rnorm(304,mean=mean(log(naictrainnew$EXPENSES)),sd=sd(log(naictrainnew$EXPENSES)))
naictrainnewln<-data.frame(label = factor(rep(c("Log(PositiveEXPENSES)","LogNormalPositive"),each=304)),x=c(log(naictrainnew$EXPENSES),LogNormalPositive))
ggplot(naictrainnewln,aes(x,fill=label))+geom_density(alpha=.3)

#3rd Method
NormalAbsoluteLog<-rnorm(307,mean=mean(sign(naictrain$EXPENSES)*log(abs(naictrain$EXPENSES))),sd=sd(sign(naictrain$EXPENSES)*log(abs(naictrain$EXPENSES))))
naictrainlnabs<-data.frame(label = factor(rep(c("sign(expenses)*Log(abs(EXPENSES))","NormalAbsoluteLog"),each=307)),x=c(sign(naictrain$EXPENSES)*log(abs(naictrain$EXPENSES)),NormalAbsoluteLog))
ggplot(naictrainlnabs,aes(x,fill=label))+geom_density(alpha=.3)

#4th Method
LogNormaladd<-rnorm(307,mean=mean(log(naictrain$EXPENSES+.0021)),sd=sd(log(naictrain$EXPENSES+.0021)))
naictrainlnadd<-data.frame(label = factor(rep(c("Log(EXPENSES+.0021)","LogNormaladd"),each=307)),x=c(log(naictrain$EXPENSES+.0021),LogNormaladd))
ggplot(naictrainlnadd,aes(x,fill=label))+geom_density(alpha=.3)

#5th Method
LogNormalsqr<-rnorm(307,mean=mean(log((naictrain$EXPENSES)^2)),sd=sd(log((naictrain$EXPENSES)^2)))
naictrainlnsqr<-data.frame(label = factor(rep(c("Log(EXPENSES^2)","LogNormalsqr"),each=307)),x=c(log((naictrain$EXPENSES)^2),LogNormalsqr))
ggplot(naictrainlnsqr,aes(x,fill=label))+geom_density(alpha=.3)

#Normality of Error is important and that is why we dont care about dependent variable
# much

#Now we will analyse all the quantitative variable at once using correlation plots
#par(mfrow=c(1,1))
library(car)
library(corrgram)
library(corrplot)
library(corpcor)
scatterplot.matrix(naictrain[,-c(0:4)])
#This shows high correlation between CASH and ASSESTS...and also shows other
# correlation as well.
corrgram(naictrain[,-c(0:4)])
#dev.off()

#corrplot presents the correlation matrix of all variables.
corrplot(cor(naictrain[,-c(0:4)]),method="number")
cor2pcor(cor(naictrain[,-c(0:4)]))#this shows partial correlation between two
# variables leaving the other variables.
#So the correlation stats shows that are correlation between CASH and ASSESTS,
#SHORTLOSS and GPWPERSONAL and so on...

## Qualitative variables 
naictrain %>% gather(GROUP:STOCK, key = "variable", value = "value") %>% ggplot(aes(x = value)) +
  geom_bar()+ facet_wrap(~variable, scales = 'free_x')

table(naictrain$GROUP)
table(naictrain$MUTUAL)
table(naictrain$STOCK)
#There are close to 200 affiliated groups,close to 250 non-mutual companies
# and close to and above 200 stock companies.

# there is no such significance of qualitative data for linear model, but for
# understanding we have genereated few details.

#now we will start building basic models
model1 <- lm(EXPENSES~factor(GROUP)+factor(MUTUAL)+factor(STOCK)+RBC+
               STAFFWAGE+AGENTWAGE+LONGLOSS+SHORTLOSS+GPWPERSONAL+GPWCOMM
             +ASSETS+CASH+LIQUIDRATIO,data=naictrain[,-c(1)])
summary(model1)
anova(model1)
#Multiple R-squared:  0.9385,	Adjusted R-squared:  0.9357
#basic model says that LONGLOSS, SHORTLOSS, GPWPERSONAL,GPWCOMM and CASH are significant variable in the model
#Where are anova which apply F test on each variable says different thing, except
#MUTUAL,STAFFWAGE,LIQUIDRATIO all are significant, which we check further.

model2 <- lm(log(EXPENSES)~.,data=naictrain[,-c(1)])
summary(model2) # rejected model2, Multiple R-squared:  0.4367,	Adjusted R-squared:  0.4115
anova(model2)

model3 <- lm(EXPENSES~.,data=naictrainnew[,-c(1)])
summary(model3) # rejected model3
anova(model3)
#Multiple R-squared:  0.9393,	Adjusted R-squared:  0.9365, we reject coz it does
#not improves the R2 drastically and also deleting records is not what business
#would love to see.

model4 <- lm(sign(EXPENSES)*log(abs(EXPENSES))~.,data=naictrain[,-c(1)])
summary(model4) # reject model4
anova(model4)
#Multiple R-squared:  0.3447,	Adjusted R-squared:  0.3156, very low thus rejected
#this does not explain large variance in response, thus we cant move ahead with this model as stepping stone.

model5 <- lm(log((EXPENSES)^2)~.,naictrain[,-1])
summary(model5)
anova(model5)
#Multiple R-squared:  0.4228,	Adjusted R-squared:  0.3971, very low thus rejected
#this does not explain large variance in response, thus we cant move ahead with this model as well.

model6 <- lm(log(EXPENSES+.0021)~.,data=naictrain[,-c(1)])
summary(model6)
anova(model6)
#Multiple R-squared:  0.5424,	Adjusted R-squared:  0.5221, rejected as well

#So the best model which explains the variance in response is model1, so we will further
# analyse the 1st model
#Now we have fixed reponse variable, now will see regressors.

qqnorm(rstandard(model1))
qqline(rstandard(model1))
#does not show it is normal
plot(model1)

plot(model1$residuals~model1$fitted.values,main="Fitted values vs. Residuals")
plot(model1$residuals~naictrain$RBC,main="Regressor vs. Residuals")
#checking student residual distribution
library(MASS)

stresid <- studres(model1) 
hist(stresid, freq=FALSE, main="Distribution of Studentized Residuals") 
xfit<-seq(min(stresid),max(stresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
# The plot clearly shows the fit is good, now we will analyse further

library(car) 
residualPlot(model1, id.n=5)

residualPlots(model1, id.n=5)
#the conclusion we can draw from the residual plots 
#1. outliers points 248,245,236,225,138, we need to check whether these points are
# influencial or not.
#2.Residual plot shows that RBC, Shortloss and GPWPERSONAL have slight patterns
#3.As tukey test value is >.05 thus we dont need tranformation for X varibales.
#4.Here we fix the regressors, no transformation is requried.

#now we will check for collinearity
# we will check pairwise collinearity

vif(model1)
#ASSETS and CASH have high values, thus they are high correlated.

library(perturb)
colldiag(naictrain[,-1],center = TRUE)
#this also shows that ASSESTS and CASH are highly correlated and are the trouble
# maker
#we will not drop any variable as of now, as this will be taken care by AIC rule.

#Now we will move to finding best variables in the model1
step1 <-stepAIC(model1,direction = "both")
step1$anova

#so the best model is
best_model <- lm(EXPENSES ~ LONGLOSS + SHORTLOSS + GPWPERSONAL + GPWCOMM + 
                   CASH,data=naictrain[,-1])
summary(best_model) #Multiple R-squared:  0.9376,	Adjusted R-squared:  0.9365
anova(best_model)

#here both the summary and anova shows the same truth that all variables are 
# significant in the model.
par(mfrow=c(1,1))
qqnorm(rstandard(best_model))
qqline(rstandard(best_model))
#does not show normality for the points to the left and right.

plot(best_model$residuals~best_model$fitted.values,main="Fitted values vs. Residuals")

#checking student residual distribution
#library(MASS)

stresidbest <- studres(best_model) 
hist(stresidbest, freq=FALSE, main="Distribution of Studentized Residuals for model11") 
xfit<-seq(min(stresidbest),max(stresidbest),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
# The plot clearly shows the fit is good, now we will analyse further

#library(car) 
residualPlot(best_model, id.n=5)

residualPlots(best_model, id.n=5)

par(mfrow=c(1,1))
#checking for influential observation and deleting if required

cutoff <- 4/((nrow(naictrain)-length(best_model$coefficients)-2))
plot(best_model,which = 4, cook.levels = cutoff)
influencePlot(best_model,id.n=3) # A user friendly representation of the above
plot(best_model, which=4, cook.levels=cutoff)
#here we can see that 138,225,236 are not influencial points
#the influencial points are 175,248,245

#now we will build model without these points and see the difference.
#remove points 175,248,245
naictrain11 <- naictrain[-c(175,245,248),-1]
model11 <- lm(EXPENSES ~ LONGLOSS + SHORTLOSS + GPWPERSONAL + GPWCOMM + 
                CASH,data=naictrain11)
summary(model11) #Multiple R-squared:  0.9379,	Adjusted R-squared:  0.9369
anova(model11)
#There is only significant improvent in the model so we will live with our best_model

#now we will evaluate our model
y_hat<-predict.lm(best_model,newdata=naictest[,], se.fit=TRUE)$fit
y_hat<-as.vector(y_hat)
dev<-naictest$EXPENSES-y_hat
num<-sum(dev^2)
dev1<-naictest$EXPENSES-mean(naictest$EXPENSES)
den<-sum(dev1^2)
predicted.Rsq<- 1-(num/den)
predicted.Rsq

#so the evaluation shows that the R-Square is 90% thus model is able to explain
#90% variation of response variable in the test data as well, which is good as
# of now.

#Now we will build final model on original data
final_model <- lm(EXPENSES ~ LONGLOSS + SHORTLOSS + GPWPERSONAL + GPWCOMM + 
                    CASH,data=naic_data)
summary(final_model) #ultiple R-squared:  0.9387,	Adjusted R-squared:  0.9379
anova(final_model)

#here both the summary and anova shows the same truth that all variables are 
# significant in the final model.
par(mfrow=c(1,1))
qqnorm(rstandard(final_model))
qqline(rstandard(final_model))

plot(best_model$residuals~best_model$fitted.values,main="Fitted values vs. Residuals")

#checking student residual distribution

stresidbest <- studres(final_model) 
hist(stresidbest, freq=FALSE, main="Distribution of Studentized Residuals for model11") 
xfit<-seq(min(stresidbest),max(stresidbest),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

residualPlot(final_model, id.n=5)

residualPlots(final_model, id.n=5)
# The plot clearly shows the fit is good.

final_model$coefficients

#So following are the observation
#1. our model is linear fit which is
#    Expenses = LONGLOSS + SHORTLOSS + GPWPERSONAL + GPWCOMM + CASH
#2. Regressor coefficients are
# (Intercept)     LONGLOSS    SHORTLOSS  GPWPERSONAL      GPWCOMM         CASH 
# 0.0008504658 0.4928909559 0.2157188163 0.0846478138 0.1078262484 0.0128386594 
#In absence of above variable, we can say that EXPENSES will be 0.00085 million dollars
#with every million dollors longloss there will be .4928 increase in expenses, keeping
#other properties constant, similarly can be said about other regressors.
#3. let us suppose that we have one situation where
#    
# Intercept	  LONGLOSS    SHORTLOSS   GPWPERSONAL GPWCOMM     CASH	   EXPENSES
# 0.000850466	0.492890956	0.215718816	0.084647814	0.107826248	0.012838659	
# Values1     0.001	      0.001	      0.001	      0.001	      0.1	    #0.003035416
# Values2     0.002	      0.002	      0.002	      0.002	      0.2	          #0.005220365
#      SO here we see that EXPENSES has increased, so we have to control the costs
# associated with these regressors to minimize the Expenses, and majorly we need to
# minimize cost involved in lONGLOSS and SHORTLOSS as they are the major contributor in the model.
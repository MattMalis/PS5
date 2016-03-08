getwd()
setwd("/Users/iramalis/Desktop/gitstuff/PS5")

rm(list=ls())



############### READING IN AND RE-CODING DATA ##################



library(foreign)
data<-read.dta("anes_timeseries_2012_stata12.dta")

dim(data)

search()

## investigating variables of interest...
table(data$interest_attention)
table(data$interest_following)
table(data$gender_respondent_x)
table(data$candrel_dpc)
table(data$relig_import)
head(data$relig_import)
head(relig_import)
summary(data$ft_dpc)

attach(data)
anes<-NULL
## storing variables in new data.frame
anes<-cbind(as.character(caseid), as.character(ft_dpc), as.character(interest_attention), 
            as.character(interest_following), as.character(gender_respondent_x), 
            as.character(candrel_dpc), as.character(relig_church), 
            as.character(relig_churchoft), as.character(ineq_incgap))
anes<-as.data.frame(anes)
detach(data)

##casting variables as numeric or character
anes[,c(1,2)]<-apply(anes[,c(1,2)], 2, function(x){
  x<-as.numeric(as.character(x))
})
anes[,-c(1,2)]<-apply(anes[,-c(1,2)], 2, function(x){
  x<-as.character(x)
})
str(anes)


## male=1, female=0
anes$male<-ifelse(anes$V5=='1. Male', 1, 0)

## believes Obama is a Muslim = 1, otherwise 0
anes$muslimObama<-ifelse(anes$V6=="04. Muslim", 1,0)

##how often do you pay attention to what's going on in government and politcs?
##  Always = 5, Most of the time = 4, About half the time = 3, Some of the time = 2, Never = 1
anes$attention<-ifelse(anes$V3=="1. Always", 5,
                       ifelse(anes$V3=="2. Most of the time", 4,
                              ifelse(anes$V3=="3. About half the time", 3,
                                     ifelse(anes$V3=="4. Some of the time", 2,
                                            ifelse(anes$V3=="5. Never", 1, NA)))))

##how interested are you in political campaigns so far this year?
## Very much interested = 3, Somewhat interested = 2, Not much interested = 1
anes$following<-ifelse(anes$V4=="1. Very much interested", 3,
                       ifelse(anes$V4=="2. Somewhat interested", 2,
                              ifelse(anes$V4=="3. Not much interested", 1, NA)))

## referring to binary "relig_church" variable and ordinal "relig_churchoft" variable
##    to distinguish between "no" and non-response from the binary

## do you ever attend religious services?
##  Yes = 1, No = 2
anes$church<-ifelse(anes$V7=="1. Yes", 1, 
                    ifelse(anes$V7=="2. No", 0, NA))

## How often do you attend religious services?
##  Never = 0 (including "Never" response to this question, and "No" response to previous)
##  A few times a year = 1, Once or twice a month = 2, Almost every week = 3, Every week = 4
anes$churchFreq<-ifelse(anes$church== 0, 0,
                        ifelse(anes$V8=="5. Never", 0,
                               ifelse(anes$V8=="4. A few times a year", 1,
                                      ifelse(anes$V8=="3. Once or twice a month", 2,
                                             ifelse(anes$V8=="2. Almost every week", 3,
                                                    ifelse(anes$V8=="1. Every week", 4, NA))))))

## Do you believe income gap, compared to 20 years ago, is:
##  Larger = 3, About the same = 2, Smaller = 1
anes$incomeGapLarger<- ifelse(anes$V9=="1. Larger", 3,
                              ifelse(anes$V9=="3. About the same", 2,
                                     ifelse(anes$V9=="2. Smaller", 1, NA)))

## removing uncoded variables, re-naming the first two
names(anes)
anes<-anes[,-c(3:9)]
names(anes)[1:2]<-c("caseID", "thermObama")

str(anes)

## handling NA values for thermObama:
anes$thermObama<-sapply(anes$thermObama, function(x){ 
  ifelse(x<0, NA, x)})




########## ASSIGNING TEST AND CONTROL GROUPS ###########


## assigning random values
anes$random<-0
anes$random<-sapply(anes$random, function(x) {
  x<-runif(1)})
summary(anes$random)
## separating into test/training sets: if random > 0.5, test = 1, otherwise test = 0
anes$test<- ifelse(anes$random>0.5, 1, 0)
table(anes$test)




########## STATISTICAL MODELS ###########


## model1
model1<-lm(thermObama~churchFreq+muslimObama+attention+male+incomeGapLarger, data=anes[anes$test==0,])
summary(model1)

## model2
model2<-lm(thermObama~churchFreq+muslimObama, data=anes[anes$test==0,])
summary(model2)

# model3
model3<-lm(thermObama~following+male+incomeGapLarger, data=anes[anes$test==0,])
summary(model3)




########## PREDICTED DATA #################


predict1<-array(as.numeric(predict(model1, anes[anes$test==1,])))
summary(predict1)
summary(anes$thermObama)

predict2<-array(as.numeric(predict(model2, anes[anes$test==1,])))
summary(predict2)

predict3<-array(as.numeric(predict(model3, anes[anes$test==1,])))
summary(predict3)

predictions<-as.matrix(cbind(predict1, predict2, predict3))

observed<-anes$thermObama[anes$test==1]
length(observed)
dim(predictions)



############## FIT STATISTICS - INDIVIDUAL FUNCTIONS ####################


## funciton RMSE
## @param preds, matrix of predicted values (default = 'predictions' matrix created above)
## @param obs, matrix of observed values (default = 'observed' vector created above)
## @param model, value 1:3 denoting which model to assess fitness of (default = 1)
## returns fitness of model, calcultated by RMSE

RMSE <- function (preds=predictions, obs=observed, model=1) {
  errors<-abs(preds[,model]-obs)
  return(sqrt(mean(errors^2, na.rm=T)))
}


## funciton MAD
## @param preds, matrix of predicted values (default = 'predictions' matrix created above)
## @param obs, matrix of observed values (default = 'observed' vector created above)
## @param model, value 1:3 denoting which model to assess fitness of (default = 1)
## returns fitness of model, calcultated by MAD

MAD <- function (preds=predictions, obs=observed, model=1) {
  errors<-abs(preds[,model]-obs)
  return(median(errors, na.rm=TRUE))
}


## funciton RMSLE
## @param preds, matrix of predicted values (default = 'predictions' matrix created above)
## @param obs, matrix of observed values (default = 'observed' vector created above)
## @param model, value 1:3 denoting which model to assess fitness of (default = 1)
## returns fitness of model, calcultated by RMSLE

RMSLE <- function (preds=predictions, obs=observed, model=1) {
  hold<-log(preds[,model]+1)-log(obs+1)
  return(sqrt(mean(hold^2, na.rm=T)))
}


## funciton MAPE
## @param preds, matrix of predicted values (default = 'predictions' matrix created above)
## @param obs, matrix of observed values (default = 'observed' vector created above)
## @param model, value 1:3 denoting which model to assess fitness of (default = 1)
## returns fitness of model, calcultated by MAPE

MAPE <- function (preds=predictions, obs=observed, model=1) {
  errors<-abs(preds[,model]-obs)
  percentErrors<-errors/obs*100
  return(mean(percentErrors[percentErrors!=Inf], na.rm=TRUE))
}

#### WHAT DO WHEN OBS = 0? 

## funciton MEAPE
## @param preds, matrix of predicted values (default = 'predictions' matrix created above)
## @param obs, matrix of observed values (default = 'observed' vector created above)
## @param model, value 1:3 denoting which model to assess fitness of (default = 1)
## returns fitness of model, calcultated by MEAPE

MEAPE <- function (preds=predictions, obs=observed, model=1) {
  errors<-abs(preds[,model]-obs)
  percentErrors<-errors/obs *100
  return(median(percentErrors, na.rm=TRUE))
}
### WHAT ABOUT INFINITY VALUES HERE? (WHEN OBS = 0)? 
### not a problem cuz its median but still......





############ FIT STATISTICS - MASTER FUNCTION #############



## function fitStatistics
## @param preds, matrix of predicted values (default = 'predictions' matrix created above)
## @param obs, matrix of observed values (default = 'observed' vector created above)
## @params findRMSE, findMAD, findRMSLE, findMAPE, findMEAPE: boolean of whether to calculate these values
##      (default to TRUE for all)
## returns matrix of 


fitStatistics<-function(preds=predictions, obs=observed, 
                        findRMSE=T, findMAD=T, findRMSLE=T, findMAPE=T, findMEAPE=T){
  fitStatValues<-matrix(nrow=3)
  statNames<-(NULL)
  if (findRMSE){
    statRMSE<-sapply(1:3, function(x) RMSE(model=x))
    fitStatValues<-cbind(fitStatValues, (statRMSE))
    statNames<-c(statNames, "RMSE")
  }
  if (findMAD){
    statMAD<-sapply(1:3, function(x) MAD(model=x))
    fitStatValues<-cbind(fitStatValues, (statMAD))
    statNames<-c(statNames, "MAD")
  }
  if (findRMSLE){
    statRMSLE<-sapply(1:3, function(x) RMSLE(model=x))
    fitStatValues<-cbind(fitStatValues, (statRMSLE))
    statNames<-c(statNames, "RMSLE")
  }
  if (findMAPE){
    statMAPE<-sapply(1:3, function(x) MAPE(model=x))
    fitStatValues<-cbind(fitStatValues, (statMAPE))
    statNames<-c(statNames, "MAPE")
  }
  if (findMEAPE){
    statMEAPE<-sapply(1:3, function(x) MEAPE(model=x))
    fitStatValues<-cbind(fitStatValues, statMEAPE)
    statNames<-c(statNames, "MEAPE")
  }
  fitStatValues<-fitStatValues[,-1]
  colnames(fitStatValues)<-statNames
  rownames(fitStatValues)<-c("Model1", "Model2", "Model3")
  return(fitStatValues)
}

## RUNNING FUNCTION ON THE THREE MODELS

fitStatistics()




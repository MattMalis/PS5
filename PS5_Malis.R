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

## recoding: all "Don't Know" or "Refused" coded as NA

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

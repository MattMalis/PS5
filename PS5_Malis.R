getwd()
setwd("/Users/iramalis/Desktop/gitstuff/PS5")

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


## storing variables in new data.frame
anes<-cbind(as.character(caseid), as.character(ft_dpc), as.character(interest_attention), 
             as.character(interest_following), as.character(gender_respondent_x), 
               as.character(candrel_dpc), as.character(relig_church), as.character(relig_churchoft))
anes<-as.data.frame(anes)

##casting variables as numeric or character
anes[,c(1,2)]<-apply(anes[,c(1,2)], 2, function(x){
  x<-as.numeric(as.character(x))
})
anes[,-c(1,2)]<-apply(anes[,-c(1,2)], 2, function(x){
  x<-as.character(x)
})
str(anes)

## recoding

anes$male<-ifelse(anes$V5=='1. Male', 1, 0)

anes$muslimObama<-ifelse(anes$V6=="04. Muslim", 1,0)

anes$attention<-ifelse(anes$V3=="1. Always", 5,
                    ifelse(anes$V3=="2. Most of the time", 4,
                       ifelse(anes$V3=="3. About half the time", 3,
                            ifelse(anes$V3=="4. Some of the time", 2,
                                   ifelse(anes$V3=="5. Never", 1, NA)))))

anes$following<-ifelse(anes$V4=="1. Very much interested", 3,
                       ifelse(anes$V4=="2. Somewhat interested", 2,
                              ifelse(anes$V4=="3. Not much interested", 1, NA)))

## referring to binary "relig_church" variable and ordinal "relig_churchoft" variable
##    to distinguish between "no" and non-response from the binary

anes$church<-ifelse(anes$V7=="1. Yes", 1, 
                       ifelse(anes$V7=="2. No", 0, NA))

anes$churchFreq<-ifelse(anes$church== 0, 0,
                    ifelse(anes$V8=="5. Never", 0,
                      ifelse(anes$V8=="4. A few times a year", 1,
                        ifelse(anes$V8=="3. Once or twice a month", 2,
                          ifelse(anes$V8=="2. Almost every week", 3,
                             ifelse(anes$V8=="1. Every week", 4, NA))))))

## removing uncoded variables, re-naming the frist two
names(anes)
anes<-anes[,-c(3:8)]
names(anes)[1:2]<-c("caseID", "thermObama")

str(anes)

detach(data)
attach(anes)


## separating into test/training sets (test = 1,0)
anes$random<-anes$caseID
anes$random<-sapply(anes$random, function(x) {
  x<-runif(1)})
summary(anes$random)
summary(anes$test)

anes$test<- ifelse(anes$random>0.5, 1, 0)
      
?apply

table(anes$random)

?runif

model1<-lm(thermObama~churchFreq+muslimObama+attention+male)
summary(model1)

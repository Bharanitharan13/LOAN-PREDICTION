setwd('E:\\loan prediction')

##improting

train<-read.csv('train_ctrUa4K.csv',header = TRUE,sep = ',',na.strings = c("NA","NAN",""))
test<-read.csv('test_lAUu6dG.csv',header = TRUE,sep = ',',na.strings = c("NA","NAN",""))

####audit the data

str(train)

train$Credit_History<-as.factor(train$Credit_History)

train$Dependents<- as.character(train$Dependents)
train$Dependents<- ifelse(train$Dependents=='3+','3',train$Dependents)
train$Dependents<- as.factor(train$Dependents)
table(train$Dependents)

##imputation using Amelia##

summary(train)
str(train)
install.packages("Amelia")

library(Amelia)
names(train)

new_impute <- amelia(train,m=5,
                     idvars =c("Loan_ID","Education","ApplicantIncome","CoapplicantIncome","Property_Area","Loan_Status"),
                     noms = c("Gender","Married","Self_Employed","Credit_History"),
                     ords = c("Dependents"))

write.amelia(new_impute,file.stem = "importedfiles")


train1<- read.csv('importedfiles1.csv',na.strings = c(""))
train1$X<- NULL

summary(train1)

library(e1071)

skewness(train1$ApplicantIncome)
skewness(train1$LoanAmount)

install.packages("psych")

library(psych)
describe(train1)
library(Hmisc)
cormat<-subset(train1,select=c('ApplicantIncome','CoapplicantIncome','LoanAmount','Loan_Amount_Term'))
corellation<-rcorr(as.matrix(cormat))
cormat<-as.data.frame(corellation$r)               
cormat
train$Credit_History<-as.factor(train$Credit_History)

train1$ln_applicantincome<- log(train$ApplicantIncome)
train1$ln_applicantincome <- ifelse(train1$ln_applicantincome==-Inf,0,train1$ln_applicantincome)
skewness(train1$ln_applicantincome)
train1$CoapplicantIncome<- log(train$CoapplicantIncome)
train1$ln_coapplicantincome <- ifelse(train1$CoapplicantIncome==-Inf,0,train1$CoapplicantIncome)
skewness(train1$ln_coapplicantincome)
names(train1)

train1$Log_Loan_Amount_Term <- log(train1$Loan_Amount_Term)
skewness(train1$Log_Loan_Amount_Term)

model_ln <-glm(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area+ln_applicantincome+ln_coapplicantincome,
               family = 'binomial',data = train1)


summary(model_ln)

str(train1)

train1$new_loan_status<-model_ln$fitted.values
train1$new_loan_status<- ifelse(train1$new_loan_status>.50,'Y','N')

table(train1$Loan_Status,train1$new_loan_status)

library(Amelia)

names(test)
str(test)

test$Dependents<- as.character(test$Dependents)

test$Dependents<- ifelse(test$Dependents=="3+","3",test$Dependents)
test$Dependents<- as.factor(test$Dependents)

table(test$Dependents)


new_impute <- amelia(test,m=5,
                     idvars =c("Loan_ID","Education","ApplicantIncome","CoapplicantIncome","Property_Area"),
                     noms = c("Gender","Married","Self_Employed","Credit_History"),
                     ords = c("Dependents"))

write.amelia(new_impute,file.stem = "importedtestfiles")


test1<- read.csv('importedtestfiles1.csv',na.strings = c(""))

str(test1)
test1$Credit_History<- as.factor(test1$Credit_History)
summary(test1)
test1$X<- NULL
test1$ln_coapplicantincome<-ifelse(test1$CoapplicantIncome==0,0,log(test1$CoapplicantIncome))
test1$ln_applicantincome<-ifelse(test1$ApplicantIncome==0,0,log(test1$ApplicantIncome))

names(test1)

test1$loanstatus<- predict(model_ln,test1,type = 'response')

test1$preditedloanstatus<- ifelse(test1$loanstatus<.50,"N","Y")

submission<- c("Loan_ID","preditedloanstatus")

submit<-test1[submission]

write.csv(submit,'submission.csv',row.names = FALSE)

#######desicition tree######


install.packages("party")

library('party')

model_decisiontreeparty <-ctree(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area+ln_applicantincome+ln_coapplicantincome,data = train1)

summary(model_decisiontree)

install.packages('rpart')
library(rpart)
model_decisiontree <-rpart(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+
                             LoanAmount+Loan_Amount_Term+Credit_History+Property_Area+ln_applicantincome+ln_coapplicantincome,data = train1,
                           parms=list(split='information'))


train1$treepredic<- predict(model_decisiontree,train1,type = 'class')
train1$treepredictree<- predict(model_decisiontreeparty,train1)


table(train$Loan_Status,train$treepredic)
table(train$Loan_Status,train$treepredictree)

###randomforest#####

install.packages("randomForest")
library(randomForest)
model_randomforest <-randomForest(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+
                             LoanAmount+Loan_Amount_Term+Credit_History+Property_Area+ln_applicantincome+ln_coapplicantincome,data = train1,
                             ntree=150,na.action = na.omit)
model_randomforesttest150
model_randomforest
summary(model_randomforest)
train1$treepredicrandomforest <- predict(model_randomforest,train1,type = 'class')

table(train1$Loan_Status,train1$treepredicrandomforest)
test1$randomforest<- predict(model_randomforest,test1,type = "class")
model_randomforest
test1$decisiontree<- predict(model_decisiontree,test1,type = "class")



submission<- c("Loan_ID","decisiontree")

submit<-test1[submission]

write.csv(submit,'submissionrandomforest.csv',row.names = FALSE)

write.csv(train1,'train1.csv',row.names = FALSE)

table(train1$Loan_Status,train1$new_loan_status,train1$treepredicrandomforest,train1$treepredic)


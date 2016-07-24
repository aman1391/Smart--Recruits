setwd("./analytics vidhya/Smart")
train<-read.csv("training.csv",header=T)
test<-read.csv("test.csv",header=T)
test$Business_Sourced<--1

X_All<-rbind(train,test)
summary(X_All)

table(is.na(X_All))
#Applicant Gender
levels(X_All$Applicant_Gender)[levels(X_All$Applicant_Gender)==""]="M"

#Applicant DOB
X_All$Applicant_BirthDate<-as.character(X_All$Applicant_BirthDate)
X_All$Applicant_BirthDate<-strsplit(X_All$Applicant_BirthDate,"/")
for( i in 1:nrow(X_All)){
  X_All[i,'Applicant_BirthDate']<-X_All$Applicant_BirthDate[[i]][3]
}
X_All$Applicant_BirthDate<-as.character(X_All$Applicant_BirthDate)
X_All$Applicant_BirthDate<-as.numeric(X_All$Applicant_BirthDate)
X_All$Applicant_age<-2009-X_All$Applicant_BirthDate
X_All$Applicant_BirthDate<-NULL
boxplot(X_All$Applicant_age)
table(is.na(X_All$Applicant_age))
summary(X_All$Applicant_age)
X_All$Applicant_age[is.na(X_All$Applicant_age)]<-mean(X_All$Applicant_age,na.rm=T)
X_All$Applicant_age[X_All$Applicant_age<27]=27
X_All$Applicant_age[X_All$Applicant_age>55]=55

#Applicatant marrital status
levels(X_All$Applicant_Marital_Status)[levels(X_All$Applicant_Marital_Status) %in% c("D","W")]="S"
levels(X_All$Applicant_Marital_Status)[levels(X_All$Applicant_Marital_Status) %in% c("")]="M"

#Manager DOJ
X_All$Manager_DOJ<-as.character(X_All$Manager_DOJ)
X_All$Manager_DOJ<-strsplit(X_All$Manager_DOJ,"/")
for(i in 1:nrow(X_All)){
  X_All[i,'Manager_yr']<-30-as.numeric(X_All$Manager_DOJ[[i]][2])
}
for(i in 1:nrow(X_All)){
  X_All[i,'Manager_yr']<-X_All[i,'Manager_yr']+(11-as.numeric(X_All$Manager_DOJ[[i]][1]))*30
}
for(i in 1:nrow(X_All)){
  X_All[i,'Manager_yr']<-X_All[i,'Manager_yr']+((2009-as.numeric(X_All$Manager_DOJ[[i]][3]))-1)*365
}

X_All$Manager_DOJ<-NULL
boxplot(X_All$Manager_yr)
X_All$Manager_yr[X_All$Manager_yr>(1.5*IQR(X_All$Manager_yr,na.rm=T))]<-1.5*IQR(X_All$Manager_yr,na.rm=T)


#manager dob
X_All$Manager_DoB<-as.character(X_All$Manager_DoB)
X_All$Manager_DoB<-strsplit(X_All$Manager_DoB,"/")

for( i in 1:nrow(X_All)){
  X_All[i,'Manager_DoB']<-X_All$Manager_DoB[[i]][3]
}
X_All$Manager_DoB<-as.numeric(X_All$Manager_DoB)
X_All$Manager_age<-2009-X_All$Manager_DoB
X_All$Manager_DoB<-NULL
boxplot(X_All$Manager_age)
X_All$Manager_age[X_All$Manager_age>53]=53

#Manager joining position
table(X_All$Manager_Joining_Designation)
levels(X_All$Manager_Joining_Designation)[levels(X_All$Manager_Joining_Designation)==""]<-"Other"
levels(X_All$Manager_Joining_Designation)[levels(X_All$Manager_Joining_Designation)=="Other"]<-"Level 3"
X_All$Manager_Joining_Designation<-substr(X_All$Manager_Joining_Designation,7,8)
X_All$Manager_Joining_Designation<-as.numeric(X_All$Manager_Joining_Designation)

#Manager current designation
table(X_All$Manager_Current_Designation)
levels(X_All$Manager_Current_Designation)[levels(X_All$Manager_Current_Designation)==""]<-"Level 1"
X_All$Manager_Current_Designation<-substr(X_All$Manager_Current_Designation,7,8)
X_All$Manager_Current_Designation<-as.numeric(X_All$Manager_Current_Designation)

X_All$MAnager_growth<-X_All$Manager_Current_Designation-X_All$Manager_Joining_Designation
levels(X_All$MAnager_growth)
#X_All$MAnager_growth<-as.numeric(X_All$MAnager_growth)
X_All$Manager_Joining_Designation<-NULL
X_All$Manager_Current_Designation<-NULL

#Manger total product
X_All$Manager_total_product<-(X_All$Manager_Num_Products+X_All$Manager_Num_Products2)/2
table(X_All$Manager_total_product)
boxplot(X_All$Manager_total_product)
X_All$Manager_total_product[X_All$Manager_total_product>1.5*IQR(X_All$Manager_total_product,na.rm=T)]<-1.5*IQR(X_All$Manager_total_product,na.rm=T)

#Manager total business
X_All$Manager_total_manager<-(X_All$Manager_Business+X_All$Manager_Business2)/2
boxplot(X_All$Manager_total_manager)
summary(X_All$Manager_total_manager)
X_All$Manager_total_manager[X_All$Manager_total_manager>1.5*IQR(X_All$Manager_total_manager,na.rm = T)]<-1.5*IQR(X_All$Manager_total_manager,na.rm=T)
X_All$Manager_Business<-NULL
X_All$Manager_Num_Products<-NULL
X_All$Manager_Business2<-NULL
X_All$Manager_Num_Products2<-NULL

#MAnager grade
X_All$Manager_Grade<-as.factor(X_All$Manager_Grade)
table(X_All$Manager_Grade)

#Manager status
X_All$Manager_Status<-droplevels(X_All$Manager_Status)


#Manager gender
X_All$Manager_Gender<-droplevels(X_All$Manager_Gender)

#Applicant education
table(X_All$Applicant_Qualification)
levels(X_All$Applicant_Qualification)
levels(X_All$Applicant_Qualification)[levels(X_All$Applicant_Qualification) %in% c("Associate / Fellow of Institute of Chartered Accountans of India","Associate/Fellow of Acturial Society of India","Associate/Fellow of Institute of Company Secretories of India","Associate/Fellow of Insurance Institute of India","Associate/Fellow of Institute of Institute of Costs and Works Accountants of India")]<-"Associate"
X_All$Applicant_Qualification<-droplevels(X_All$Applicant_Qualification)
levels(X_All$Applicant_Qualification)[levels(X_All$Applicant_Qualification) %in% c("Professional Qualification in Marketing","Masters of Business Administration")]<-"MBA"
levels(X_All$Applicant_Qualification)[levels(X_All$Applicant_Qualification) %in% c("")]<-"Others"

#Applicant Occupation
table(X_All$Applicant_Occupation)
levels(X_All$Applicant_Occupation)[levels(X_All$Applicant_Occupation)==""]<-"Others2"

#PIN
X_All$Office_PIN_A<-substr(X_All$Office_PIN,1,1)
X_All$Office_PIN_B<-substr(X_All$Office_PIN,2,2)
X_All$Office_PIN_C<-substr(X_All$Office_PIN,3,3)
X_All$Applicant_City_PIN_A<-substr(X_All$Applicant_City_PIN,1,1)
X_All$Applicant_City_PIN_B<-substr(X_All$Applicant_City_PIN,2,2)
X_All$Applicant_City_PIN_C<-substr(X_All$Applicant_City_PIN,3,3)
X_All$Office_PIN_A<-as.numeric(X_All$Office_PIN_A)
X_All$Office_PIN_B<-as.numeric(X_All$Office_PIN_B)
X_All$Office_PIN_C<-as.numeric(X_All$Office_PIN_C)
X_All$Applicant_City_PIN_A<-as.numeric(X_All$Applicant_City_PIN_A)
X_All$Applicant_City_PIN_B<-as.numeric(X_All$Applicant_City_PIN_B)
X_All$Applicant_City_PIN_C<-as.numeric(X_All$Applicant_City_PIN_C)
X_All$City_A<-X_All$Applicant_City_PIN_A-X_All$Office_PIN_A
X_All$City_B<-X_All$Applicant_City_PIN_B-X_All$Office_PIN_B
X_All$City_C<-X_All$Applicant_City_PIN_C-X_All$Office_PIN_C
table((X_All$City_C))
X_All$City_A[is.na(X_All$City_A)]<-0
X_All$City_A[X_All$City_A>0|X_All$City_A<0]<-1
X_All$City_B[X_All$City_B>0|X_All$City_B<0]<-1
X_All$City_C[X_All$City_C>0|X_All$City_C<0]<-1
X_All$Office_PIN<-NULL
X_All$Office_PIN_A<-NULL
X_All$Office_PIN_B<-NULL
X_All$Office_PIN_C<-NULL
X_All$Applicant_City_PIN_A<-NULL
X_All$Applicant_City_PIN_B<-NULL
X_All$Applicant_City_PIN_C<-NULL
X_All$Applicant_City_PIN<-NULL
X_All$City_A[is.na(X_All$City_A)]=0
X_All$City_B[is.na(X_All$City_B)]=0
X_All$City_C[is.na(X_All$City_C)]=0
table(is.na(X_All$City_C))
X_All$City_A<-as.factor(X_All$City_A)
X_All$City_B<-as.factor(X_All$City_B)
X_All$City_C<-as.factor(X_All$City_C)


#APPLICANT RECEIPT YR
X_All$Application_Receipt_Date<-as.character(X_All$Application_Receipt_Date)
X_All$Applicant_Receipt_Date<-strsplit(X_All$Application_Receipt_Date,"/")
#X_All$Applicant_Receipt_Yr<-strsplit(X_All$Application_Receipt_Date,"/")
for(i in 1:nrow(X_All)){
  X_All[i,'Applicant_yr']<-30-as.numeric(X_All$Applicant_Receipt_Date[[i]][2])
}
for(i in 1:nrow(X_All)){
  X_All[i,'Applicant_yr']<-X_All[i,'Applicant_yr']+(11-as.numeric(X_All$Applicant_Receipt_Date[[i]][1]))*30
}
for(i in 1:nrow(X_All)){
  X_All[i,'Applicant_yr']<-X_All[i,'Applicant_yr']+((2009-as.numeric(X_All$Applicant_Receipt_Date[[i]][3]))-1)*365
}

X_All$Applicant_Receipt_Date<-NULL
X_All$Application_Receipt_Date<-NULL
boxplot(X_All$Applicant_yr)

boxplot(X_All)
table(is.na(X_All))
summary(X_All)
table(X_All$Manager_Grade)
X_All$Manager_Grade<-as.character(X_All$Manager_Grade)
X_All$Manager_Grade[is.na(X_All$Manager_Grade)]="other"
X_All$Manager_Grade<-as.factor(X_All$Manager_Grade)

X_All$Manager_Status<-as.character(X_All$Manager_Status)
X_All$Manager_Status[is.na(X_All$Manager_Status)]="other"
X_All$Manager_Status<-as.factor(X_All$Manager_Status)

X_All$Manager_Gender<-as.character(X_All$Manager_Gender)
X_All$Manager_Gender[is.na(X_All$Manager_Gender)]="other"
X_All$Manager_Gender<-as.factor(X_All$Manager_Gender)

X_All$Manager_Num_Application[is.na(X_All$Manager_Num_Application)]=-1
X_All$Manager_Num_Coded[is.na(X_All$Manager_Num_Coded)]=-1
X_All$Manager_yr[is.na(X_All$Manager_yr)]<-mean(X_All$Manager_yr,na.rm=T)
X_All$Manager_age[is.na(X_All$Manager_age)]<-mean(X_All$Manager_age,na.rm=T)
X_All$Manager_total_product[is.na(X_All$Manager_total_product)]=-1
X_All$Manager_total_manager[is.na(X_All$Manager_total_manager)]=-1

X_All$Manager_total_product<-(X_All$Manager_total_product-min(X_All$Manager_total_product))/(max(X_All$Manager_total_product)-min(X_All$Manager_total_product))
X_All$Manager_total_manager<-(X_All$Manager_total_manager-min(X_All$Manager_total_manager))/(max(X_All$Manager_total_manager)-min(X_All$Manager_total_manager))
X_All$Applicant_yr<-(X_All$Applicant_yr-min(X_All$Applicant_yr))/(max(X_All$Applicant_yr)-min(X_All$Applicant_yr))
X_All$Manager_yr<-(X_All$Manager_yr-min(X_All$Manager_yr))/(max(X_All$Manager_yr)-min(X_All$Manager_yr))
X_All$Applicant_age<-(X_All$Applicant_age-min(X_All$Applicant_age))/(max(X_All$Applicant_age)-min(X_All$Applicant_age))
X_All$Manager_age=(X_All$Manager_age-min(X_All$Manager_age))/(max(X_All$Manager_age)-min(X_All$Manager_age))

X_All$code=X_All$Manager_Num_Coded-X_All$Manager_Num_Application
library(dummies)
X_All_2=dummy.data.frame(X_All,names=c("Applicant_Gender","Applicant_Marital_Status","Applicant_Occupation","Applicant_Qualification","Manager_Grade","Manager_Status","Manager_Gender","City_A","City_B","City_C"))
#X_All_A<-X_All_2[,-c(1,38)]
#X_All_A=X_All_A[,-c(28,29,47,45,4,2)]
X_All_2=X_All_2[,-c(36,37)]
X_train1<-X_All_2[1:nrow(train),]
y<-X_train1[,36]
X_train1<-X_train1[,-c(1,36)]
X_test1<-X_All_2[-(1:nrow(train)),-c(1,36)]
library(xgboost)
model_xgb_cv<- xgb.cv(data=data.matrix(X_train2),label=data.matrix(y),objective="binary:logistic",nfold=5,nrounds=200,eta=0.02,max_depth=5,subsample=0.6,colsample_bytree=0.85,min_child_weight=1,eval_metric="auc")
model_xgb<- xgboost(data=data.matrix(X_train2),label=data.matrix(y),objective="binary:logistic",nrounds=200,eta=0.02,max_depth=5,subsample=0.6,colsample_bytree=0.85,min_child_weight=1,eval_metric="auc")
pred1<-predict(model_xgb,data.matrix(X_test2))

sub1<-data.frame("ID"=test$ID,"Business_Sourced"=pred1)
write.csv(sub1,"sub1.csv",row.names = F)

library(glmnet)
x=model.matrix(~.,data=X_train1)
gcv <- cv.glmnet(x=x ,
                 y = y,
                 family = 'binomial',
                 type.measure = 'auc',
                 nfolds = 4)
coeffs <- as.matrix(coef(gcv, s = 'lambda.1se'))
chosenVars1 <- rownames(coeffs)[abs(coeffs[,1]) > 0]
chosenVars1
table(is.na(X_All$City_A))
X_All_3<-X_All_2[,c(3,6,8,10,11,12,17,21,24,25,26,27,31,32,34,36,37,39,41,44,48,51)]
X_train2<-X_All_3[1:nrow(train),]
X_test2<-X_All_3[-(1:nrow(train)),]


preds <- vector("list", length = 100)

for(i in 1:100){
  print(paste('training model:', i))
  model <- xgboost(data=data.matrix(X_train2),label=data.matrix(y),objective="binary:logistic",nrounds=200,eta=0.02,max_depth=5,subsample=0.6,colsample_bytree=0.85,min_child_weight=1,eval_metric="auc")
  
  print(paste('applying prediction:', i))
  preds[[i]] <- predict(model, newdata =data.matrix(X_test2))
}
com_preds=colMeans(do.call(rbind,preds))


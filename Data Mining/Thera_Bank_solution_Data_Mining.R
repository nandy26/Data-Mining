#Bank personal loan modeling
rm(list=ls())
setwd("D:/BABI/Data Mining/Mini_Project")
library(readxl)
Loan_Dataset=read_excel("Thera Bank_Personal_Loan.xlsx",2)


#colnames(Loan_Dataset_Clustering)=make.names(colnames(Loan_Dataset_Clustering))

#Converting Negative working experience into zero,to make it meaningful
for ( i in  1 : nrow(Loan_Dataset))
{
  
  if (Loan_Dataset$`Experience (in years)`[i]<0)
  {
    Loan_Dataset$`Experience (in years)`[i] =0
  }
}

#####################Exploratory data analysis###################

#Uni-variate analysis-Summary Satatistics Measure of central tendency 
summary(Loan_Dataset)
##Frequency distribution for categorical variable (Univariate Analysis)
table(Loan_Dataset[,c(10)])
library(ggplot2)
hist(Loan_Dataset$`Age (in years)`,col="BLUE")
qplot(`Age (in years)`, data = Loan_Dataset, geom = "histogram",fill = `Personal Loan`,ylab = "Frequency")
boxplot(Loan_Dataset$`Age (in years)`,col="BLUE")#no outliers
hist(Loan_Dataset$`Experience (in years)`,col="BLUE",main="Customers Working Experience")
boxplot(Loan_Dataset$`Experience (in years)`,col="BLUE",main="Customers Working Experience")#no outliers

hist(Loan_Dataset$`Income (in K/month)`,col="BLUE",main="Customers Income")
boxplot(Loan_Dataset$`Income (in K/month)`,col="BLUE",ylab="Income",main="Customers Income")

hist(Loan_Dataset$CCAvg,col="BLUE",main = "Credit card average")
boxplot(Loan_Dataset$CCAvg,col="BLUE",ylab="Credit card average")

hist(Loan_Dataset$Mortgage,col="BLUE",main="Mortgage")
boxplot(Loan_Dataset$Mortgage,col="BLUE",ylab="Mortgage")
#Bi-variate analysis
boxplot(Loan_Dataset$`Income (in K/month)`~ Loan_Dataset$`Personal Loan`,xlab = "Personal loan", ylab = "Income",col="blue",main="Response rate based on income")
boxplot(Loan_Dataset$`Experience (in years)`~ Loan_Dataset$`Personal Loan`,xlab = "Personal loan", ylab = "Working Experience",col="blue",main="Response rate based on experience")
##correlation between continous variables (Bivariate Analysis)
Cor_Matrix=round(cor(Loan_Dataset[,c(2,3,4,7,8,9,11,12,13,14)]),2)
library(corrplot)
corrplot.mixed(Cor_Matrix, lower.col ="black", number.cex = .6)

##################Hierarcial clustering#################
Loan_Dataset_hierarchy=Loan_Dataset
str(Loan_Dataset_hierarchy)
View(Loan_Dataset_hierarchy)
#Every columns are in different units ,so we need to scale linearly all the columns
Scaled_Matrix=scale(Loan_Dataset_hierarchy[,-c(1,10)])
print(Scaled_Matrix,digits = 2)
#To check whether the data are linearly scaled or not
apply(Scaled_Matrix,2,mean)
apply(Scaled_Matrix,2,sd)
Distance_Matrix=dist(Scaled_Matrix,method="minkowski",p=2)
print(Distance_Matrix)
cluster=hclust(Distance_Matrix,method="average")
cluster
plot(cluster,labels=as.character("0","1"))
plot(cluster)
plot(cluster,labels=as.character(Loan_Dataset_hierarchy[,2]))
rect.hclust(cluster,k=3,border = "Red")
Loan_Dataset_hierarchy$cluster=cutree(cluster,k=4)
Loan_Dataset_hierarchy
aggregate(Loan_Dataset_hierarchy[,-c(1,2,8)],list(Loan_Dataset_hierarchy$cluster),FUN ="mean")



##################K-means clustering##########################

Loan_Dataset_Clustering=Loan_Dataset
Loan_Dataset_Clustering=na.omit(Loan_Dataset_Clustering)
str(Loan_Dataset_Clustering)
scaled_Matrix=scale(Loan_Dataset_Clustering[,c(2,3,4,5,7,9)])
scaled_Matrix$`Family members`=Loan_Dataset_Clustering$`Family members`
scaled_Matrix$Education=Loan_Dataset_Clustering$Education
scaled_Matrix$`Personal Loan`=Loan_Dataset_Clustering$`Personal Loan`
scaled_Matrix$`Securities Account`=Loan_Dataset_Clustering$`Securities Account`
scaled_Matrix$`CD Account`=Loan_Dataset_Clustering$`CD Account`
scaled_Matrix$Online=Loan_Dataset_Clustering$Online
scaled_Matrix$CreditCard=Loan_Dataset_Clustering$CreditCard
head(scaled_Matrix)
cluster_data=kmeans(scaled_Matrix,centers = 2,nstart = 5)
cluster_data$cluster
cluster_data$totss
cluster_data$size
cluster_data$centers
cluster_data$withinss
cluster_data$tot.withinss
cluster_data$betweenss
tot=rep(1,5)
for (i in 1:5)
{
  set.seed(100)
  cluster_data_new=kmeans(scaled_Matrix,centers = i,nstart = 5)
  tot[i]=cluster_data_new$tot.withinss
  print(cluster_data_new$tot.withinss)
}
library(cluster)
plot(tot,type='b')
#by the tradition method(Elbow),k value is 3
clusplot(scaled_Matrix,cluster_data$cluster,color =TRUE,shade =TRUE,label=2,lines=1)
library(NbClust)
Nb_cluster=NbClust(scaled_Matrix,min.nc = 2,max.nc = 5,method = "kmeans")
cluster_data=kmeans(scaled_Matrix,centers = 4,nstart = 5)

nrow(Loan_Dataset_Clustering)
Loan_Dataset_Clustering$cluster=cluster_data$cluster
Aggregate_result=aggregate(Loan_Dataset_Clustering[,-c(1,10)],list(Loan_Dataset_Clustering$cluster),FUN="mean")
Aggregate_result

summary(Loan_Dataset_Clustering)
###################CART model#######################


Loan_Dataset_Cart=na.omit(Loan_Dataset)
library(caTools)
sample=sample.split(Loan_Dataset_Cart$`Personal Loan`,SplitRatio = 0.7)
Loan_Dataset_Cart_train=subset(Loan_Dataset_Cart,sample==TRUE)
nrow(Loan_Dataset_Cart_train)
Loan_Dataset_Cart_test=subset(Loan_Dataset_Cart,sample==FALSE)
colnames(Loan_Dataset_Cart_train)=make.names(colnames(Loan_Dataset_Cart_train))
Loan_Dataset_Cart_train$Personal.Loan=as.factor(Loan_Dataset_Cart_train$Personal.Loan)
dim(Loan_Dataset_Cart_train)
dim(Loan_Dataset_Cart_test)
library("rpart")
library("rpart.plot")
set.seed(1000)
CART_Model=rpart(formula=Loan_Dataset_Cart_train$Personal.Loan~.,data=Loan_Dataset_Cart_train[,-c(1,5)],method = "class",cp=0,minbucket=3)
rpart.plot(CART_Model)
CART_Model
printcp(CART_Model)
plotcp(CART_Model)
Pruned_Model=prune(CART_Model,cp=0.14,"CP")
printcp(Pruned_Model)
plotcp(Pruned_Model)
rpart.plot(Pruned_Model)

path.rpart(Pruned_Model,c(5))#To get the path of tree w.r.t node number

#################Model performance on CART training dataset################

#1)scoring
Loan_Dataset_Cart_train$Predicted_result=predict(Pruned_Model,data=Loan_Dataset_Cart_train[,-c(1,5)],type="class")
Loan_Dataset_Cart_train$Probabilty=predict(Pruned_Model,data=Loan_Dataset_Cart_train[,-c(1,5)],type ="prob")
head(Loan_Dataset_Cart_train)
sum(Loan_Dataset_Cart_train$`Personal Loan`=="0")
sum(Loan_Dataset_Cart_train$Predicted_result=="0")

sum(Loan_Dataset_Cart_train$`Personal Loan`=="1")
sum(Loan_Dataset_Cart_train$Predicted_result=="1")

table(Loan_Dataset_Cart_train$Personal.Loan,Loan_Dataset_Cart_train$Predicted_result)
#2)Confusion matrix
Table_cart_train=table(Loan_Dataset_Cart_train$Personal.Loan,Loan_Dataset_Cart_train$Predicted_result)
Accuracy_cart_train=(Table_cart_train[1,1]+Table_cart_train[2,2])/nrow(Loan_Dataset_Cart_train)
Error_cart_train=round((1-Accuracy_cart_train),4)
#Sensitivity/Recall/True positive rate
Sensitivity_cart_train=(Table_cart_train[2,2])/(Table_cart_train[2,1]+Table_cart_train[2,2])
#Specificity /True Negative Rate
Specificity_cart_train=(Table_cart_train[1,1])/(Table_cart_train[1,2]+Table_cart_train[1,1])

#3)Deciling code-Rank ordering
qs_cart_train=quantile(Loan_Dataset_Cart_train$Probabilty,prob = seq(0,1,length=11))
print(qs_cart_train)
print(qs_cart_train[10])
threshold=qs_cart_train[10]
mean((Loan_Dataset_Cart_train$Personal.Loan[Loan_Dataset_Cart_train$Probabilty>threshold])=="1")
Loan_Dataset_Cart_train$Deciles=cut(Loan_Dataset_Cart_train$Probabilty,unique(qs_cart_train),include.lowest = TRUE,right = FALSE)
head(Loan_Dataset_Cart_train)
print(Loan_Dataset_Cart_train$Deciles)
#Rank ordering
library(data.table)
#Loan_Dataset_Cart_train$Personal.Loan=as.numeric(Loan_Dataset_Cart_train$Personal.Loan)
DT_cart_train=data.table(Loan_Dataset_Cart_train)
#Aggregate columns

Rtable_cart_train=DT_cart_train[,list(cnt=length(Personal.Loan),
                                  cnt_tar1 = sum(Personal.Loan==1), 
                                  cnt_tar0 = sum(Personal.Loan==0)),by=Deciles][order(-Deciles)]
print(Rtable_cart_train)
Rtable_cart_train$rrate = round(Rtable_cart_train$cnt_tar1 / Rtable_cart_train$cnt,4)*100;
Rtable_cart_train$cum_resp = cumsum(Rtable_cart_train$cnt_tar1)
Rtable_cart_train$cum_non_resp = cumsum(Rtable_cart_train$cnt_tar0)
Rtable_cart_train$cum_rel_resp = round(Rtable_cart_train$cum_resp / sum(Rtable_cart_train$cnt_tar1),4)*100;
Rtable_cart_train$cum_rel_non_resp = round(Rtable_cart_train$cum_non_resp / sum(Rtable_cart_train$cnt_tar0),4)*100;
Rtable_cart_train$ks = abs(Rtable_cart_train$cum_rel_resp - Rtable_cart_train$cum_rel_non_resp);

print(Rtable_cart_train)

#4)ROC curve(FPR Vs TPR)
#ROCR and ineq packages to compute AUC, KS and gini
library(ROCR)
library(ineq)
ROC_cart_train=prediction(Loan_Dataset_Cart_train$Probablity,Loan_Dataset_Cart_train$Prediction)
perf_cart_train=performance(ROC_cart_train,"tpr","fpr")
plot(perf_cart_train,main="TPR Vs FPR")
KS_cart_train = max(perf_cart_train@y.values[[1]]-perf_cart_train@x.values[[1]])
auc_cart_train = performance(ROC_cart_train,"auc"); 
auc_cart_train = as.numeric(auc_cart_train@y.values)

gini_cart_train = ineq(Loan_Dataset_Cart_train$Probablity, type="Gini")

# 5)Concordance Function
library(InformationValue)
Concordance(actuals=Loan_Dataset_Cart_train$Personal.Loan, predictedScores=Loan_Dataset_Cart_train$Probablity)


#################Model performance on CART Testing dataset################
# 1)Scoring
Loan_Dataset_Cart_test$Predicted_result=predict(Pruned_Model,newdata=Loan_Dataset_Cart_test[,-c(1,5)],type="class")
Loan_Dataset_Cart_test$Probabilty=predict(Pruned_Model,newdata=Loan_Dataset_Cart_test[,-c(1,5)],type ="prob")
Accuracy_CART_train=round((3157+310)/nrow(Loan_Dataset_Cart_train),4)
Accuracy_CART_test=round((1352+125)/nrow(Loan_Dataset_Cart_test),4)
#2)Confusion matrix
Table_cart_test=table(Loan_Dataset_Cart_test$Personal.Loan,Loan_Dataset_Cart_test$Prediction)
Accuracy_cart_test=(Table_cart_test[1,1]+Table_cart_test[2,2])/nrow(Loan_Dataset_Cart_test)
Error_cart_train=round((1-Accuracy_cart_test),4)
#Sensitivity/Recall/True positive rate
Sensitivity_cart_train=(Table_cart_test[2,2])/(Table_cart_test[2,1]+Table_cart_test[2,2])
#Specificity /True Negative Rate
Specificity_cart_train=(Table_cart_test[1,1])/(Table_cart_test[1,2]+Table_cart_test[1,1])

#3)Deciling code-Rank ordering
qs_cart_test=quantile(Loan_Dataset_Cart_test$Probablity,prob = seq(0,1,length=11))
print(qs_cart_test)
print(qs_cart_test[10])
threshold=qs_cart_test[10]
mean((Loan_Dataset_Cart_test$Personal.Loan[Loan_Dataset_Cart_test$Probablity>threshold])=="1")
Loan_Dataset_Cart_test$Deciles=cut(Loan_Dataset_Cart_test$Probablity,unique(qs_cart_test),include.lowest = TRUE,right = FALSE)
head(Loan_Dataset_Cart_test)
print(Loan_Dataset_Cart_test$Deciles)
#Rank ordering
library(data.table)
#Loan_Dataset_Cart_test$Personal.Loan=as.numeric(Loan_Dataset_Cart_test$Personal.Loan)
DT_cart_test=data.table(Loan_Dataset_Cart_test)
#Aggregate columns

Rtable_cart_test=DT_cart_test[,list(cnt=length(Personal.Loan),
                                      cnt_tar1 = sum(Personal.Loan==1), 
                                      cnt_tar0 = sum(Personal.Loan==0)),by=Deciles][order(-Deciles)]
print(Rtable_cart_test)
Rtable_cart_test$rrate = round(Rtable_cart_test$cnt_tar1 / Rtable_cart_test$cnt,4)*100;
Rtable_cart_test$cum_resp = cumsum(Rtable_cart_test$cnt_tar1)
Rtable_cart_test$cum_non_resp = cumsum(Rtable_cart_test$cnt_tar0)
Rtable_cart_test$cum_rel_resp = round(Rtable_cart_test$cum_resp / sum(Rtable_cart_test$cnt_tar1),4)*100;
Rtable_cart_test$cum_rel_non_resp = round(Rtable_cart_test$cum_non_resp / sum(Rtable_cart_test$cnt_tar0),4)*100;
Rtable_cart_test$ks = abs(Rtable_cart_test$cum_rel_resp - Rtable_cart_test$cum_rel_non_resp);

print(Rtable_cart_test)

#4)ROC curve(FPR Vs TPR)
#ROCR and ineq packages to compute AUC, KS and gini
library(ROCR)
library(ineq)
ROC_cart_test=prediction(Loan_Dataset_Cart_test$Probablity,Loan_Dataset_Cart_test$Prediction)
perf_cart_test=performance(ROC_cart_test,"tpr","fpr")
plot(perf_cart_test,main="TPR Vs FPR")
KS_cart_test = max(perf_cart_test@y.values[[1]]-perf_cart_test@x.values[[1]])
auc_cart_test = performance(ROC_cart_test,"auc"); 
auc_cart_test = as.numeric(auc_cart_test@y.values)

gini_cart_test = ineq(Loan_Dataset_Cart_test$Probablity, type="Gini")

# 5)Concordance Function
library(InformationValue)
Concordance(actuals=Loan_Dataset_Cart_test$Personal.Loan, predictedScores=Loan_Dataset_Cart_test$Probablity)



#######################Random Forest#####################

library(randomForest)
Loan_Dataset_RF=na.omit(Loan_Dataset)
samples=sample.split(Loan_Dataset_RF$`Personal Loan`,SplitRatio = 0.7)
Loan_Dataset_RF_train=subset(Loan_Dataset_RF,samples=="TRUE")
Loan_Dataset_RF_test=subset(Loan_Dataset_RF,samples=="FALSE")
set.seed(1234)
Loan_Dataset_RF_train$`Personal Loan`=as.factor(Loan_Dataset_RF_train$`Personal Loan`)
colnames(Loan_Dataset_RF_train)=make.names(colnames(Loan_Dataset_RF_train))
str(Loan_Dataset_RF_train)
#Response rate
sum(Loan_Dataset_RF_train$Personal.Loan==1)/nrow(Loan_Dataset_RF_train)
##Build the first RF model
RF_Model=randomForest(Personal.Loan~.,data=Loan_Dataset_RF_train[,-c(1,5)],mtry=5,ntree=501,nodesize=10,method="class",importance=TRUE)
print(RF_Model)
##Plot the RF to know the optimum number of trees
#Out of Bag estimate errror
plot(RF_Model,main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3) 
title(main="Error Rates Random Forest - Training data")
RF_Model$err.rate
#It is observed that as the number of tress increases, the OOB error rate starts decreasing 
#till it reaches around 23rd tree with OOB = 0.014 (the minimum value). 
#After this, the OOB doesn't decrease further and remains largely steady. Hence, the optimal number of trees would be 23.

##Identify the importance of the variables
importance(RF_Model)
##Tune up the RF model to find out the best mtry
RF_Tuned=tuneRF(Loan_Dataset_RF_train[,-c(1,10)],Loan_Dataset_RF_train$Personal.Loan,stepFactor = 2,improve = 0.0001,ntreeTry = 23)
##Build the refined RF model
Tuned_RF_Model=randomForest(Personal.Loan~.,data=Loan_Dataset_RF_train[,-c(1,5)],mtry=6,ntree=51,nodesize=10,method="class",importance=TRUE)

######################RF Model performance-Training dataset#####################


#1)Scoring
Loan_Dataset_RF_train$Prediction=predict(Tuned_RF_Model,data=Loan_Dataset_RF_train[,-c(1,5)],type = "class")
Loan_Dataset_RF_train$Probablity=predict(Tuned_RF_Model,data=Loan_Dataset_RF_train[,-c(1,5)],type = "prob")[,"1"]
#2)Confusion matrix
Table_RF_train=table(Loan_Dataset_RF_train$Personal.Loan,Loan_Dataset_RF_train$Prediction)
Accuracy_RF_train=(Table_RF_train[1,1]+Table_RF_train[2,2])/nrow(Loan_Dataset_RF_train)
Error_RF_train=round((1-Accuracy_RF_train),4)
#Sensitivity/Recall/True positive rate
Sensitivity_RF_train=(Table_RF_train[2,2])/(Table_RF_train[2,1]+Table_RF_train[2,2])
#Specificity /True Negative Rate
Specificity_RF_train=(Table_RF_train[1,1])/(Table_RF_train[1,2]+Table_RF_train[1,1])

#3)Deciling code-Rank ordering
qs=quantile(Loan_Dataset_RF_train$Probablity,prob = seq(0,1,length=11))
print(qs)
print(qs[10])
threshold=qs[10]
mean((Loan_Dataset_RF_train$Personal.Loan[Loan_Dataset_RF_train$Probablity>threshold])=="1")
Loan_Dataset_RF_train$Deciles=cut(Loan_Dataset_RF_train$Probablity,unique(qs),include.lowest = TRUE,right = FALSE)
head(Loan_Dataset_RF_train)
print(Loan_Dataset_RF_train$Deciles)
#Rank ordering
library(data.table)
#Loan_Dataset_RF_train$Personal.Loan=as.numeric(Loan_Dataset_RF_train$Personal.Loan)
DT_RF_train=data.table(Loan_Dataset_RF_train)
#Aggregate columns

Rtable_RF_train=DT_RF_train[,list(cnt=length(Personal.Loan),
                                  cnt_tar1 = sum(Personal.Loan==1), 
                                  cnt_tar0 = sum(Personal.Loan==0)),by=Deciles][order(-Deciles)]
print(Rtable_RF_train)
Rtable_RF_train$rrate = round(Rtable_RF_train$cnt_tar1 / Rtable_RF_train$cnt,4)*100;
Rtable_RF_train$cum_resp = cumsum(Rtable_RF_train$cnt_tar1)
Rtable_RF_train$cum_non_resp = cumsum(Rtable_RF_train$cnt_tar0)
Rtable_RF_train$cum_rel_resp = round(Rtable_RF_train$cum_resp / sum(Rtable_RF_train$cnt_tar1),4)*100;
Rtable_RF_train$cum_rel_non_resp = round(Rtable_RF_train$cum_non_resp / sum(Rtable_RF_train$cnt_tar0),4)*100;
Rtable_RF_train$ks = abs(Rtable_RF_train$cum_rel_resp - Rtable_RF_train$cum_rel_non_resp);

print(Rtable_RF_train)

#4)ROC curve(FPR Vs TPR)
#ROCR and ineq packages to compute AUC, KS and gini
library(ROCR)
library(ineq)
ROC_RF_train=prediction(Loan_Dataset_RF_train$Probablity,Loan_Dataset_RF_train$Prediction)
perf_RF_train=performance(ROC_RF_train,"tpr","fpr")
plot(perf_RF_train,main="TPR Vs FPR")
KS_RF_train = max(perf_RF_train@y.values[[1]]-perf_RF_train@x.values[[1]])
auc_RF_train = performance(ROC_RF_train,"auc"); 
auc_RF_train = as.numeric(auc_RF_train@y.values)

gini = ineq(Loan_Dataset_RF_train$Probablity, type="Gini")

# 5)Concordance Function
library(InformationValue)
Concordance(actuals=Loan_Dataset_RF_train$Personal.Loan, predictedScores=Loan_Dataset_RF_train$Probablity)

######################RF Model performance-Testing dataset#####################


#1)Scoring
colnames(Loan_Dataset_RF_test)=make.names(colnames(Loan_Dataset_RF_test))
Loan_Dataset_RF_test$Prediction=predict(Tuned_RF_Model,newdata=Loan_Dataset_RF_test[,-c(1,5)],type = "class")
Loan_Dataset_RF_test$Probablity=predict(Tuned_RF_Model,newdata=Loan_Dataset_RF_test[,-c(1,5)],type = "prob")[,"1"]
#2)Confusion matrix
Table_RF_test=table(Loan_Dataset_RF_test$Personal.Loan,Loan_Dataset_RF_test$Prediction)
Accuracy_RF_test=(Table_RF_test[1,1]+Table_RF_test[2,2])/nrow(Loan_Dataset_RF_test)
Error_RF_test=round((1-Accuracy_RF_test),4)
#Sensitivity/Recall/True positive rate
Sensitivity_RF_test=(Table_RF_test[2,2])/(Table_RF_test[2,1]+Table_RF_test[2,2])
#Specificity /True Negative Rate
Specificity_RF_test=(Table_RF_test[1,1])/(Table_RF_test[1,2]+Table_RF_test[1,1])
#3)Deciling code-Rank ordering
qs_RF_test=quantile(Loan_Dataset_RF_train$Probablity,prob = seq(0,1,length=11))
print(qs_RF_test)
print(qs_RF_test[10])
threshold=qs_RF_test[10]
mean((Loan_Dataset_RF_test$Personal.Loan[Loan_Dataset_RF_test$Probablity>threshold])=="1")
Loan_Dataset_RF_test$Deciles=cut(Loan_Dataset_RF_test$Probablity,unique(qs),include.lowest = TRUE,right = FALSE)
head(Loan_Dataset_RF_test)
print(Loan_Dataset_RF_test$Deciles)
#Rank ordering
library(data.table)
#Loan_Dataset_RF_test$Personal.Loan=as.numeric(Loan_Dataset_RF_test$Personal.Loan)
DT_RF_train=data.table(Loan_Dataset_RF_test)
#Aggregate columns

Rtable_RF_train=DT_RF_train[,list(cnt=length(Personal.Loan),
                                  cnt_tar1 = sum(Personal.Loan==1), 
                                  cnt_tar0 = sum(Personal.Loan==0)),by=Deciles][order(-Deciles)]
print(Rtable_RF_train)
Rtable_RF_train$rrate = round(Rtable_RF_train$cnt_tar1 / Rtable_RF_train$cnt,4)*100;
Rtable_RF_train$cum_resp = cumsum(Rtable_RF_train$cnt_tar1)
Rtable_RF_train$cum_non_resp = cumsum(Rtable_RF_train$cnt_tar0)
Rtable_RF_train$cum_rel_resp = round(Rtable_RF_train$cum_resp / sum(Rtable_RF_train$cnt_tar1),4)*100;
Rtable_RF_train$cum_rel_non_resp = round(Rtable_RF_train$cum_non_resp / sum(Rtable_RF_train$cnt_tar0),4)*100;
Rtable_RF_train$ks = abs(Rtable_RF_train$cum_rel_resp - Rtable_RF_train$cum_rel_non_resp);

print(Rtable_RF_train)

#4)ROC curve(FPR Vs TPR)
#ROCR and ineq packages to compute AUC, KS and gini
library(ROCR)
library(ineq)
ROC_RF_test=prediction(Loan_Dataset_RF_test$Probablity,Loan_Dataset_RF_test$Prediction)
perf_RF_test=performance(ROC_RF_test,"tpr","fpr")
plot(perf_RF_test,main="TPR Vs FPR- RF Testing dataset")
KS_RF_test = max(perf_RF_test@y.values[[1]]-perf_RF_test@x.values[[1]])
auc_RF_test = performance(ROC_RF_test,"auc"); 
auc_RF_test = as.numeric(auc_RF_test@y.values)

gini_RF_test = ineq(Loan_Dataset_RF_test$Probablity, type="Gini")

# 5)Concordance Function
library(InformationValue)
Concordance(actuals=Loan_Dataset_RF_test$Personal.Loan, predictedScores=Loan_Dataset_RF_test$Probablity)
                                                      




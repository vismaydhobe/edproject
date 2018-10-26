rm(list=ls())
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"usdm","class")
lapply(x, require, character.only = TRUE)

df_empabs=read.csv("Absenteeism_at_work_Project.xls")
sum(is.na(df_empabs))

#first remove the observations for which target variable is null
df_empabs = df_empabs[which(!df_empabs$Absenteeism.time.in.hours %in% NA),]
dim(df_empabs)


#data preprocessing
df_empabs$Work.load.Average.day=as.numeric(df_empabs$Work.load.Average.day)
#Change Reason code 0 to 20
df_empabs$Reason.for.absence[which(df_empabs$Reason.for.absence %in% 0)] = 20
#for month = 0 , make it 12
df_empabs$Month.of.absence[which(df_empabs$Month.of.absence %in% 0)] = 12

#missing value analysis
df_empabs$Work.load.Average.day[is.na(df_empabs$Work.load.Average.daye)] = median(df_empabs$Work.load.Average.day, na.rm = T)
df_empabs$Month.of.absence[is.na(df_empabs$Month.of.absencee)] = median(df_empabs$Month.of.absence, na.rm = T)
df_empabs$Reason.for.absence[is.na(df_empabs$Reason.for.absence)] = median(df_empabs$Reason.for.absence, na.rm = T)

#other variables missing values imputation
df_empabs=knnImputation(df_empabs,k=3)


# df=subset(df_empabs,select= c(Transportation.expense,Distance.from.Residence.to.Work,Service.time,Work.load.Average.day,Hit.target,
 #                              Weight,Height,Body.mass.index))

#outlier analysis
multi.hist(df, main = NA, dcol = c("blue", "red"),
            dlty = c("solid", "solid"), bcol = "grey95")

#boxplot analysis
numeric_var=c("Transportation.expense","Distance.from.Residence.to.Work","Service.time","Age","Work.load.Average.day","Hit.target",
              "Son","Pet","Weight","Height","Body.mass.index")


for (i in 1:length(numeric_var))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (numeric_var[i]), x = "Absenteeism.time.in.hours"), data = df_empabs)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "skyblue" ,outlier.shape=20,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=numeric_var[i],x="Absenteeism time")+
           ggtitle(paste("Box plot of Absenteeism time for",numeric_var[i])))
}

#gn1

#boxplot analysis
boxplot.stats(df_empabs$Transportation.expense)$out
val = df_empabs$Transportation.expense[df_empabs$Transportation.expense %in% boxplot.stats(df_empabs$Transportation.expense)$out]
df_empabs$Transportation.expense[df_empabs$Transportation.expense %in% val] = mean(df_empabs$Transportation.expense, na.rm = T)
boxplot.stats(df_empabs$Hit.target)$out
val = df_empabs$Hit.target[df_empabs$Hit.target %in% boxplot.stats(df_empabs$Hit.target)$out]
df_empabs$Hit.target[df_empabs$Hit.target %in% val] = mean(df_empabs$Hit.target, na.rm = T)
boxplot.stats(df_empabs$Service.time)$out
val = df_empabs$Service.time[df_empabs$Service.time %in% boxplot.stats(df_empabs$Service.time)$out]
df_empabs$Service.time[df_empabs$Service.time %in% val] = mean(df_empabs$Service.time, na.rm = T)
boxplot.stats(df_empabs$Age)$out
val = df_empabs$Age[df_empabs$Age %in% boxplot.stats(df_empabs$Age)$out]
df_empabs$Age[df_empabs$Age %in% val] = mean(df_empabs$Age, na.rm = T)
boxplot.stats(df_empabs$Work.load.Average.day)$out
val = df_empabs$Work.load.Average.day[df_empabs$Work.load.Average.day %in% boxplot.stats(df_empabs$Work.load.Average.day)$out]
df_empabs$Work.load.Average.day[df_empabs$Work.load.Average.day %in% val] = mean(df_empabs$Work.load.Average.day, na.rm = T)


#Feature selection
numeric_index=c("Transportation.expense","Distance.from.Residence.to.Work","Service.time","Age","Work.load.Average.day","Hit.target","Son","Pet","Weight","Height","Body.mass.index")


corrgram(df_empabs[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot") 

## Chi-squared Test of Independence
factor_index = c("ID","Reason.for.absence", "Month.of.absence" ,"Day.of.the.week","Seasons","Disciplinary.failure","Education","Social.drinker","Social.smoker")
factor_data = df_empabs[,factor_index]

for (i in 1:9)
{
  print(names(factor_data)[i])
  print(chisq.test(table(df_empabs$Absenteeism.time.in.hours,factor_data[,i]),simulate.p.value = TRUE))
}

df_empabs = subset(df_empabs,select = -c(Age,Son,Pet,Weight,Height,Education,Social.smoker))

#feature scaling

#Normalisation
cnames = c("Transportation.expense" ,"Distance.from.Residence.to.Work","Service.time","Work.load.Average.day","Hit.target","Body.mass.index" )

for(i in cnames){
  print(i)
  df_empabs[,i] = (df_empabs[,i] - min(df_empabs[,i]))/
    (max(df_empabs[,i] - min(df_empabs[,i])))
}

#sampling
train_index = sample(1:nrow(df_empabs), 0.8 * nrow(df_empabs))
train = df_empabs[ train_index,]
test  = df_empabs[-train_index,]

#Linear Regression
vif(df_empabs[,-14])
lm_model = lm(Absenteeism.time.in.hours ~., data = train)
summary(lm_model)
predictions_LR = predict(lm_model, test[,1:13])

#MAE
MAE = function(y, yhat){
  mean(abs((y - yhat)))
}
#Calculate MAE
MAE(test[,14], predictions_LR)

#KNN regressor model
k= knn(train[,1:13],test[,1:13],train$Absenteeism.time.in.hours, k=3)
MAE(test[,14], as.numeric(k))

library("randomForest")
RF_model = randomForest(Absenteeism.time.in.hours ~ ., train, importance = TRUE)
importance(RF_model)
predictions_RF = predict(RF_model, test[,1:13])
MAE(test[,14], predictions_RF)


#Predict for problem statement
predictions_LR = predict(lm_model, df_empabs[,1:13])
df_empabs1=df_empabs
df_empabs1$predicted_abs_hrs=predictions_LR
aggregate(data=df_empabs1,predicted_abs_hrs~Reason.for.absence,sum) 
aggregate(data=df_empabs1,predicted_abs_hrs~Month.of.absence,sum) 




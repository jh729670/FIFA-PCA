library(dplyr)
library(tidyr)
library(ggplot2)

#reads the data
data_df<-read.csv('data.csv')
#Explanatory Analysis of data
str(data_df)
names(data_df)
View(data_df)

#Grabs only the necessary columns I need to perform PCA
soccer_df<-data_df[,c(3,8,22,55:88)]
names(soccer_df)
#Omits all NA values in data frame
soccer_df<-na.omit(soccer_df)
summary(soccer_df$Position)
#Too many positions, simplify by assigning only 4 positions: OFF,MID,DEF,GK
soccer_df$Position<-if_else(soccer_df$Position %in% c("CB","LB","LCB","LWB","RB","RCB","RWB"),"DEF",
                            ifelse(soccer_df$Position %in% c("CDM","RDM","LDM","CAM","CM","LAM","LCM","LM","RAM","RCM","RM"),"MID",
                                   ifelse(soccer_df$Position %in% c("CF","LAM","LF","LS","LW","RF","RS","ST"),"OFF","GK")))

soccer_df$Position<-as.factor(soccer_df$Position)

View(soccer_df)
str(soccer_df$Position)
#Plots histogram of position players overall ratings
ggplot(soccer_df, aes(Overall,col=Position, fill=Position))+geom_histogram(binwidth = 10)
#Plots median overall score per Position
ggplot(soccer_df, aes(Position,Overall, col=Position, fill=Position))+geom_point()

summary(soccer_df)

dim(soccer_df)

str(soccer_df)
#Performs Principal Component Analysis
soccer_pca<-prcomp(soccer_df[,c(4:37)],scale=TRUE)
soccer_pca$x
soccer_pca$rotation
soccer_pca$sdev
#Plots Scree Plot of Std.Devaition
summary(soccer_pca)
plot(soccer_pca$sdev, type='b',main='Scree Plot of Standard Deviation',
     xlab='# of Variables',ylab='Standard Deviation')
plot(soccer_pca, type='l')

#Scree Plot of variance
#Gets variance of pca
soccer_var<-soccer_pca$sdev^2
explained<-soccer_var/sum(soccer_var)
plot(explained, xlab="Principal Component",ylab="Proportion of Variance",
     type='b',main='Scree Plot of Variance')

#finds what attribute contributes the most regardless of direction PCA1
scores<-abs(soccer_pca$rotation[,1])
scores<-sort(scores,decreasing=TRUE)
top_5<-names(scores[1:5])
top_pca1<-names(scores[1])

#Finds what attributes contriubutes most for PCA2
Scores_2<-abs(soccer_pca$rotation[,2])
Scores_2<-sort(Scores_2, decreasing=TRUE)
top_5.2<-names(Scores_2[1:5])
top_pca2<-names(Scores_2[1])

#Finds what attributes contriubutes most for PCA3
Scores_3<-abs(soccer_pca$rotation[,3])
Scores_3<-sort(Scores_3, decreasing=TRUE)
top_5.3<-names(Scores_3[1:5])
top_pca3<-names(Scores_3[1])

#Finds what attributes contriubutes most for PCA4
Scores_4<-abs(soccer_pca$rotation[,4])
Scores_4<-sort(Scores_4, decreasing=TRUE)
top_5.4<-names(Scores_4[1:5])
top_pca4<-names(Scores_4[1])

#Plots first two PCA components
soccer_df2<-cbind(soccer_df,soccer_pca$x[,1:2])
ggplot(soccer_df2, aes(PC1, PC2, col=Position,fill=Position, alpha=0.5))+geom_point()+
  ggtitle('Graph of PC1 & PC2')


# Mutiple Linear Regression using caret
library(caret)
library(lattice)
str(soccer_df)
#Creates Cross Validation settings. 10 cross Validation
crossValsettings<-trainControl(method='repeatedcv', number=10,
                               savePredictions = TRUE)
#Trains the model using the top Prinicpal Component Variables for the first 4 PCA
crossval<-train(Overall~BallControl+SlidingTackle+Reactions+Strength, data=soccer_df,
                method='lm', trControl=crossValsettings)
#Predictions between observed and predicted
prediction<-predict(crossval,newdata = soccer_df)
#Prints model statistics from crossval. Adj R^2 of .75 and low P-Values
summary(crossval)


#Multiple Linear Regression splitting
sample_df<-sample(1:nrow(soccer_df),.7*nrow(soccer_df))
train<-soccer_df[sample_df,]
test<-soccer_df[-sample_df,]

#Trains the model
train_model<-lm(Overall~BallControl+SlidingTackle+Reactions+Strength,
                data=train)
#Prints Model Statisitcs of the trained model. Get simialr results as the 1st Model
summary(train_model)

prediction_lm<-predict(train_model, data=test, type='response')

#empty Vector used to store PCA. Making a loop

Top_PCA_Var<-vector()

for (i in 1:4){
  scores<-abs(soccer_pca$rotation[,i])
  scores<-sort(scores,decreasing=TRUE)
  Top_PCA_Var[i]<-(scores[1])
  return(Top_PCA_Var)
 }

Top_PCA_Var

scores[1:4]

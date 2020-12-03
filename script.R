#Load the required libraries

library(ggplot2)
library(readr)
library(caTools)


#Load the dataset from remote source

studentdata <- read_csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
View(studentdata)
summary(studentdata)

#plot the graph of hours vs scores
plot(studentdata$Hours,studentdata$Scores,xlab = "Hours studied",ylab = "Percentage Score",main = "Hours vs Percentage",col="red")
#the plot shows a positive linear relation,now lets check the correlation
cor(studentdata$Hours,studentdata$Scores)
#0.97 shows a very strong correlation

#Splitting the data into tranining set and testing set
set.seed(2)
split <- sample.split(studentdata,SplitRatio = 0.8)
split
train<- subset(studentdata,split="TRUE")
test<- subset(studentdata,split="FALSE")
train
test

#Create Model
Model <- lm(studentdata$Scores ~.,data = train)
summary(Model)
#Training Complete

# Now lets plot the regression line on hours vs score graph
plot(studentdata$Hours,studentdata$Scores,xlab = "Hours studied",ylab = "Percentage Score",main = "Hours vs Percentage",col="red")
abline(Model,col="blue")

#Lets make prediction over testing data
Predict <- predict(Model,test)
Predict

#Comparing actual and predicted
df <- data.frame(Actual=test$Scores,Predicted=Predict)
DT::datatable(df)

#Making prediction for given value hours=9.25
new_data <- data.frame(Hours=c(9.25))
predict(Model,newdata = new_data)
#Hence the predicted score is 92.90

#Now lets find the accuracy or Mean absolute error
#RMSE-ROOT MEAN SQUARE VALUE
rmse <- sqrt(mean(Predict- studentdata$Scores)^2)
rmse


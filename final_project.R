#Input DataSet
Dataset<-read.csv("C:/Users/Ahish Nagaraj/Desktop/PROJECT/Dataset_with_rand/Updates Karnataka Agro Dataset 2000.csv")



#Summary of DataSet
Summary_Dataset<-summary(Dataset)
print(Summary_Dataset)


#Noof datasets
Noofdatasets<-c(nrow(Dataset))
print(Noofdatasets)
#barplot(Noofdatasets,col="Black")

MissingData<-data.frame(colSums(is.na(Dataset)))
Missing<-c(MissingData$colSums.is.na.Dataset..)
print(Missing)

install.packages("<package_name>")
library(packagename)



install.packages("ggplot2")
library(ggplot2)

#DISTRICT COUNT Wirh Graph
Districtsumm<-data.frame(summary(Dataset$District_Name))
districtsnames<-unique(Dataset$District_Name)
DistrictCounts<-c(Districtsumm$summary.Dataset.District_Name.)
district_count_data<-data.frame(districtsnames,DistrictCounts)
district_graph_plot_head<-ggplot(head(district_count_data), aes(x=districtsnames, y=DistrictCounts)) +
  geom_bar(stat="identity")
district_graph_plot_tail<-ggplot(tail(district_count_data), aes(x=districtsnames, y=DistrictCounts)) +
  geom_bar(stat="identity")
print(district_graph_plot_head)
print(district_graph_plot_tail)


#Crop Details with plot

Cropnames<-data.frame(summary(Dataset$Crop))
Crop_names<-sort(unique(Dataset$Crop))
Crop_count<-c(Cropnames$summary.Dataset.Crop.)
Crop_details<-data.frame(Crop_names,Crop_count)
middle_crop_details<-data.frame(Crop_details$Crop_names[56:70],Crop_details$Crop_count[56:70])
cropbar_head<-ggplot(head(Crop_details), aes(x=Crop_names, y=Crop_count)) +
  geom_bar(stat="identity")
cropbar_tail<-ggplot(tail(Crop_details), aes(x=Crop_names, y=Crop_count)) +
  geom_bar(stat="identity")
cropbar_middle<-ggplot(middle_crop_details, aes(x=middle_crop_details$Crop_details.Crop_names.56.70., y=middle_crop_details$Crop_details.Crop_count.56.70.)) +
  geom_bar(stat="identity")
#produce bargraphs
print(cropbar_head)
print(cropbar_tail)
print(cropbar_middle)



#crop Year


Data_Production_in_2000<- data.frame(Dataset[Dataset$Crop_Year==2000,"Production"])
Data_Production_in_2000_mean<-mean(Data_Production_in_2000$Dataset.Dataset.Crop_Year....2000...Production..)
library(ggplot2)

Test_Crop<-data.frame(subset(Dataset,Dataset$District_Name =="Mandya" & Dataset$Crop_Year ==2008, select = c("Production","Crop")))

cropbar_Test<-ggplot(Test_Crop, aes(x=Test_Crop$Crop, y=Test_Crop$Production)) +
  geom_bar(stat="identity")

print(cropbar_Test)



#Mandya complete dataframe.

Mandya_Complete_Datadframe<-subset(Dataset,Dataset$District_Name =="Davanagere" , select = c("State_Name","District_Name","Crop_Year","Season","Crop","Area","Production","Ammonium.Value","CO2eq.Value"))

#Mandya dataset for Regression test between Area and production
Mandya_Regression_test<-data.frame(Mandya_Complete_Datadframe$Area,Mandya_Complete_Datadframe$Production)
Mandya_Scatter_plot<-plot(Mandya_Regression_test$Mandya_Complete_Datadframe.Production ~ Mandya_Regression_test$Mandya_Complete_Datadframe.Area, data = Mandya_Regression_test,
                          xlab = "Area",
                          ylab = "Production",
                          main = "Mandya Area v/s Production Scatter Plot"
                          ,pch=19)


#linear model summary
model1<-lm(Mandya_Regression_test$Mandya_Complete_Datadframe.Production ~ Mandya_Regression_test$Mandya_Complete_Datadframe.Area, data = Mandya_Regression_test)

ggplot(Mandya_Complete_Datadframe,aes(x=Area,y=Production))+geom_point()+geom_smooth(method = lm)

summary(model1)

#Single crop dataframe
Crop_data_frame_Mandya<-subset(Mandya_Complete_Datadframe,Mandya_Complete_Datadframe$Crop=="Banana",select = c("State_Name","District_Name","Crop_Year","Season","Crop","Area","Production","Ammonium.Value","CO2eq.Value"))
#linear banana model summary
model_crop1<-lm(Banana_data_frame_Mandya$Production ~ Banana_data_frame_Mandya$Area, data = Banana_data_frame)

#predict Banana production 
predict(model_banana1)

#New value
newvalue1<-data.frame(Banana_data_frame_Mandya$Area==500)

predict(model_banana,newvalue)

ggplot(Banana_data_frame_Mandya,aes(x=Area,y=Production))+geom_point()+geom_smooth(method = lm)

summary(model_banana1)



#n = nrow(Mandya_Complete_Datadframe)
#trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
#train_Mandya <- Mandya_Complete_Datadframe[trainIndex ,]
#test_Mandya <- Mandya_Complete_Datadframe[-trainIndex ,]

#test_without_production=subset(test_Mandya,test_Mandya$District_Name=="Mandya",select=c("State_Name","District_Name","Crop_Year","Season","Crop","Area","Ammonium.Value","CO2eq.Value"))




#Mysuru Complete Dataset
Mysuru_Complete_Datadframe<-subset(Dataset,Dataset$District_Name =="Yadgiri" , select = c("State_Name","District_Name","Crop_Year","Season","Crop","Area","Production","Ammonium.Value","CO2eq.Value"))



#Mysuru dataset for Regression test between Area and production
Mysuru_Regression_test<-data.frame(Mysuru_Complete_Datadframe$Production,Mysuru_Complete_Datadframe$Area)
Mysuru_Scatter_plot<-plot(Mysuru_Regression_test$Mysuru_Complete_Datadframe.Production ~ Mysuru_Regression_test$Mysuru_Complete_Datadframe.Area, data = Mysuru_Regression_test,
                          xlab = "Area",
                          ylab = "Production",
                          main = "Mysuru Area v/s Production Scatter Plot"
                          ,pch=19)
#linear model summary
model2<-lm(Mysuru_Regression_test$Mysuru_Complete_Datadframe.Production ~ Mysuru_Regression_test$Mysuru_Complete_Datadframe.Area, data = Mysuru_Regression_test)


ggplot(Mysuru_Complete_Datadframe,aes(x=Area,y=Production))+geom_point()+geom_smooth(method = lm)

summary(model2)

#Single crop(Banana) Regression Test
Banana_data_frame<-subset(Mysuru_Complete_Datadframe,Mysuru_Complete_Datadframe$Crop=="Banana",select = c("State_Name","District_Name","Crop_Year","Season","Crop","Area","Production","Ammonium.Value","CO2eq.Value"))
Banana_Mysuru_Scatter_plot<-plot(Banana_data_frame$Production ~ Banana_data_frame$Area, data = Banana_data_frame,
                                 xlab = "Area",
                                 ylab = "Production",
                                 main = "Mysuru Banana Area v/s Production Scatter Plot"
                                 ,pch=19)
#linear banana model summary
model_banana<-lm(Banana_data_frame$Production ~ Banana_data_frame$Area, data = Banana_data_frame)

#predict Banana production 
co=coef(summary(model_banana))
predict(model_banana)



#New value
newvalue<-data.frame(Banana_data_frame$Production==2)

predict(model_banana,newvalue)

ggplot(Banana_data_frame,aes(x=Area,y=Production))+geom_point()+geom_smooth(method = lm)

summary(model_banana)






#Weather data set 
weather_Dataset<-read.csv("C:/Users/Ahish Nagaraj/Desktop/PROJECT/weather.csv")

apply(weather_Dataset,2,function(x) length(unique(x)))

colnames(weather_Dataset)

str(weather_Dataset)

#no of rows
n <- nrow(weather_Dataset)

#which spans the following timeline:
c(as.character(weather_Dataset$Date[1]),
  as.character(weather_Dataset$Date[n]))

#We further notice that RISK_MM relation with the RainTomorrow variable is the following.

all.equal(weather_Dataset$RISK_MM > 1,
          weather_Dataset$RainTomorrow == "Yes")


#The Rainfall variable and the RainToday are equivalent according to the following relationship (as anticipated by the Rainfall description).

all.equal(weather_Dataset$Rainfall > 1, weather_Dataset$RainToday == "Yes")

#To make it more challenging, we decide to take RISK_MM, RainFall and RainToday out, and determine a new dataset as herein depicted
weather_data2 <- subset(weather_Dataset, select = -c(Date, Location, RISK_MM, Rainfall, RainToday))

colnames(weather_data2)

cols_withNa <- apply(weather_data2, 2, function(x) sum(is.na(x)))

#Look at the NA's counts associated to WindDir9am. If WindDir9am were a not significative predictor for RainTomorrow, we could take such data column out and increased the complete cases count. When dealing with the categorical variable analysis we determine if that is possible. For now, we consider records without NA's values.
weather_data3 <- weather_data2[complete.cases(weather_data2),]


#Categorical Variable Analysis
factor_vars <- names(which(sapply(weather_data3, class) == "factor"))

factor_vars <- setdiff(factor_vars, "RainTomorrow")

chisq_test_res <- lapply(factor_vars, function(x) { 
  chisq.test(weather_data3[,x], weather_data3[, "RainTomorrow"], simulate.p.value = TRUE)
})
names(chisq_test_res) <- factor_vars
chisq_test_res$WindGustDir

names(chisq_test_res) <- factor_vars
chisq_test_res$WindDir9am

names(chisq_test_res) <- factor_vars
chisq_test_res$WindDir3pm



#Above shown Chi-squared p-value results tell us that RainTomorrow values 
#depend upon WindGustDir (we reject the null hypothesis that RainTomorrow 
#does not depend upon the WindGustDir). We reject the null-hypothesis for 
#WindDir9am and WindDir3pm as well. We therefore expect to take advantage of 
#WindGustDir as predictor and not to consider WindDir9am and WindDir3pm for such purpose.


#Visual Understanding of the Above test
#install.packages("gmodels")
#install.packages("plotly")
library(gmodels)
library(plotly)
library(reshape)
library(ggplot2)
#library(scales)

barchart_res <- lapply(factor_vars, function(x) { 
  title <- colnames(weather_data3[,x, drop=FALSE])
  wgd <- CrossTable(weather_data3[,x], weather_data3$RainTomorrow, prop.chisq=F)
  barchart(wgd$prop.row, stack=F, auto.key=list(rectangles = TRUE, space = "top", title = title))
})
names(barchart_res) <- factor_vars
barchart_res$WindGustDir

barchart_res$WindDir9am

barchart_res$WindDir3pm

weather_data4 <- subset(weather_data2, select = -c(WindDir9am, WindDir3pm))
weather_data5 <- weather_data4[complete.cases(weather_data4),]
colnames(weather_data5)

##Yet to create barcharts
#install.packages("corrplot")
library(corrplot)


##Quantitative Variable Analysis
factor_vars <- names(which(sapply(weather_data5, class) == "factor"))
numeric_vars <- setdiff(colnames(weather_data5), factor_vars)
numeric_vars <- setdiff(numeric_vars, "RainTomorrow")
numeric_vars
numeric_vars_mat <- as.matrix(weather_data5[, numeric_vars, drop=FALSE])
numeric_vars_cor <- cor(numeric_vars_mat)
corrplot(numeric_vars_cor)

#By taking a look at above shown correlation plot, we can state that:
# - Temp9am strongly positive correlated with MinTemp
#- Temp9am strongly positive correlated with MaxTemp
#- Temp9am strongly positive correlated with Temp3pm
#- Temp3pm strongly positive correlated with MaxTemp
##- Pressure9am strongly positive correlated with Pressure3pm
#- Humidity3pm strongly negative correlated with Sunshine
#- Cloud9am strongly negative correlated with Sunshine
#- Cloud3pm strongly negative correlated with Sunshine


pairs(weather_data5[,numeric_vars], col=weather_data5$RainTomorrow)

#install.packages("corrplot")
library(corrplot)
#install.packages("chunked")
library(chunked)
#install.packages("knitr")
library(knitr)

gp <- invisible(lapply(numeric_vars, function(x) { 
  ggplot(data=weather_data5, aes(x= RainTomorrow, y=eval(parse(text=x)), col = RainTomorrow)) + geom_boxplot() + xlab("RainTomorrow") + ylab(x) + ggtitle("") + theme(legend.position="none")}))
gp

#grob_plots <- invisible(lapply(chunk(1,length(gp), 4), function(x) {
# marrangeGrob(grobs=lapply(gp[x], ggplotGrob), nrow=2, ncol=2)}))
#grob_plots

write.csv(weather_data5, file="weather_data5.csv", sep=",", row.names=FALSE)


suppressPackageStartupMessages(library(caret))

set.seed(1023)

colnames(weather_data5)

nrow(weather_data5)

sum(weather_data5["RainTomorrow"] == "Yes")

sum(weather_data5["RainTomorrow"] == "No")

train_rec <- createDataPartition(weather_data5$RainTomorrow, p = 0.7, list = FALSE)
training <- weather_data5[train_rec,]
testing <- weather_data5[-train_rec,]

#Check For Data Balance

sum(training["RainTomorrow"] == "Yes")/sum(training["RainTomorrow"] == "No")


sum(testing["RainTomorrow"] == "Yes")/sum(testing["RainTomorrow"] == "No")




#9AM Forecast Model

#we are going to take advantage of a train control directive made available by the caret package which prescribes repeated k-flod cross-validation
trControl <- trainControl(method = "repeatedcv",  repeats = 5, number = 10, verboseIter = FALSE)

predictors_9am_c1 <- c("Cloud9am",  "Humidity9am", "Pressure9am", "Temp9am")

formula_9am_c1 <- as.formula(paste("RainTomorrow", paste(predictors_9am_c1, collapse="+"), sep="~"))

#train
mod9am_c1_fit <- train(formula_9am_c1,  data=training, method="glm", 
                       family="binomial", trControl = trControl, metric = 'Accuracy')
mod9am_c1_fit$results$Accuracy

#summary
summary_rep <- summary(mod9am_c1_fit$finalModel)

summary_rep

#Logistic Regression for more adequate fit

1 - pchisq(summary_rep$deviance, summary_rep$df.residual)


drop1(mod9am_c1_fit$finalModel, test="Chisq")



predictors_9am_c2 <- c("Cloud9am",  "Humidity9am", "Pressure9am", "MinTemp")
formula_9am_c2 <- as.formula(paste("RainTomorrow", paste(predictors_9am_c2, collapse="+"), sep="~"))
mod9am_c2_fit <- train(formula_9am_c2,  data=training, method="glm", 
                       family="binomial", trControl = trControl, metric = 'Accuracy')
mod9am_c2_fit$results$Accuracy

summary_rep <- summary(mod9am_c2_fit$finalModel)

summary_rep


1 - pchisq(summary_rep$deviance, summary_rep$df.residual)

mod9am_pred <- predict(mod9am_c1_fit, testing)
confusionMatrix(mod9am_pred, testing[,"RainTomorrow"])

#3PM Forecast Model


predictors_3pm_c1 <- c("Cloud3pm", "Humidity3pm", "Pressure3pm", "Temp3pm")
formula_3pm_c1 <- as.formula(paste("RainTomorrow", paste(predictors_3pm_c1, collapse="+"), sep="~"))
mod3pm_c1_fit <- train(formula_3pm_c1,  data = training, method = "glm", family = "binomial",
                       trControl = trControl, metric = 'Accuracy')
mod3pm_c1_fit$results$Accuracy

summary_rep <- summary(mod3pm_c1_fit$finalModel)

drop1(mod3pm_c1_fit$finalModel, test="Chisq")

#The p-value associated with the null hypothesis that this model is a good fit for the data is:
1 - pchisq(summary_rep$deviance, summary_rep$df.residual)

#We go on with the computation of the test set accuracy.

mod3pm_pred <- predict(mod3pm_c1_fit, testing)
confusionMatrix(mod3pm_pred, testing[,"RainTomorrow"])

#The test set prediction accuracy is quite satisfactory.

#Evening Forecast Model

predictors_evening_c1 <- c("Pressure3pm", "Temp3pm", "Sunshine")
formula_evening_c1 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c1, collapse="+"), sep="~"))
mod_ev_c1_fit <- train(formula_evening_c1,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c1_fit$results$Accuracy

summary_rep <- summary(mod_ev_c1_fit$finalModel)
summary_rep


drop1(mod_ev_c1_fit$finalModel, test="Chisq")

1 - pchisq(summary_rep$deviance, summary_rep$df.residual)

predictors_evening_c2 <- c(predictors_3pm_c1, "WindGustDir", "WindGustSpeed")
formula_evening_c2 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c2, collapse="+"), sep="~"))
mod_ev_c2_fit <- train(formula_evening_c2,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c2_fit$results$Accuracy

drop1(mod_ev_c2_fit$finalModel, test="Chisq")


predictors_evening_c2 <- c(predictors_3pm_c1, "WindGustDir")
formula_evening_c2 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c2, collapse="+"), sep="~"))
mod_ev_c2_fit <- train(formula_evening_c2,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c2_fit$results$Accuracy
summary_rep <- summary(mod_ev_c2_fit$finalModel)

summary_rep

drop1(mod_ev_c2_fit$finalModel, test="Chisq")

1 - pchisq(summary_rep$deviance, summary_rep$df.residual)

predictors_evening_c3 <- c("Pressure3pm", "Sunshine")
formula_evening_c3 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c3, collapse="+"), sep="~"))
mod_ev_c3_fit <- train(formula_evening_c3,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c3_fit$results$Accuracy

summary_rep <- summary(mod_ev_c3_fit$finalModel)

drop1(mod_ev_c3_fit$finalModel, test="Chisq")

1 - pchisq(summary_rep$deviance, summary_rep$df.residual)

#We compare the last two models by running an ANOVA analysis on those to 
anova(mod_ev_c2_fit$finalModel, mod_ev_c3_fit$finalModel, test="Chisq")

modevening_pred <- predict(mod_ev_c2_fit, testing)
confusionMatrix(modevening_pred, testing[,"RainTomorrow"])

modevening_pred <- predict(mod_ev_c3_fit, testing)
confusionMatrix(modevening_pred, testing[,"RainTomorrow"])

saveRDS(list(weather_data5, train_rec, training, testing, mod9am_c1_fit, mod9am_c2_fit, mod3pm_c1_fit, mod_ev_c2_fit, mod_ev_c3_fit), file="wf_log_reg_part2.rds")


#part 4 


all.equal(weather_Dataset$Rainfall > 1, weather_Dataset$RainToday == "Yes")

#Moderate Rainfall Scenario

weather_data6 <- subset(weather_Dataset, select = -c(Date, Location, RISK_MM, RainToday, WindDir9am, WindDir3pm))
weather_data6$RainfallTomorrow <- c(weather_data6$Rainfall[2:nrow(weather_data6)], NA)
weather_data6$Humidity3pmTomorrow <- c(weather_data6$Humidity3pm[2:nrow(weather_data6)], NA)
weather_data6$WindGustSpeedTomorrow <- c(weather_data6$WindGustSpeed[2:nrow(weather_data6)], NA)
weather_data6$SunshineTomorrow <- c(weather_data6$Sunshine[2:nrow(weather_data6)], NA)
weather_data6$MinTempTomorrow <- c(weather_data6$MinTemp[2:nrow(weather_data6)], NA)
weather_data6$MaxTempTomorrow <- c(weather_data6$MaxTemp[2:nrow(weather_data6)], NA)



weather_data7 = weather_data6[complete.cases(weather_data6),]
head(weather_data7)

hr_idx = which(weather_data7$RainfallTomorrow > 15)

train_hr <- hr_idx[hr_idx %in% train_rec]

test_hr <- hr_idx[!(hr_idx %in% train_rec)]

rain_test <- weather_data7[test_hr,]

rain_test

#Let us see how the first weather forecast evening model performs

opt_cutoff <- 0.42
pred_test <- predict(mod_ev_c2_fit, rain_test, type="prob")
prediction <- ifelse(pred_test$Yes >= opt_cutoff, "Yes", "No")
data.frame(prediction = prediction, RainfallTomorrow =rain_test$RainfallTomorrow)

confusionMatrix(as.factor(prediction), rain_test$RainTomorrow)

#Then, the second evening weather forecast model.

opt_cutoff <- 0.56
pred_test <- predict(mod_ev_c3_fit, rain_test, type="prob")
prediction <- ifelse(pred_test$Yes >= opt_cutoff, "Yes", "No")
data.frame(prediction = prediction, RainfallTomorrow = rain_test$RainfallTomorrow)


confusionMatrix(as.factor(prediction), rain_test$RainTomorrow)


#Chance of Rain



chance_of_rain <- function(model, data_record){
  chance_frac <- predict(mod_ev_c3_fit, data_record, type="prob")[, "Yes"]
  paste(round(chance_frac*100), "%", sep="")
}

chance_of_rain(mod_ev_c3_fit, testing[1:10,])


#Tomorrow’s Rainfall Prediction

weather_data8 = weather_data7[weather_data7$RainfallTomorrow > 1,]
rf_fit <- lm(RainfallTomorrow ~  MaxTemp + Sunshine + WindGustSpeed - 1, data = weather_data8)
summary(rf_fit)


#Plots

lm_pred <- predict(rf_fit, weather_data8)
plot(x = seq_along(weather_data8$RainfallTomorrow), y = weather_data8$RainfallTomorrow, type='p', xlab = "observations", ylab = "RainfallTomorrow")
legend("topright", c("actual", "predicted"), fill = c("black", "red"))
points(x = seq_along(weather_data8$RainfallTomorrow), y = lm_pred, col='red')

#Tomorrow’s Humidity 3pm Prediction


h3pm_fit <- lm(Humidity3pmTomorrow ~ Humidity3pm + Sunshine, data = weather_data7)
summary(h3pm_fit)

lm_pred <- predict(h3pm_fit, weather_data7)
plot(x = seq_along(weather_data7$Humidity3pmTomorrow), y = weather_data7$Humidity3pmTomorrow, type='p', xlab = "observations", ylab = "Humidity3pmTomorrow")
legend("topright", c("actual", "predicted"), fill = c("black", "red"))
points(x = seq_along(weather_data7$Humidity3pmTomorrow), y = lm_pred, col='red')

#Tomorrow’s WindGustSpeed Prediction

wgs_fit <- lm(WindGustSpeedTomorrow ~ WindGustSpeed + Pressure9am + Pressure3pm, data = weather_data7)
summary(wgs_fit)


lm_pred <- predict(wgs_fit, weather_data7)
plot(x = seq_along(weather_data7$WindGustSpeedTomorrow), y = weather_data7$WindGustSpeedTomorrow, type='p', xlab = "observations", ylab = "WindGustSpeedTomorrow")
legend("topright", c("actual", "predicted"), fill = c("black", "red"))
points(x = seq_along(weather_data7$WindGustSpeedTomorrow), y = lm_pred, col='red')

#Tomorrow’s Sunshine Prediction

sun_fit <- lm(SunshineTomorrow ~ Sunshine*Humidity3pm + Cloud3pm + Evaporation + I(Evaporation^2) + WindGustSpeed - 1, data = weather_data7)
summary(sun_fit)

lm_pred <- predict(sun_fit, weather_data7)
plot(x = seq_along(weather_data7$SunshineTomorrow), y = weather_data7$SunshineTomorrow, type='p', xlab = "observations", ylab = "SunshineTomorrow")
legend("topright", c("actual", "predicted"), fill = c("black", "red"))
points(x = seq_along(weather_data7$SunshineTomorrow), y = lm_pred, col='red')


cloud9am_fit <- lm(Cloud9am ~ Sunshine, data = weather_data7)
summary(cloud9am_fit)

lm_pred <- round(predict(cloud9am_fit, weather_data7))
lm_pred[lm_pred == 9] = 8
plot(x = weather_data7$Sunshine, y = weather_data7$Cloud9am, type='p', xlab = "Sunshine", ylab = "Cloud9am")
legend("topright", c("actual", "predicted"), fill = c("black", "red"))
points(x = weather_data7$Sunshine, y = lm_pred, col='red')

cloud3pm_fit <- lm(Cloud3pm ~ Sunshine, data = weather_data7)
summary(cloud3pm_fit)


lm_pred <- round(predict(cloud3pm_fit, weather_data7))
lm_pred[lm_pred == 9] = 8
plot(x = weather_data7$Sunshine, y = weather_data7$Cloud3pm, type='p', xlab = "Sunshine", ylab = "Cloud3pm")
legend("topright", c("actual", "predicted"), fill = c("black", "red"))
points(x = weather_data7$Sunshine, y = lm_pred, col='red')

#Tomorrow’s MinTemp Prediction

minTemp_fit <- lm(MinTempTomorrow ~ MinTemp + Humidity3pm , data = weather_data7)
summary(minTemp_fit)


lm_pred <- predict(minTemp_fit, weather_data7)
plot(x = weather_data7$Sunshine, y = weather_data7$MinTemp, type='p', xlab = "Sunshine", ylab = "MinTemp")
legend("topright", c("actual", "fitted"), fill = c("black", "red"))
points(x = weather_data7$Sunshine, y = lm_pred, col='red')



#Tomorrow’s MaxTemp Prediction

maxTemp_fit <- lm(MaxTempTomorrow ~ MaxTemp + Evaporation, data = weather_data7)
summary(maxTemp_fit)


lm_pred <- predict(maxTemp_fit, weather_data7)
plot(x = weather_data7$Sunshine, y = weather_data7$MaxTemp, type='p', xlab = "Sunshine", ylab = "MaxTemp")
legend("topright", c("actual", "fitted"), fill = c("black", "red"))
points(x = weather_data7$Sunshine, y = lm_pred, col='red')


#CloudConditions computation Not Working
#install.packages("knitr")
#install.packages("rmarkdown")
library(knitr)
library(rmarkdown)

computeCloudConditions = function(cloud_9am, cloud_3pm) {
  cloud_avg = min(round((cloud_9am + cloud_3pm)/2), 8)
  cc_str = NULL
  if (cloud_avg == 8) {
    cc_str = "Cloudy"
  } else if (cloud_avg >= 6) {
    cc_str = "Mostly Cloudy"
  } else if (cloud_avg >= 3) {
    cc_str = "Partly Cloudy"
  } else if (cloud_avg >= 1) {
    cc_str = "Mostly Sunny"
  } else if (cloud_avg < 1) {
    cc_str = "Sunny"
  }
  cc_str
}


#Weather Forecast Report



weather_report <- function(today_record, rain_tomorrow_model, cutoff) {
  # RainTomorrow  prediction
  rainTomorrow_prob <- predict(rain_tomorrow_model, today_record, type="prob")
  rainTomorrow_pred = ifelse(rainTomorrow_prob$Yes >= cutoff, "Yes", "No")
  
  # Rainfall prediction iff RainTomorrow prediction is Yes; chance of rain probability
  rainfall_pred <- NA
  chance_of_rain <- NA
  if (rainTomorrow_pred == "Yes") {
    rainfall_pred <- round(predict(rf_fit, today_record), 1)
    chance_of_rain <- round(rainTomorrow_prob$Yes*100)
  }
  
  # WindGustSpeed prediction
  wgs_pred <- round(predict(wgs_fit, today_record), 1)
  
  # Humidity3pm prediction
  h3pm_pred <- round(predict(h3pm_fit, today_record), 1)
  
  # sunshine prediction is used to fit Cloud9am and Cloud3pm
  sun_pred <- predict(sun_fit, today_record)
  
  cloud9am_pred <- min(round(predict(cloud9am_fit, data.frame(Sunshine=sun_pred))), 8)
  cloud3pm_pred <- min(round(predict(cloud3pm_fit, data.frame(Sunshine=sun_pred))), 8)
  # a descriptive cloud conditions string is computed
  CloudConditions_pred <- computeCloudConditions(cloud9am_pred, cloud3pm_pred)
  
  # MinTemp prediction
  minTemp_pred <- round(predict(minTemp_fit, today_record), 1)
  
  # MaxTemp prediction
  maxTemp_pred <- round(predict(maxTemp_fit, today_record), 1)
  
  # converting all numeric predictions to strings
  if (is.na(rainfall_pred)) {
    rainfall_pred_str <- "< 1 mm"
  } else {
    rainfall_pred_str <- paste(rainfall_pred, "mm", sep = " ")
  }
  
  if (is.na(chance_of_rain)) {
    chance_of_rain_str <- ""
  } else {
    chance_of_rain_str <- paste(chance_of_rain, "%", sep="")
  }
  
  wgs_pred_str <- paste(wgs_pred, "Km/h", sep= " ")
  h3pm_pred_str <- paste(h3pm_pred, "%", sep = "")
  minTemp_pred_str <- paste(minTemp_pred, "°C", sep= "")
  maxTemp_pred_str <- paste(maxTemp_pred, "°C", sep= "")
  
  report <- data.frame(Rainfall = rainfall_pred_str,
                       ChanceOfRain = chance_of_rain_str,
                       WindGustSpeed = wgs_pred_str, 
                       Humidity = h3pm_pred_str,
                       CloudConditions = CloudConditions_pred,
                       MinTemp = minTemp_pred_str,
                       MaxTemp = maxTemp_pred_str)
  report
}

weather_report

tomorrow_report <- weather_report(weather_Dataset[10,], mod_ev_c3_fit, 0.56)

install.packages("latexpdf")

library(latexpdf)
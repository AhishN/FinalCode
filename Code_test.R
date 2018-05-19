test.data<-data.frame(
  
  Area=c(1,2,3,4,5,6),
  Production=c(1,3,6,1,1,12)
  
)


test.data

#install.packages("caret",dependencies = c("Depends", "Suggests"))
library(caret)


#model1
fit=lm(Production~Area,test.data)

summary(fit)



co[,1]

co[,4]

predict(fit)

newcar=data.frame(Area=10)

predict(fit,newcar)

ggplot(test.data,aes(x=Area,y=Production))+geom_point()+geom_smooth(method = lm)



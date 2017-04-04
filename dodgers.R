##Chinakonda Chenchu, Bhargava Reddy
# read in data and create a data frame called dodgers
dodgers <- read.csv("dodgers.csv")
print(str(dodgers))  # check the structure of the data frame

# define an ordered day-of-week variable 
# for plots and data summaries
dodgers$ordered_day_of_week <- with(data=dodgers,
                                    ifelse ((day_of_week == "Monday"),1,
                                    ifelse ((day_of_week == "Tuesday"),2,
                                    ifelse ((day_of_week == "Wednesday"),3,
                                    ifelse ((day_of_week == "Thursday"),4,
                                    ifelse ((day_of_week == "Friday"),5,
                                    ifelse ((day_of_week == "Saturday"),6,7)))))))

dodgers$ordered_day_of_week <- factor(dodgers$ordered_day_of_week, levels=1:7,
                                      labels=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))

print(str(dodgers))  # check the structure of the data frame

#View(dodgers)
# define an ordered month variable 
# for plots and data summaries
dodgers$ordered_month <- with(data=dodgers,
                              ifelse ((month == "APR"),4,
                              ifelse ((month == "MAY"),5,
                              ifelse ((month == "JUN"),6,
                              ifelse ((month == "JUL"),7,
                              ifelse ((month == "AUG"),8,
                              ifelse ((month == "SEP"),9,10)))))))

dodgers$ordered_month <- factor(dodgers$ordered_month, levels=4:10,
                                labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct"))

#View(dodgers)
#########
##  Exploratory Analysis using ggplot2
#########
ggplot(dodgers, aes(x=temp,y=attend/1000,shape=fireworks))+
  geom_point(aes(color=fireworks),size=4) + 
  facet_grid(day_night~skies,switch = "both") + theme_light() +
  xlab("Temperature (F)")+ ylab("Attendence  (Thousands)")+
  ggtitle("Relationship between Temperature and Attendence")

ggplot(dodgers,aes(x=attend/1000,y=opponent))+
  geom_point(aes(color=day_night),size=4)

#######################################
#########################################

ggplot(dodgers,aes(x=attend/1000,y=fireworks,color=ordered_day_of_week))+ 
  geom_bar(stat="identity")


#fireworks and bobblehead
ggplot(dodgers, aes(x=temp,y=attend/1000,shape=fireworks))+
  geom_point(aes(color=bobblehead),size=4) + 
  facet_grid(ordered_day_of_week~.) + theme_light() +
  xlab("Temperature (F)")+ ylab("Attendence  (Thousands)")+
  ggtitle("Relationship between Temperature and Attendence")

#########################################
#########################################


#################################################
##  Making the training and testing data frames
####################################################

# employ a training-and-test regimen
set.seed(1234) # set seed for repeatability of training-and-test split

prob_dist<-c(2/3,1/3)

training_test <- sample(c(1,2),nrow(dodgers),replace=TRUE,prob=prob_dist)
dodgers$training_test <- factor(training_test,levels=c(1,2), labels=c("TRAIN","TEST"))


dodgers.train <- subset(dodgers, training_test == "TRAIN")
print(str(dodgers.train)) # check training data frame
#View(dodgers)

dodgers.test <- subset(dodgers, training_test == "TEST")
print(str(dodgers.test)) # check test data frame


#################################################
##  Linear Regression Analysis
####################################################
my.model <- {attend ~ temp + fireworks + ordered_day_of_week + bobblehead + fireworks}

# fit the model to the training set
train.model.fit <- lm(my.model, data = dodgers.train)
# obtain predictions from the training set
dodgers.train$predict_attend <- predict(train.model.fit) 

# evaluate the fitted model on the test set
dodgers.test$predict_attend <- predict(train.model.fit, 
                                       newdata = dodgers.test)

# compute the proportion of response variance
# accounted for when predicting out-of-sample
cat("\n","Proportion of Test Set Variance Accounted for: ",
    round((with(dodgers.test,cor(attend,predict_attend)^2)),
          digits=3),"\n",sep="")

# use the full data set to obtain an estimate of the increase in
# attendance due to bobbleheads, controlling for other factors 
my.model.fit <- lm(my.model, data = dodgers)  # use all available data
print(summary(my.model.fit))
# tests statistical significance of the bobblehead promotion
# type I anova computes sums of squares for sequential tests
print(anova(my.model.fit))  

cat("\n","Estimated Effect of Bobblehead Promotion on Attendance: ",
    round(my.model.fit$coefficients[length(my.model.fit$coefficients)],
          digits = 0),"\n",sep="")


library(car)
#residualPlots(my.model.fit)
marginalModelPlots(my.model.fit)
print(outlierTest(my.model.fit))








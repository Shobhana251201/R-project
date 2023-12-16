project <- read.csv("C:\Users\Dell\Desktop",header = T)
head(project)
names(project)

library(plyr)
library(dplyr)

summary(project)

dim(project) #no. of rows and cols

# % of employees that leave each year
StatusCount<- as.data.frame.matrix(project %>%group_by(STATUS_YEAR) %>%select(STATUS) %>%table())
StatusCount$TOTAL<-StatusCount$ACTIVE + StatusCount$TERMINATED
StatusCount$PercentTerminated <-StatusCount$TERMINATED/(StatusCount$TOTAL)*100
View(StatusCount)



library(ggplot2)

#in which dept termination is most
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(BUSINESS_UNIT),fill = as.factor(STATUS)),data=project,position = position_stack())

#type of terminations 
TerminatesData<- as.data.frame(project %>%filter(STATUS=="TERMINATED"))
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(STATUS_YEAR),fill = as.factor(termtype_desc)),data=TerminatesData,position = position_stack())+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
?desc

#reason for voluntary and involuntary terminations dept wise

ggplot() + geom_bar(aes(y = ..count..,x =as.factor(department_name),fill = as.factor(termreason_desc)),data=TerminatesData,position = position_stack())+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


term_vars <- c("age","length_of_service","city_name", "department_name","job_title","store_name","gender_full","BUSINESS_UNIT","STATUS")
# import libraries
library(rattle)  # graphical interface for data science in R
library(magrittr)  # For %>% and %<>% operators.
library(rpart.plot) # decision tree model and plot
# Partition the data into training and test sets
pro_term_train <- subset(project, STATUS_YEAR < 2015)
pro_term_test <- subset(project, STATUS_YEAR == 2015)
set.seed(99)  # set a pre-defined value for the random seed so that results are repeatable
# Decision tree model
rpart_model <- rpart(STATUS ~.,
                     data = pro_term_train[term_vars],
                     method = 'class',
                     parms = list(split='information'),
                     control = rpart.control(usesurrogate = 0,
                                             maxsurrogate = 0))
# Plot the decision tree
rpart.plot(rpart_model, roundint = FALSE, type = 3)



# create separate variable for voluntary_terminations
project$resigned <- ifelse(project$termreason_desc == "Resignaton", "Yes", "No")
project$resigned <- as.factor(project$resigned)  # convert to factor (from character)
summary(project$resigned)


# from above We can see that there are only 2111 resignations compared to 47542 non-resignations.

# Subset the data again into train & test sets. Here we use all years before 2015 (2006-14) as the training set, with the last year (2015) as the test set
project_train <- subset(project, STATUS_YEAR < 2015)
project_test <- subset(project, STATUS_YEAR == 2015)
library(ROSE) 

# Tables to show balanced dataset sample sizes
project_train_rose<- ovun.sample(resigned~., data=project_train, N=nrow(project_train), p=0.5,
                                seed=1, method="both")$data
project_train_rose
table(project_train_rose$resigned) # now the resigned and non resigned nos are relatively balanced for any comparison



library(randomForest) 
# Select variables (res_vars) for the model to predict 'resigned'
res_vars <- c("age","length_of_service","city_name", "department_name","job_title","store_name","gender_full","BUSINESS_UNIT","resigned")
set.seed(222)
project_res_rose_RF <- randomForest(resigned ~ .,
                                data = project_train_rose[res_vars],
                                ntree=500, importance = TRUE,
                                na.action = na.omit)
varImpPlot(project_res_rose_RF,type=1,main="Variable Importance (Accuracy)",sub = "Random Forest Model")
varImpPlot

var_importance <-importance(project_res_rose_RF)
project_res_rose_RF  # view results & Confusion matrix



# generate predictions based on test data ("project_test")
project_res_rose_RF_pred <- predict(project_res_rose_RF, newdata = project_test)
confusionMatrix(data = project_res_rose_RF_pred,
                reference = project_test$resigned,
                positive = "Yes", mode = "prec_recall")




library(randomForest)
library(dplyr)
library(dbplyr)

# Calculate prediction probabilites of employees who will resign
project_res_rose_RF_pred_probs <- predict(project_res_rose_RF, project_test, type="prob")
Employees_flight_risk <- as.data.frame(cbind(project_test$EmployeeID,
                                             project_res_rose_RF_pred_probs))
Employees_flight_risk <- rename(Employees_flight_risk,
                                EmployeeID = V1)
Employees_flight_risk <- arrange(Employees_flight_risk, desc(Yes))
head(Employees_flight_risk)




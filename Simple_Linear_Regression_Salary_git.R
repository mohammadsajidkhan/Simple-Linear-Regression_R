#simple linear regression

                  # 1)Data preprocessing 
                  # - importing dataset
                  # - filling missing coloumns of dataset
                  # - Encoding categorical data
                  
#Importing Dataset
dataset=read.csv("Salary_Data.csv")

# Taking care of missing data
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)
dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$Salary)

# Encoding categorical data
dataset$Country = factor(dataset$Country,
                         levels = c('France', 'Spain', 'Germany'),
                         labels = c(1, 2, 3))
dataset$Purchased = factor(dataset$Purchased,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))

                  # 2) Splitting the dataset into the Training set 
                  #    and Test set
                  # install.packages('caTools')
library(caTools)
set.seed(123)
split=sample.split(dataset$Salary, SplitRatio=2/3)
train_set=subset(dataset, split==TRUE )
test_set=subset(dataset, split==FALSE )

#filling simple linear regression into training set
regressor=lm(formula = Salary~YearsExperience, data=train_set)

#Predicting the Test set data
y_pred=predict(regressor, newdata = test_set) 

                  #3) Visualization of results

# Visualizing the Training set results
library(ggplot2)
ggplot()+
  geom_point(aes(x=train_set$YearsExperience, y=train_set$Salary),
             color="red")+
  geom_line(aes(x=train_set$YearsExperience,y=predict(regressor, newdata=train_set)),
            color="blue")+
  ggtitle('Salary vs Experince ( Training Set)')+
  xlab('Years of Experience')+
  ylab('Salary')


#Visualizing Test set results

ggplot()+
  geom_point(aes(x=test_set$YearsExperience, y=test_set$Salary),
             color="black")+
  geom_line(aes(x=test_set$YearsExperience,y=predict(regressor, newdata=test_set)),
            color="blue")+
  ggtitle('Salary vs Experince ( Test Set)')+
  xlab('Years of Experience')+
  ylab('Salary')



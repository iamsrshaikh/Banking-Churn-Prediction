
#Installation of Required Libraries
install.packages("readr");
install.packages('ggplot2');
install.packages("dplyr");


library(readr);
library(ggplot2);
library(dplyr);
library(tidyr);

# Importing the dataset
dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]


#Exploratory Data Analysis
View(dataset)
str(dataset);
dim(dataset);
glimpse(dataset);
summary(dataset);


ggplot(dataset, aes(Exited, fill = Exited)) +
  geom_bar() +
  theme(legend.position = 'none')

table(dataset$Exited)





# Encoding the categorical variables as factors

dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

View(dataset)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')


install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
View(training_set)

# Feature Scaling
training_set[-11] = scale(training_set[-11])
test_set[-11] = scale(test_set[-11])

View(training_set)

# Fitting ANN to the Training set
install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)
model = h2o.deeplearning(y = 'Exited',
                         training_frame = as.h2o(training_set),
                         activation = 'Rectifier',
                         hidden = c(5,5),
                         epochs = 100,
                         train_samples_per_iteration = -2)

# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[-11]))
y_pred = (y_pred > 0.5)
y_pred = as.vector(y_pred)

result <- paste(test_set$Exited, "|", y_pred)
View(result)

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)
cm



 

h2o.shutdown()
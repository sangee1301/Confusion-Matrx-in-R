setwd("C:/Users/Sangee/Downloads/upwork/Phani/Confusion Matrix in R")

#importing libraries
library(randomForest)

#importing the bank marketing dataset
bank <- read.csv("Bank.csv", sep=";", stringsAsFactors ==FALSE)
str(bank)

#create a new dataframe with only Age, Job, Marital, Education, 
#   Housing, Loan, contact, poutcome and y
bank_new <- data.frame(as.numeric(as.factor(bank$age)),
                       as.numeric(as.factor(bank$job)),
                       as.numeric(as.factor(bank$marital)),
                       as.numeric(as.factor(bank$education)),
                       as.numeric(as.factor(bank$housing)),
                       as.numeric(as.factor(bank$loan)),
                       as.numeric(as.factor(bank$contact)),
                       as.numeric(as.factor(bank$poutcome)),
                       bank$y)
# Rename the columns
colnames(bank_new) <- c("Age", "Job", "Marital", "Education", 
                        "Housing", "Loan","contact","poutcome" ,"y")
#spliting  the dataframe into train and test data
set.seed(2262)
train_ind <- sample(seq_len(nrow(bank_new)), size = floor(0.80 * nrow(bank_new)))
train <- bank_new[train_ind, ]
test <- bank_new[-train_ind, ]

#random forest classifier
fitRF <- randomForest(as.factor(bank$y)~., bank_new)
predicted <- predict(fitRF, bank_new)

#confusion matrix
confusionMatrix(predicted, factor(bank$y))
confusion_table <- table(predicted, bank$y)

n <- sum(confusion_table) # number of instances
rowsums <- apply(confusion_table, 1, sum) # number of instances per class
colsums <- apply(confusion_table, 2, sum) # number of predictions per class
diag <- diag(confusion_table) # number of correctly classified instances per class
accuracy <- sum(diag) / n # Calculate the Accuracy
precision <- diag / colsums # Calculate the Precision
recall <- diag / rowsums # Calculate the Recall



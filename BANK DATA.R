# Get  the working directories
getwd()
# Install these packages into the workspace
install.packages("plyr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("e1071")
install.packages("gmodels")
install.packages("caret")
install.packages("ROCR")
install.packages("kableExtra")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caTools")
install.packages("ncvreg")
install.packages("biglasso")
install.packages("bigmemory")
install.packages("glmnet")
install.packages("lars")
install.packages("randomForest")
install.packages("rattle")
install.packages("gridExtra")
# Loaded the below libraries into the work space
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
require(e1071)
library(gmodels)
library(data.table)
library(caret)
library(ROCR)
library(kableExtra)
library(rpart)
library(rpart.plot)
library(caTools)
library(ncvreg)
library(biglasso)
library(bigmemory)
library(gridExtra)
library(rattle)
require
library(psych)
library(ggplot2)
library(GGally)
#Load the dataset to be used
bmData <- read.csv("bmData.csv")
# To View the bank Data set
View(bmData)
# Remove duplicate rows
bmclean <- bmData %>% distinct
# Remove rows with cols that has missing values
nrow(bmclean)
# Save the updated data in the below format
write.csv(bmclean, file = "Bank Data Cleaned.csv")
# Conditionally formatting all "y" to 0, and 1.
bmclean$y <- ifelse(bmclean$y == "y", 1, 0)
# Changing Categorical Variables to Factor Variables
bmclean$job <- as.factor(bmclean$job)
bmclean$marital <- as.factor(bmclean$marital)
bmclean$education <- as.factor(bmclean$education)
bmclean$default <- as.factor(bmclean$default)
bmclean$housing <- as.factor(bmclean$housing)
bmclean$loan <- as.factor(bmclean$loan)
bmclean$contact <- as.factor(bmclean$contact)
bmclean$month <- as.factor(bmclean$month)
bmclean$day_of_week <- as.factor(bmclean$day_of_week)
bmclean$poutcome <- as.factor(bmclean$poutcome)
bmclean$y <- as.factor(bmclean$y)
# Observing the Structure of the Dataset
str(bmclean)
#Observing the Summary of the Dataset
summary(bmclean)
# Description of the dataset
describeData(bmclean)
# Investigating the levels of categorical variables
levels(bmclean$job)
levels(bmclean$marital)
levels(bmclean$education)
levels(bmclean$default)
levels(bmclean$loan)
levels(bmclean$contact)
levels(bmclean$poutcome)
levels(bmclean$y)
levels(bmclean$housing)
levels(bmclean$month)
levels(bmclean$day_of_week)
# Assigning the subscribers to YSub 
YSub <- filter(bmData, y == "yes")
# Correlation of Numeric Variables
correlation_matrix <- cor(bmclean[, c("age", "duration", "emp.var.rate", "cons.price.idx", 
                                      "cons.conf.idx", "euribor3m", "nr.employed", 
                                      "previous", "pdays", "campaign")])
# Visualize the correlation matrix using a heatmap
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(20),
        main = "Correlation Matrix Heatmap",
        xlab = "Numerical Variables", ylab = "Numerical Variables")
# Create histogram for age by subscription status
ggplot(data = bmData, aes(x = age, fill = y)) +
  geom_histogram(binwidth = 7, position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of Age by Term Deposit Subscription",
       x = "Age",
       y = "Frequency",
       fill = "Subscription Status") +
  theme_minimal()
# Create histogram for Duration by subscription status
ggplot(data = bmData, aes(x = duration, fill = y)) +
  geom_histogram(binwidth = 200, position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of Duration by Term Deposit Subscription",
       x = "Duration",
       y = "Frequency",
       fill = "Subscription Status") +
  theme_minimal()
# Create a boxplot for age by subscription status
ggplot(data = bmData, aes(x = y, y = age, fill = y)) +
  geom_boxplot() +
  labs(title = "Distribution of Age by Term Deposit Subscription",
       x = "Subscription Status",
       y = "Age",
       fill = "Subscription Status") +
  theme_minimal()
# Frequency Tables for Categorical Variables
table(bmData$poutcome, bmData$y)
table(bmData$contact, bmData$y)
table(bmData$education)
table(bmData$default)
table(bmData$housing)
table(bmData$month)
#Exploring the Relationship between the Categorical Variables and the Term Subscription(y)
library(egg)
library(ggpubr)
p1 <-ggplot(bmData, aes(x=job,fill=y))+ geom_bar()+theme_bw()+xlab("job") + ylab("frequency")
p2 <-ggplot(bmData, aes(x=marital,fill=y))+ geom_bar(position = 'fill')+theme_bw()+xlab("marital") + ylab("frequency")
p3 <-ggplot(bmData, aes(x=education,fill=y))+ geom_bar(position = 'fill')+theme_bw()+xlab("education") + ylab("frequency")
p4<-ggplot(bmData, aes(x=default,fill=y))+ geom_bar(position = 'fill')+theme_bw()+xlab("default") + ylab("frequency")
p5<-ggplot(bmData, aes(x=housing,fill=y))+ geom_bar(position = 'fill')+theme_bw()+xlab("housing") + ylab("frequency")
p6<-ggplot(bmData, aes(x=loan,fill=y))+ geom_bar(position = 'fill')+theme_bw()+xlab("loan") + ylab("frequency")
p7<-ggplot(bmData, aes(x=contact,fill=y))+ geom_bar(position = 'fill')+theme_bw()+xlab("contact") + ylab("frequency")
p8<-ggplot(bmData, aes(x=month,fill=y))+ geom_bar(position = 'fill')+theme_bw()+xlab("month") + ylab("frequency")
p9<-ggplot(bmData, aes(x=day_of_week,fill=y))+ geom_bar(position = 'fill')+theme_bw()+xlab("day of week") + ylab("frequency")
p10<-ggplot(bmData, aes(x=poutcome,fill=y))+ geom_bar(position = 'fill')+theme_bw()+xlab("outcome") + ylab("frequency")
fig1<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, labels = 1:2, common.legend = TRUE, legend = "bottom")
fig1
annotate_figure(fig1, bottom  = text_grob("Subscription Percentage in categorical variables", col = "blue", face = "bold", size = 14))
# Load necessary library
library(gmodels)
# Create cross-tabulation between job and marital status
job_marital_cross <- table(bmclean$job, bmclean$marital)
# Create a color palette
my_colors <- c("lightblue", "lightgreen", "orange", "lightyellow", "violet")
# Create a mosaic plot with color
mosaicplot(job_marital_cross, main = "Cross-tabulation of Job and Marital Status", color = my_colors)
# Create cross-tabulation between job and age status
job_contact_cross <- table(bmclean$job, bmclean$contact)
# Create a color palette
my_colors <- c("yellow", "purple")
# Create a mosaic plot with color
mosaicplot(job_contact_cross, main = "Cross-tabulation of Job and Age", color = my_colors)
# Job Distribution with respect to contact
ggplot(YSub, aes(job)) + geom_bar(aes(fill = contact))
#Change scientific notation
options(scipen = 999)
# Correlation test of Age to the 'y' variable
ageTermDeposit <-
  cor.test(as.numeric(as.factor(bmData$y)),
           as.numeric(as.factor(bmData$age)),
           method = "pearson")
ageTermDeposit
# Correlation test of Job to the 'y' variable
jobTermDeposit <-
  cor.test(as.numeric(as.factor(bmData$y)),
           as.numeric(as.factor(bmData$job)),
           method = "pearson")
jobTermDeposit

# Correlation test of Marital to the 'y' variable
maritalTermDeposit <-
  cor.test(as.numeric(as.factor(bmData$y)),
           as.numeric(as.factor(bmData$marital)),
           method = "pearson")
maritalTermDeposit

eduTermDeposit <-
  cor.test(as.numeric(as.factor(bmData$y)),
           as.numeric(as.factor(bmData$education)),
           method = "pearson")
eduTermDeposit

# Correlation test of Housing to the 'y' variable
housingTermDeposit <-
  cor.test(as.numeric(as.factor(bmData$y)),
           as.numeric(as.factor(bmData$housing)),
           method = "pearson")
housingTermDeposit

# Correlation test of Loan to the 'y' variable
loanTermDeposit <-
  cor.test(as.numeric(as.factor(bmData$y)),
           as.numeric(as.factor(bmData$loan)),
           method = "pearson")
loanTermDeposit

# Correlation test of Housing and Loan to the 'y' variable
housingLoanTermDeposit <-
  cor.test(as.numeric(as.factor(bmData$y)),
           as.numeric(as.factor(bmData$housing)) +
             as.numeric(as.factor(bmData$loan)),
           method = "pearson")
housingLoanTermDeposit
# Training and Testing the data set
set.seed(12345)
sampleData <-
  sample(
    x = 1:nrow(bmData),
    size = 0.8 * nrow(bmData),
    replace = F
  )
trainData <- bmData[sampleData, ]
testData <- bmData[-sampleData, ]
sapply(bmData, class)

# CART
bankCART <- rpart(y ~ ., data = trainData, method = "class",
                    control = rpart.control(maxdepth = 5, minsplit = 10, minbucket = 5))

par(mfrow = c(1, 1))
fancyRpartPlot(bankCART ,
               digits = 2 ,
               palettes = c("Purples", "Reds"))

cartPred <- predict(bankCART , testData , type = "class")
cartProb <- predict(bankCART , testData , type = "prob")
confusion_matrix<-confusionMatrix(as.factor(testData$y), as.factor(cartPred))
confusion_matrix
#Cross-Table-Decision-Tree
CrossTable(
  testData$y,
  cartPred,
  prop.chisq = FALSE,
  prop.c = FALSE,
  prop.r = FALSE,
  dnn = c('actual default', 'predicted default')
)
# Fine-tune the decision tree model based on the confusion matrix
# Define confusion matrix
confusion_matrix <- matrix(c(7020, 295, 429, 494), nrow = 2, byrow = TRUE,
                           dimnames = list(Reference = c("no", "yes"),
                                           Prediction = c("no", "yes")))
# Extract True Negatives (TN) and False Positives (FP) from the confusion matrix
TN <- confusion_matrix[1, 1]  # True Negatives
FP <- confusion_matrix[1, 2]  # False Positives
# Extract False Negatives (FN) and True Positives (TP) from the confusion matrix
FN <- confusion_matrix[2, 1]  # False Negatives
TP <- confusion_matrix[2, 2]  # True Positives
# Calculate specificity
Specificity <- TN / (TN + FP)
# Calculate sensitivity
Sensitivity <- TP / (TP + FN)
# Calculate overall accuracy
Accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
# Calculate precision
Precision <- TP / (TP + FP)
# Calculate F1 score
F1_score <- 2 * (Precision * Sensitivity) / (Precision + Sensitivity)
# Print the evaluation metrics
cat("Sensitivity:", Sensitivity, "\n")
cat("Accuracy:", Accuracy, "\n")
cat("Precision:", Precision, "\n")
cat("F1 Score:", F1_score, "\n")
cat("F1 Score:", F1_score, "\n")
# Define the cross-validation method with stratified sampling
train_control <- trainControl(method = "cv", 
                              number = 1000,  # Number of folds
                              classProbs = TRUE,  # For class probabilities
                              summaryFunction = twoClassSummary)  # For two-class classification

# Train the model using cross-validation
model <- train(y ~ ., data = trainData, 
               method = "rpart",  
               trControl = train_control)
# Print the cross-validated results
print(model)
# KNN
bank.knn <- train(
  y ~ .,
  data = trainData,
  method = "knn",
  maximize = TRUE,
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("center", "scale")
)

predictedkNN <- predict(bank.knn , newdata = testData)
confusionMatrix(as.factor(predictedkNN) , as.factor(testData$y))
# Cross Table - KNN
CrossTable(
  testData$y,
  predictedkNN,
  prop.chisq = FALSE,
  prop.c = FALSE,
  prop.r = FALSE,
  dnn = c('actual default', 'predicted default')
)

# Decision Tree Classification
decisionTree <-
  rpart(formula = y ~ .,
        data = trainData,
        method = "class")

# Decision Tree Plot
prp(
  decisionTree,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Decision Tree"
)

# Predict the Test data by Probability
pred.DT <-
  predict(decisionTree, newdata = testData[-21], type = 'prob')
pred.DT

# Prediction - Decision Tree
predDT <- data.frame(y = testData$y, pred = NA)
predDT$pred <- pred.DT[, 2]
predDT

rocr.pred <-
  prediction(predictions = pred.DT[, 2], labels = testData$y)
rocr.perf <-
  performance(rocr.pred, measure = "tpr", x.measure = "fpr")
rocr.auc <- as.numeric(performance(rocr.pred, "auc")@y.values)
# ROC AUC
rocr.auc
# Plot ROC Curve
plot(
  rocr.perf,
  lwd = 3,
  colorize = TRUE,
  print.cutoffs.at = seq(0, 1, by = 0.1),
  text.adj = c(-0.2, 1.7),
  main = 'ROC Curve'
)
mtext(paste('Decision Tree - auc : ', round(rocr.auc, 5)))
abline(0, 1, col = "tomato", lty = 2)

rpart.plot(decisionTree)
pred <- predict(decisionTree, testData[-21], type = "class")
confusionMatrix(as.factor(testData$y), as.factor(pred))

# Logistic Regression Model
logRegModel <-
  glm(y ~ .,
      family = binomial(link = "logit"),
      data = bmclean)
LogRegPred <- predict(logRegModel, testData)
# Probability
prob <-
  (exp(logRegModel$coefficients[1])) / (1 + exp(logRegModel$coefficients[1]))
prob

# Random Forest
rfModel <- train(y ~ .,
                 data = trainData,
                 method = "rf",
                 ntree = 20)

refPred <- predict(rfModel, testData)
confusionMatrix(as.factor(testData$y), as.factor(refPred))


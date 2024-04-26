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
require(grDevices)
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
# Observing the structure of the dataset
str(bmclean)
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
# Frequency Tables for Categorical Variables
table(bmData$poutcome, bmData$y)
table(bmData$contact, bmData$y)
table(bmData$education)
table(bmData$default)
table(bmData$housing)
table(bmData$month)
install.packages()
library(psych)
library(ggplot2)
library(GGally)
# Investigating the inferential part of the dataset
describeData(bmclean)
# Assigning the subscribers to x 
YSub <- filter(bmData, y == "yes")
# Correlation of Numeric Variables
numeric_var <- sapply(bmclean, is.numeric)
cormat <- cor(bmclean[,numeric_var])
ggcorrplot::ggcorrplot(cormat, title = "Correlation of Numeric Variables")
# Job distribution with respect to y
ggplot(bmData) + geom_bar(mapping= aes(x= job,fill = y))+theme_light()
# Job Distribution with respect to contact
ggplot(YSub, aes(job)) + geom_bar(aes(fill = contact))
# Age Histogram
hist(
  bmData$age,
  main = "Histogram Plot - Age",
  xlab = "Age",
  ylab = "Frequency ",
  border = "black",
  xlim = c(0, 100),
  ylim = c(0, 10000),
  col = "violet"
)
# Age ~ Marital Status Histogram
ggPlot <- ggplot (bmclean)
plot1 <- ggPlot + geom_histogram(aes(x = age),
                                 color = "black",
                                 fill = "lightyellow",
                                 binwidth = 3) +
  ggtitle('Age Distribution') +
  ylab('Count') +
  xlab('Age') +
  geom_vline(aes(xintercept = mean(age), color = "tomato")) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  theme(legend.position = "none")

# Age ~ Marital Status Box plot
plot2 <- ggPlot + geom_boxplot(aes(y = age)) +
  ggtitle('Age Boxplot') +
  ylab('Age')

grid.arrange(plot1, plot2, ncol = 2, nrow = 1)

# Age ~ Marital Status Histogram plot
p3 <- ggplot(bmclean, aes(x = age, fill = marital)) +
  geom_histogram(binwidth = 2, alpha = 0.7) +
  facet_grid(cols = vars(y)) +
  expand_limits(x = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  ggtitle("Age vs Marital Status")

p3

meanAge <-
  bmclean %>% group_by(y) %>% summarize(grp.mean = mean(age))
# Age ~ Subscription Status Histogram
ggplot (bmclean, aes(x = age)) +
  geom_histogram(color = "black",
                 fill = "lightgreen",
                 binwidth = 3) +
  facet_grid(cols = vars(y)) +
  ggtitle('Age vs Subscription') + ylab('Count') + xlab('Age') +
  scale_x_continuous(breaks = seq(0, 100, 15)) +
  geom_vline(
    data = meanAge,
    aes(xintercept = grp.mean),
    color = "red",
    linetype = "dashed"
  )
# Education ~ Subscription Status Barplot
ggplot(data = bmData, aes(x = education, fill = y)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription - Education Level") +
  xlab(" Education Level") +
  guides(fill = guide_legend(title = "Subscription"))
# Age ~ Duration Status Scatter plot
ggplot(data = bmclean, aes(age, duration)) +
  geom_point() +
  facet_grid(cols = vars(y)) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  ggtitle("Scatterplot of Duration vs Age")

# Campaign ~ Duration Status Scatter plot
bmclean %>% filter(campaign < 63) %>%
  ggplot(aes(campaign, duration)) +
  geom_point() +
  facet_grid(cols = vars(y)) +
  ggtitle("Scatterplot of Duration vs Campaign")
install.packages()
library(egg)
library(tidyverse)
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
b1<-ggplot(bmData, aes(y= age, x = "", fill = y)) +geom_boxplot() + xlab(" ")
b2<-ggplot(bmData, aes(y= duration, x = "", fill = y))+geom_boxplot() + xlab(" ")
fig2<-ggarrange(b1, b2, labels = 1:2, common.legend = TRUE, legend = "bottom")
fig2
annotate_figure(fig2, bottom  = text_grob("Subscription Percentage in numeric variables", col = "maroon", face = "bold", size = 14))    
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
library(caTools)
# Training and Testing the data set
set.seed(12345)
sampleData <-
  sample(
    x = 1:nrow(bmData),
    size = 0.8 * nrow(bmData),
    replace = F
  )
trainData <- bmData[sampleData, ]
head(trainData)
testData <- bmData[-sampleData, ]
head(testData)

sapply(bmData, class)

# CART
bankCART <- rpart(y ~ ., trainData , method = 'class')
par(mfrow = c(1, 1))
fancyRpartPlot(bankCART ,
               digits = 2 ,
               palettes = c("Purples", "Reds"))

cartPred <- predict(bankCART , testData , type = "class")
cartProb <- predict(bankCART , testData , type = "prob")

confusionMatrix(as.factor(testData$y), as.factor(cartPred))
CrossTable(
  testData$y,
  cartPred,
  prop.chisq = FALSE,
  prop.c = FALSE,
  prop.r = FALSE,
  dnn = c('actual default', 'predicted default')
)

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
logRegModel
summary(logRegModel)

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
install.packages()
library(ggcorrplot)
model.matrix(~0+., data=bmData) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

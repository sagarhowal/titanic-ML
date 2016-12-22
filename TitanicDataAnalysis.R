
#Setting dataframes from csv files of Titanic Data from Kaggle
train <- read.csv('train.csv', header = TRUE)
test <- read.csv('test.csv', header = TRUE)

#Adding 'Survived' column to to Test data to combine both datasets
test.survived <- data.frame(Survived = rep('None', nrow(test)), test[,])

#Combining Data
data.combined <- rbind(train, test.survived) 

#checking datatypes in interpreter  (Unnecessary for execution)
str(data.combined)

#The Survived row in the Data is 0=Perished and 1=Alive, hence it cannot be an int or char.
#Imentioned char since we have added a "None" to it earlier on line 7
#Hence Survived row is converted to Factor type [Use "?factor" for details]
data.combined$Survived <- as.factor(data.combined$Survived)

#The row Pclass in the data is the Class of the Ticket, i.e. 1st, 2nd or 3rd. 
#Hence it cannot be taken as an int value since we do not require the algorithm to
#perform mathematical operations on it.
#We change the type of Pclass data to Factor type
data.combined$Pclass <- as.factor(data.combined$Pclass)

#Looking at survival rate
table(data.combined$Survived)

#Checking distribution across classes
table(data.combined$Pclass)

#Loading ggpolt2 library for data visualization
library(ggplot2)

#1st Hypothesis - More Rich people survived
#
#
train$Pclass <- as.factor(train$Pclass) #Converted Pclass of Train set to Factor

  #Plotting
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  stat_count(width = 0.5) +
  xlab("Pclass") + 
  ylab("Total Persons") + 
  labs(fill = "Survived") 

#checking first few names to determine the format of writing names (Unnecessary)
head(as.character(train$Name))

#checking unique names
length(unique(as.character(data.combined$Name))) #1307 out of 1309 unique names found

#checking duplicate names incase there is duplicate of bad data
#creating a dup.names variable containing all duplicate names 
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#Closer look at the duplicate names' data 
#Command -- In data.combined pulling out all the data of names which are also in dup.names
data.combined[which(data.combined$Name %in% dup.names),]


#Loading stringr library
#stringr libaray gives lots of flexibility in finding patters in strings
library(stringr)

#checking unmarried woamen's entires
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:10,] 
    #By checking first 10 entries it is observed that unmarried feamles had a approx. 70% chance of survival
    #Lets also check for Married women

#Checking married women's Entires
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses [1:10,]
    #It is observed that married women had approx. 80% chance of survival based on first 10 entries

#Checking survival characteristics for male passengers

males <- data.combined[which(train$Sex == "male"),]
males [1:10,]
    #First 10 entries show that males have approx. 10% chance of survival

#Exploring the relationship between Title and Pclass and survival of the passenger
#We create another column names Title by extracting the string from the Name column

#creating a utility function to extract title
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) >0) {
    return("Miss.")
  } else if (length(grep("Mrs.", Name)) >0) {
    return("Mrs.")
  } else if (length(grep("Mr.", Name)) >0) {
    return("Mr.")
  } else if (length(grep("Master.", Name))>0) {
    return("Master.")
  } else {
    return("Other")
  }
}

Titles <- NULL 
  for (i in 1:nrow(data.combined)) {
    Titles <- c(Titles, extractTitle(data.combined[i, "Name"]))
  }
data.combined$Titles <- as.factor(Titles)


#Creating a visualization co-relating Title and Pclass and how it affected the survival rate
#Since only the first 891 entries in the data.combined variable have survival rate defined,
#We create a visualization only for those fir 891 entires. 

ggplot(data.combined[1: nrow(train),], aes(x=Titles, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("PClass") + 
  xlab("Titles") + 
  ylab("Total Count") + 
  labs(fill = "Survived")
 
#checking Male:Female Ratio
table(data.combined$Sex)

#Realtionship between Sex, Class and Survival
ggplot(data.combined[1:nrow(train),], aes(x = Sex, fill = Survived)) +
  stat_count(width = 0.5) + 
  facet_wrap(~Pclass) + 
  xlab("Sex") +
  ylab("Total Count") + 
  labs(fill = "survived") 

#Checking age distribution 
summary(data.combined$Age)
summary(data.combined[1:nrow(train),"Age"])

#Plotting viz for sex, class and age
ggplot(data.combined[1:nrow(train),], aes(x = Age, fill = Survived)) + 
  facet_wrap(~Sex + Pclass) + 
  geom_histogram(binwidth = 02) + 
  xlab("Age") + 
  ylab("Total Count") 

#Setting "Master." Title to male children
boys <- data.combined[which(data.combined$Titles == "Master."),]
summary(boys$Age)

#examining miss since it contains unmarried girls and female babies too. 
misses <- data.combined[which(data.combined$Titles == "Miss.")]
summary(misses$Age)


#Plotting
ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) + 
  stat_count(width = 5) + 
  xlab("Age") +
  ylab("Total Count") + 
  ggtitle("Age for Miss. by Pclass")


#Checking misses who travelled alone
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),] 
summary(misses.alone$Age) 
length(which(misses.alone$Age <= 14.5))


ggplot(misses.alone[misses.alone$Age >= 3 & misses.alone$Survived != "None",], aes(x = Age, fill = Survived)) + 
  stat_count(width = 1) + 
  facet_wrap(~Pclass) + 
  xlab("Age") + 
  ylab("Total") + 
  ggtitle("Females survival below 14.5 Years of Age")
  

#Summary of Siblings and Spouses
summary(data.combined$SibSp)

#Number of Unique valeus in this data
length(unique(data.combined$SibSp))


#Converting Sibsp into a Factor type 
data.combined$SibSp <- as.factor(data.combined$SibSp)

#Visualize survival rates by Sibsp, Title and Class
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) + 
  stat_count(width = 1) + 
  facet_wrap(~Pclass + Titles) + 
  ggtitle("Pclass, Titles") + 
  xlab("Siiblings and Spouse Aboard") + 
  ylab("Total Count") + 
  ylim(0,300) + 
  labs(fill = "Survived")

#Changing the Parch data as factor and visualizing similarly

data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:nrow(train),], aes(x = Parch, fill = Survived)) + 
  stat_count(width = 0.5) + 
  facet_wrap(~Pclass + Titles) + 
  ggtitle("Passenger Class and titles") + 
  xlab("Parent/children Aboard") + 
  ylab("Total Count") + 
  ylim(0,300) + 
  labs(fill = "Survived") 

#Feature engineering: Create a Family Size feature
temp.Sibsp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)

data.combined$FamilySize <- as.factor(temp.Sibsp + temp.Parch + 1)

#Visualizing this
ggplot(data.combined[1:nrow(train),], aes(x = FamilySize, fill = Survived)) + 
  stat_count(width = 1) + 
  facet_wrap(~Pclass + Titles) + 
  xlab("Family size") +
  ylab("Total Count") + 
  ggtitle("Pclass, title") + 
  ylim(0,300) + 
  labs("Survived") 

#Exploring Ticket variable
str(data.combined$Ticket)

#Ticket is a factor with 929 vairables. 
#Convert Ticket variable to Character
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

#The data in Ticket Variable does not look structured. 
#Exploring the data to find patterns
Ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(Ticket.first.char)

#adding the Ticket.first.char variable to data.combined dataset
data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

#Checking survivability w.r.t ticket's first character. 
ggplot(data.combined[1:nrow(train),], aes(x = Ticket.first.char, fill = Survived)) + 
  geom_bar() +
  ggtitle("Survivability according to Ticket's first letter") + 
  xlab("Ticket First character") + 
  ylab("Total Count") + 
  ylim(0,350) +
  labs(fill = "Survived")

#Drilling a little deeper

ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) + 
  geom_bar() + 
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") + 
  xlab("Ticket.first.char") + 
  ylab("Total Count") + 
  labs(fill = "Survived")+
  ylim(0,150)

#Visualizing wrt Pclass and title
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) + 
  geom_bar() + 
  facet_wrap(~Pclass + Titles) + 
  ggtitle("Pclass, Titles") + 
  xlab("Ticket.first.char") + 
  ylab("Total Count") + 
  labs(fill = "Survived")+
  ylim(0,200)

#Ticket number does not  reveal any good reliability. We keep it aside. 

#Testing the fare variable
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#Visualizing fare with histogram
ggplot(data.combined, aes(x = Fare)) + 
  geom_histogram(binwidth = 5 ) + 
  xlab("Fares") +
  ylab("Total Count") + 
  ggtitle("Fares Distribution Histogram") + 
  ylim(0,200)

#Visualizing fares wrt Class and titles
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) + 
  geom_histogram(binwidth = 5) + 
  facet_wrap(~Pclass + Titles) + 
  ggtitle("Pclass, Titles") + 
  xlab("Ticket.first.char") + 
  ylab("Total Count") + 
  labs(fill = "Survived")+
  ylim(0,50)

#Moving on to Cabins

#Analyzing the Cabin variable 
str(data.combined$Cabin)

#Converting Cabin Variable from Factor to Character
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#Fixning the Cabin Variable
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

#Seperating the first character of the Cabin
Cabin.first.char <- as.factor(substr(data.combined$Cabin, 1,1))
str(Cabin.first.char)
levels(Cabin.first.char)
 #Combine in data.combined
data.combined$Cabin.first.char <- Cabin.first.char
data.combined$Cabin.first.char[1:100]

#Visualizing survival by Cabin 
ggplot(data.combined[1:nrow(train),], aes(x = Cabin.first.char, fill = Survived)) + 
  geom_bar() + 
  ggtitle("Survival by Cabin Position") + 
  xlab("Cabin First character") + 
  ylab("Total Count") + 
  labs(fill = "Survived") + 
  ylim(0,750)

#Survival by Cabin wrt Pclass
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) + 
  stat_count(width = 1) + 
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") + 
  xlab("Cabin.first.char") + 
  ylab("Total Count") + 
  labs(fill = "Survived")+
  ylim(0,500)


#Survival by Cabin wrt Class and titles
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) + 
  stat_count(width = 1) + 
  facet_wrap(~Pclass + Titles) + 
  ggtitle("Pclass, Titles") + 
  xlab("Ticket.first.char") + 
  ylab("Total Count") + 
  labs(fill = "Survived")+
  ylim(0,350)

#Investigating people with multiple Cabins
data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived)) + 
  stat_count(width = 1) + 
  facet_wrap(~Pclass + Titles) + 
  ggtitle("Pclass, Titles") + 
  xlab("Cabin.multiple") + 
  ylab("Total Count") + 
  labs(fill = "Survived")+
  ylim(0,350)


#Embarked variable 
str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) + 
  stat_count(width = 1) + 
  facet_wrap(~Pclass + Titles) + 
  ggtitle("Pclass, Titles") + 
  xlab("Embarked") + 
  ylab("Total Count") + 
  labs(fill = "Survived")+
  ylim(0,350)


#-----------------------------------------------------
#               Exploratory Modelling
#-----------------------------------------------------

library(randomForest)

#Training a Random forest using Pclass and titles
rf.train.1 <- data.combined[1:nrow(train), c("Pclass","Titles")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1 
varImpPlot(rf.1)


#Training a Random Forest with Pclass, Titles and SibSp
rf.train.2 <- data.combined[1:nrow(train), c("Pclass", "Titles", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)


#Training a Random Forest with Pclass, Titles and Parch
rf.train.3 <- data.combined[1:nrow(train), c("Pclass", "Titles", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

#Again with Pclass, Titles, Parch and Sibsp
rf.train.4 <- data.combined[1:nrow(train), c("Pclass", "Titles", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)
 

#Same with Family Size which is a reduced dimension variable for SibSp and Parch
rf.train.5 <- data.combined[1:nrow(train), c("Pclass", "Titles", "FamilySize")]


set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

#Same with SibSp as well as FamilySize
rf.train.6 <- data.combined[1:nrow(train), c("Pclass", "Titles", "SibSp", "FamilySize")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

#Same with Parch and Family size
rf.train.7 <- data.combined[1:nrow(train), c("Pclass", "Titles", "Parch", "FamilySize")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

#We have an initial model (rf.5) with accuracy of 81.59%. 
#We shall generate the output of this Random Forest model and store it in a csv file. 

#Subset our test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "Titles", "FamilySize")]

#Make Predictions 
rf.5.pred <- predict(rf.5, test.submit.df) 
table(rf.5.pred)

#Write out a CSV file 
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.pred)
write.csv(submit.df, file = "OP_RF_30112016_1.csv", row.names = FALSE )


#---------------------------------------------------------------------
#            Beginning using Cross Validation
#---------------------------------------------------------------------

#Caret package will be used for Cross Validation
#Installing and initiating Caret Package and doSNOW package in RStudio
library(caret)
library(doSNOW)

#Using k-folds method for cross validation.
#It is a healthy practice to use 10-folds CV 10 times. That is a total of 100 folds.
#This gives a very good amount of cross validation and Accuraacy 

#We use caret package to create 100 folds and ensure that the ratio of Survived and Perished
#in each fold is the same as the overall training set. 
#This is known as stratified cross validation. 
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

#Check Stratification
table(rf.label)
342/549     # = 0.6229

table(rf.label[cv.10.folds[[33]]])
308/494     # = 0.6234

#setup a TraicControl object using caret
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

#Using doSNOW package for multi-core training since there are a lot of trees to be trained
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

#Set Seed for reproducing and training
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

#Shut down the cluster
stopCluster(cl)

#Check results
rf.5.cv.1

#This prediction is less aaccurate than rf.5 variable therefore, we have overfit
#We reduce k value to 5 
#5-fold CV repeated 10 times

set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)

cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

#Stop Cluster
stopCluster(cl)

#Check
rf.5.cv.2


#5 fold is not as good. Trying 3 fold


set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)

cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.3)

#Stop Cluster
stopCluster(cl)

#Check
rf.5.cv.3


#--------------------------------------------------------------
#            Exploratory Modelling 2 
#--------------------------------------------------------------


#USing a single tree to try to understand what is going on with the features. 
#Random forest are ore powerful but single trees are easier to understand. 

#install and load required packages
library(rpart)
library(rpart.plot)

#As observed before.. Using 3 fold cv repeated 10 times

#Creating a utility function for it. 

rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(2, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  #Leverage formula interfacing for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, trControl = ctrl)
  
  #Stop Cluster
  stopCluster(cl)
  
  return(rpart.cv)
}

#grab features
features <- c("Pclass", "Titles", "FamilySize")
rpart.train.1 <- data.combined[1:nrow(train), features]

#Run cross validation and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

#Plotting the tree
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#Understanding the plot------------------------------------------------------------------
#   1 - Titles of Mr. and Others are predicted to perish at an overall accuracy of 83.2%
#
#   2 - Titles of Master, Miss and Mrs. in 1st and 2nd class is considered survived with 
#       an overall accuracy of 94.9%
#
#   3 - Titles with Master, Miss and Mrs. in the 3rd class with the family size of 5, 6, 
#       8 and 11 are predicted to NOT survived with an accuracy of 100%
#
#   4 - Titles with Master, Miss and Mrs. in the 3rd class with the family size NOT of 5, 
#       6, 8 and 11 are predicted to have survived with an accuracy of 59.6%
#----------------------------------------------------------------------------------------

#Both rf and rpart confirm that titles is quite important. 
#Further investigation
table(data.combined$Titles)

#Parse out Last name and Titles
data.combined[1:25, "Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.slpits, "[", 1)
last.names[1:10]

#Adding Last names column to the data.combined just incase we need it in the future
data.combined$Last.names <- last.names

#Now investigating titles, especially the others. titles. 
#splitting out titles from names
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits,"[", 2)
unique(titles)

#Checking the title "The"
data.combined[which(titles == "the"),]

#Re-mapping titles 
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

#Cram new titles into data.combined.
data.combined$New.titles <- as.factor(titles)


#Visualize new titles
ggplot(data.combined[1:nrow(train),], aes(x = New.titles, fill = Survived)) + 
  stat_count(width = 0.8) + 
  facet_wrap(~Pclass) + 
  ggtitle("Survival Viz for new Titles wrt Class")


#collapse titles based on Visual analysis
indexes <- which(data.combined$New.titles == "Lady.")
data.combined$New.titles[indexes] <- "Mrs."

indexes <- which(data.combined$New.titles == "Dr." |
                  data.combined$New.titles == "Rev." |
                  data.combined$New.titles == "Sir." | 
                  data.combined$New.titles == "Officer")
data.combined$New.titles[indexes] <- "Mr."

#Visualize new titles
ggplot(data.combined[1:nrow(train),], aes(x = New.titles, fill = Survived)) + 
  stat_count(width = 0.8) + 
  facet_wrap(~Pclass) + 
  ggtitle("Survival Viz for new Titles wrt Class")

#Grab features and build a tree
features <- c("Pclass", "New.titles", "FamilySize")
rpart.train.2 <- data.combined[1:891, features]

#Run Cross validation and test result
rpart.2.cv.1 <- rpart.cv(96422, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

#Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#diving into 1st class Mr. 
indexes.first.mr <- which(data.combined$New.titles == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

#Summary shows there is one female in Mr. Huh? 
first.mr.df[first.mr.df$Sex == "female",]

#Shifting Dr Alice Leader to Mrs. 
indexes <- which(data.combined$New.titles == "Mr." & data.combined$Sex == "female")
data.combined$New.titles[indexes] <- "Mrs."

#Checking for any other gender slip-ups
length(which(data.combined$Sex == "female" & 
               (data.combined$New.titles == "Master." | data.combined$New.titles == "Mr.")))


#Refreshing the Mr. 
indexes.first.mr <- which(data.combined$New.titles == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

#checking
summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])

# Take a look at some of the high fares
indexes <- which(data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760")
View(data.combined[indexes,])







#Library imports
library(dplyr)
library(plyr)
library(sqldf)
library(ggplot2)
library(corrplot)
library(ggfortify)
library(ggcorrplot)
library(stringr)

#Setting Working Directory 
setwd("~/Kaggle/Titanic")

#Importing datasets
train <- read.csv("train.csv",stringsAsFactors = FALSE,na.strings=c("","NA"))
test <- read.csv("test.csv", stringsAsFactors = FALSE,na.strings=c("","NA"))

#Taking a look at Datasets
View(train)
View(test)


#Summary of Datasets
summary(train)
summary(test)

#Observations:
#Age has 177 NA values
# We need to find NA values of Charater fields as well
sum(is.na(train$Cabin)) #687 values with NA
sum(is.na(train$Embarked)) #2 values with NA

str(train) # 891 observations  with 12 variables
str(test) # 418 observations with 11 variables (Survived is missing)


#Objective to find whether a person survived or not.
#We need to establish relationships between survival and other attributes
survived <- train$Survived[which(train$Survived==1)]
not_survived <- train$Survived[which(train$Survived==0)]
percent_survived <- (as.numeric(length(survived))/(as.numeric(length(survived))+as.numeric(length(not_survived))))*100
percent_not_survived <- (as.numeric(length(not_survived))/(as.numeric(length(survived))+as.numeric(length(not_survived))))*100



class_survival <-
  sqldf(
    '  select Pclass as "Pass_Class",
    Survived as "Sur_ind",
    count(Survived) as "Count"
    from train  group by Pclass, Survived '
  )

class_survival <- merge(class_survival,aggregate(class_survival$Count, by= list(class_survival$Pass_Class),FUN=sum),by.x = "Pass_Class",by.y = "Group.1")

class_survival$pct <- (round(class_survival$Count/class_survival$x,4)*100)
class_survival$x <- NULL


ggplot(class_survival,aes(x=Pass_Class,y=pct,fill=factor(Sur_ind))) + 
  geom_bar(stat = "identity",position = "dodge") +
labs(x="Passenger Class", y="Percentage of Survival") +  labs(fill="Survival Indicator") +
  ggtitle("Percentage of Survival Under Each Class")


#We identified the percentage of survival as per class
#Proceeding to find survival percentage by gender

gender_survival <-  sqldf(
  '  select Sex as "Gender",
    Survived as "Sur_ind",
    count(Survived) as "Count"
    from train  group by Sex, Survived '
)

gender_survival <- merge(gender_survival,aggregate(gender_survival$Count, by= list(gender_survival$Gender),FUN=sum),by.x = "Gender",by.y = "Group.1")
gender_survival$pct <- (round(gender_survival$Count/gender_survival$x,4)*100)
gender_survival$x <- NULL


ggplot(gender_survival,aes(x=Gender,y=pct,fill=factor(Sur_ind))) + 
  geom_bar(stat = "identity",position = "dodge") +
  labs(x="Gender", y="Percentage of Survival") +  labs(fill="Survival Indicator") +
  ggtitle("Percentage of Survival Under Each Gender")


#Gender under each class 
gender_class <-  sqldf(
  '  select Sex as "Gender",
    Pclass as "Class",
    count(Sex) as "Count"
    from train  group by Sex, Pclass '
)

gender_class <- merge(gender_class,aggregate(gender_class$Count, by= list(gender_class$Class),FUN=sum),by.x = "Class",by.y = "Group.1")
gender_class$pct <- (round(gender_class$Count/gender_class$x,4)*100)
gender_class$x <- NULL



ggplot(gender_class,aes(x=Class,y=pct,fill=factor(Gender))) + 
  geom_bar(stat = "identity",position = "stack") +
  labs(x="Class", y="Gender Percentage") +  labs(fill="Gender") +
  ggtitle("Percentage of Gender Under Each Class")
#We find more males in 3rd Class

#Plotting survival chances by gender and class

survival_ratio <-  sqldf(
  '  select Sex as "Gender",
    Pclass as "Class",
    count(Survived) as "Count"
    from train  where Survived==1 group by Sex, Pclass '
)

survival_ratio

survival_ratio <- merge(survival_ratio,aggregate(train$Survived,by= list(train$Pclass,train$Sex),FUN=length),by.x = c("Gender","Class"),by.y = c("Group.2","Group.1"))
survival_ratio$pct <- (round(survival_ratio$Count/survival_ratio$x,4)*100)
survival_ratio$x <- NULL


ggplot(survival_ratio,aes(x=Class,y=pct,fill=factor(Gender))) + 
  geom_bar(stat = "identity",position = "dodge") +
  labs(x="Class", y="Gender Percentage") +  labs(fill="Gender") +
  ggtitle("Percentage of Gender Under Each Class")

#Women from first and second class have ~95% survival chances
#Males from second and third class have ~10% survival chances

########################################################################################################
#checking based on embarked status

train$Embarked
embarked_survival <-
  sqldf(
    'select Pclass as "Pass_Class",
    Sex as "Gender",
    Embarked as "Embarked",
    count(Survived) as "Count"
    from train where Survived==1  and Embarked in ("S","C","Q") group by Pclass, Sex , Embarked '
  )

embarked_survival <- merge(embarked_survival,aggregate(train$Survived,by= list(train$Pclass,train$Sex,train$Embarked),FUN=length),by.x = c("Pass_Class","Gender","Embarked"),by.y = c("Group.1","Group.2","Group.3"))
embarked_survival$pct <- (round(embarked_survival$Count/embarked_survival$x,4)*100)
embarked_survival$x <- NULL


ggplot(embarked_survival, aes(x=interaction(Pass_Class, Embarked), y=pct, fill=Gender)) +   
    geom_bar(position='Dodge', stat='identity')

#Almost all females from class 1 survived
#Females dying were mainly from 3rd Class
#Males from class 1 have slightly higher chance of survival

####################################################################################################

#Only embarked and Survival
embarked_survival <-  sqldf(
  '  select Embarked as "Embarked",
    Survived as "Sur_ind",
    count(Survived) as "Count"
    from train  group by Embarked, Survived '
)

embarked_survival <- merge(embarked_survival,aggregate(embarked_survival$Count, by= list(embarked_survival$Embarked),FUN=sum),by.x = "Embarked",by.y = "Group.1")
embarked_survival$pct <- (round(embarked_survival$Count/embarked_survival$x,4)*100)
embarked_survival$x <- NULL


ggplot(embarked_survival,aes(x=Embarked,y=pct,fill=factor(Sur_ind))) + 
  geom_bar(stat = "identity",position = "dodge") +
  labs(x="Embarked", y="Percentage of Survival") +  labs(fill="Survival Indicator") +
  ggtitle("Percentage of Survival Under Each Embarked Category")

###################################################################################################################

#Parch vs Survival
parch_survival <-  sqldf(
  '  select Parch as "Parch",
    Survived as "Sur_ind",
    count(Survived) as "Count"
    from train  group by Parch, Survived '
)

parch_survival <- merge(parch_survival,aggregate(parch_survival$Count, by= list(parch_survival$Parch),FUN=sum),by.x = "Parch",by.y = "Group.1")
parch_survival$pct <- (round(parch_survival$Count/parch_survival$x,4)*100)
parch_survival$x <- NULL


ggplot(parch_survival,aes(x=Parch,y=pct,fill=factor(Sur_ind))) + 
  geom_bar(stat = "identity",position = "dodge") +
  labs(x="Parch", y="Percentage of Survival") +  labs(fill="Survival Indicator") +
  ggtitle("Percentage of Survival Under Each Parch Category")

################################################################################################################################
train$SibSp
#SibSp vs. Survival
SibSp_survival <-  sqldf(
  '  select SibSp as "SibSp",
    Survived as "Sur_ind",
    count(Survived) as "Count"
    from train  group by SibSp, Survived '
)

SibSp_survival <- merge(SibSp_survival,aggregate(SibSp_survival$Count, by= list(SibSp_survival$SibSp),FUN=sum),by.x = "SibSp",by.y = "Group.1")
SibSp_survival$pct <- (round(SibSp_survival$Count/SibSp_survival$x,4)*100)
SibSp_survival$x <- NULL


ggplot(SibSp_survival,aes(x=SibSp,y=pct,fill=factor(Sur_ind))) + 
  geom_bar(stat = "identity",position = "dodge") +
  labs(x="SibSp", y="Percentage of Survival") +  labs(fill="Survival Indicator") +
  ggtitle("Percentage of Survival Under Each Parch Category")


#####################################################################################################

ggplot(train,aes(x=as.factor(Pclass),y=Age,color=as.factor(Survived))) + geom_boxplot() +
xlab("Passenger Class") + ylab(("Age")) + labs(color="Survived")

ggplot(train,aes(x=as.factor(Embarked),y=Age,color=as.factor(Survived))) + geom_boxplot() +
  xlab("Embarked") + ylab(("Age")) + labs(color="Survived")

ggplot(train,aes(x=as.factor(Sex),y=Age,color=as.factor(Survived))) + geom_boxplot() +
  xlab("Gender") + ylab(("Age")) + labs(color="Survived")


#1st Pclass has very few children as compared to other two classes.
#1st Plcass has more old people as compared to other two classes.
#Almost all children (between age 0 to 10) of 2nd Pclass survived.
#Most children of 3rd Pclass survived.
#Younger people of 1st Pclass survived as compared to its older people.
#Most male children (between age 0 to 14) survived.
#Females with age between 18 to 40 have better survival chance.


####################################################################################################
total_survived <- subset(train,Survived == 1)
total_not_survived <- subset(train,Survived == 0)

male_survived <- subset(train, Survived == 1 & Sex=='male')
female_survived <- subset(train, Survived == 1 & Sex=='female')
male_not_survived <- subset(train, Survived == 0 & Sex=='male')
female_not_survived <- subset(train, Survived == 0 & Sex=='female')

survival_age_gender <-
  sqldf(
    'select Age as "Age",
    Sex as "Gender",
    count(Survived) as "Count"
    from train where Survived==1  and Embarked in ("S","C","Q") group by Age, Sex'
  )

ggplot(survival_age_gender, aes(x = Age, y = Count, fill = Gender)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  coord_flip() +  # Flip axes
  labs(title="Email Campaign Funnel")+
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")  # Color palette


#We can see that children with age between 0 to 5 have better chance of survival.
#Females with age between "18 to 40" and "50 and above" have higher chance of survival.
#Males with age between 0 to 14 have better chance of survival

ggcorrplot(train_corel,method = "circle")

train_corel <- data.frame(train$Survived,train$Pclass,train$Age,train$SibSp,train$Parch,train$Fare)



ggcorrplot(cor(na.omit(train_corel)), p.mat = cor_pmat(train_corel),
           hc.order = TRUE, type = "upper",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE)
levelplot(cor(train_corel))


################################################################################################################################
#Feature Extraction


train_test_data = merge.data.frame(train, test)

train_feature <- train
train_feature$Title <- str_extract(train$Name," ([A-Za-z]+)\\.")

aggregate(train_feature$Title,by=list(train_feature$Sex,train_feature$Title),FUN=length)

#We can replace less common names with Others



train_feature$Title<- str_replace_all(train_feature$Title,"Miss.","Miss")
train_feature$Title<- str_replace_all(train_feature$Title,"Mlle.","Miss")
train_feature$Title<- str_replace_all(train_feature$Title,"Ms.","Miss")
train_feature$Title<- str_replace_all(train_feature$Title,"Mme.","Mtr")
train_feature$Title<- str_replace_all(train_feature$Title,"Mrs.","Mtr")
train_feature$Title<- str_replace_all(train_feature$Title,"Mr.","Mr")
train_feature$Title<- str_replace_all(train_feature$Title,"Lady.","Other")
train_feature$Title<- str_replace_all(train_feature$Title,"Countess.","Other")
train_feature$Title<- str_replace_all(train_feature$Title,"Capt.","Other")
train_feature$Title<- str_replace_all(train_feature$Title,"Col.","Other")
train_feature$Title<- str_replace_all(train_feature$Title,"Don.","Other")
train_feature$Title<- str_replace_all(train_feature$Title,"Dr.","Other")
train_feature$Title<- str_replace_all(train_feature$Title,"Major.","Other")
train_feature$Title<- str_replace_all(train_feature$Title,"Rev.","Other")
train_feature$Title<- str_replace_all(train_feature$Title,"Sir.","Other")
train_feature$Title<- str_replace_all(train_feature$Title,"Jonkheer.","Other")
train_feature$Title<- str_replace_all(train_feature$Title,"Dona.","Other")
train_feature$Title<- str_replace_all(train_feature$Title,"Master.","Master")
train_feature$Title<- str_replace_all(train_feature$Title,"Mtr","Mrs")

aggregate(train_feature$Title,by=list(train_feature$Sex,train_feature$Title),FUN=length)

Title_Mean_1 <-
  sqldf(
    'select
    Title as "Title",
    count(Survived) as "Count"
    from train_feature where Survived==1 group by Title'
  )

Title_Mean_2 <-
  sqldf(
    'select
    Title as "Title",
    count(Survived) as "Total"
    from train_feature  group by Title'
  )


Title_Mean <- merge(Title_Mean_1,Title_Mean_2,by.x = "Title",by.y = "Title")
Title_Mean$pct <- round(Title_Mean$Count/Title_Mean$Total,2) 
Title_Mean

train_feature$Title<- str_replace_all(train_feature$Title,"Mr","1")
train_feature$Title<- str_replace_all(train_feature$Title,"Miss","2")
train_feature$Title<- str_replace_all(train_feature$Title,"Mrs","3")
train_feature$Title<- str_replace_all(train_feature$Title,"Master","4")
train_feature$Title<- str_replace_all(train_feature$Title,"Other","5")
train_feature$Title<- str_replace_all(train_feature$Title,"1s","3")

train_feature$Title <- as.numeric(train_feature$Title)


#Similarly mapping Female to 1 and Male to 0
train_feature$Sex<- str_replace_all(train_feature$Sex,"female","1")
train_feature$Sex<- str_replace_all(train_feature$Sex,"male","0")
train_feature$Sex <- as.numeric(train_feature$Sex)

#Embarked
aggregate(train_feature$Embarked,by=list(train_feature$Embarked),FUN=length)
#Mapping NA to S
train_feature$Embarked[is.na(train_feature$Embarked)] <- "S"

train_feature$Embarked<- str_replace_all(train_feature$Embarked,"S","0")
train_feature$Embarked<- str_replace_all(train_feature$Embarked,"C","1")
train_feature$Embarked<- str_replace_all(train_feature$Embarked,"Q","2")
train_feature$Embarked <- as.numeric(train_feature$Embarked)


#Age

train_feature$Age <-
  ifelse(
    train_feature$Age <= 16,
    0,
    ifelse (
      train_feature$Age > 16 & train_feature$Age <= 32,
      1,
      ifelse (
        train_feature$Age > 32 & train_feature$Age <= 48,
        2,
        ifelse (
          train_feature$Age > 48 & train_feature$Age <= 64,
          3,
            4
          )
        )
      )
  )

train_feature$Age[is.na(train_feature$Age)] <- 4

#Fare
train_feature$Fare <-  ifelse(train_feature$Fare <= 7.91, 0,
    ifelse (train_feature$Fare > 7.91 & train_feature$Fare <= 14.454, 1,
      ifelse (train_feature$Fare > 14.454 & train_feature$Fare <= 31, 2,3)))
train_feature$Fare



#Creating a column called family size from parch and SibSP

train_feature$FamilySize <- train_feature$SibSp + train_feature$Parch + 1


FamilySize_Mean_1 <-
  sqldf(
    'select
    FamilySize as "FamilySize",
    count(Survived) as "Count"
    from train_feature where Survived==1 group by FamilySize'
  )

FamilySize_Mean_2 <-
  sqldf(
    'select
    FamilySize as "FamilySize",
    count(Survived) as "Total"
    from train_feature  group by FamilySize'
  )


FamilySize_Mean <- merge(FamilySize_Mean_1,FamilySize_Mean_2,by.x = "FamilySize",by.y = "FamilySize")
FamilySize_Mean$pct <- round(FamilySize_Mean$Count/FamilySize_Mean$Total,2) 
FamilySize_Mean
#Having FamilySize upto 4 (from 2 to 4) has better survival chance.
#FamilySize = 1, i.e. travelling alone has less survival chance.
#Large FamilySize (size of 5 and above) also have less survival chance.

train_feature$IsAlone <-  ifelse(train_feature$FamilySize == 1, 1,0)

#We can drop unesscary features which are not required in detemining the survival
train_feature$Name <- NULL
train_feature$SibSp <- NULL
train_feature$Parch <- NULL
train_feature$Ticket <- NULL
train_feature$Cabin <- NULL
train_feature$FamilySize <- NULL
train_feature$PassengerId <- NULL
train_feature$AgeBand <- NULL
train_feature$FareBand <- NULL

#We need to map age also similar to test_feature
test$Sex<- str_replace_all(test$Sex,"female","1")
test$Sex<- str_replace_all(test$Sex,"male","0")
test$Sex <- as.numeric(test$Sex)

test$Age <- ifelse(test$Age <= 16,0,
    ifelse (test$Age > 16 & test$Age <= 32,1,
      ifelse (test$Age > 32 & test$Age <= 48,2,
        ifelse (test$Age > 48 & test$Age <= 64,3,4))))

test$Age[is.na(test$Age)] <- 4


test$Fare <-  ifelse(test$Fare <= 7.91, 0,
                              ifelse (test$Fare > 7.91 & test$Fare <= 14.454, 1,
                                      ifelse (test$Fare > 14.454 & test$Fare <= 31, 2,3)))


test$Embarked[is.na(test$Embarked)] <- "S"

test$Embarked<- str_replace_all(test$Embarked,"S","0")
test$Embarked<- str_replace_all(test$Embarked,"C","1")
test$Embarked<- str_replace_all(test$Embarked,"Q","2")
test$Embarked <- as.numeric(test$Embarked)



test$Title <- str_extract(test$Name," ([A-Za-z]+)\\.")

aggregate(test$Title,by=list(test$Sex,test$Title),FUN=length)

#We can replace less common names with Others



test$Title<- str_replace_all(test$Title,"Miss.","Miss")
test$Title<- str_replace_all(test$Title,"Mlle.","Miss")
test$Title<- str_replace_all(test$Title,"Ms.","Miss")
test$Title<- str_replace_all(test$Title,"Mme.","Mtr")
test$Title<- str_replace_all(test$Title,"Mrs.","Mtr")
test$Title<- str_replace_all(test$Title,"Mr.","Mr")
test$Title<- str_replace_all(test$Title,"Lady.","Other")
test$Title<- str_replace_all(test$Title,"Countess.","Other")
test$Title<- str_replace_all(test$Title,"Capt.","Other")
test$Title<- str_replace_all(test$Title,"Col.","Other")
test$Title<- str_replace_all(test$Title,"Don.","Other")
test$Title<- str_replace_all(test$Title,"Dr.","Other")
test$Title<- str_replace_all(test$Title,"Major.","Other")
test$Title<- str_replace_all(test$Title,"Rev.","Other")
test$Title<- str_replace_all(test$Title,"Sir.","Other")
test$Title<- str_replace_all(test$Title,"Jonkheer.","Other")
test$Title<- str_replace_all(test$Title,"Dona.","Other")
test$Title<- str_replace_all(test$Title,"Master.","Master")
test$Title<- str_replace_all(test$Title,"Mtr","Mrs")


test$Title<- str_replace_all(test$Title,"Mr","1")
test$Title<- str_replace_all(test$Title,"Miss","2")
test$Title<- str_replace_all(test$Title,"Mrs","3")
test$Title<- str_replace_all(test$Title,"Master","4")
test$Title<- str_replace_all(test$Title,"Other","5")
test$Title<- str_replace_all(test$Title,"1s","3")

test$Title <- as.numeric(test$Title)

test$FamilySize <- test$SibSp + test$Parch + 1


test$IsAlone <-  ifelse(test$FamilySize == 1, 1,0)




test$Name <- NULL
test$SibSp <- NULL
test$Parch <- NULL
test$Ticket <- NULL
test$Cabin <- NULL
test$FamilySize <- NULL


############################################################################################################
#Creating training Sets
X_train <- train_feature
X_train$Survived <- NULL
Y_train <- train_feature$Survived
X_test <- test
X_test$PassengerId <- NULL

str(X_train)
str(Y_train)
str(X_test)


#Logistic regression
fit <- glm(Y_train ~.,data=X_train,family = binomial(link='logit'))
summary(fit)
confint(fit)
exp(coef(fit))
exp(confint(fit))

predictTrain <- predict(fit, type="response")

baseAcur = as.numeric(length(which(Y_train==0))/length(Y_train))
baseAcur

table(Y_train,predictTrain >= 0.5)

accuracy = (466+241)/nrow(X_train)
sensitivity = 241/(241+101)
specificity = 466/(466+83)


cat("accuracy: ", accuracy, " > ", "baseline: ", baseAcur)

predictTest <- predict(fit,type="response",newdata=test)
test$Survived <- as.numeric(predictTest >= 0.5)
Predictions <- data.frame(test[c("PassengerId","Survived")])
table(test$Survived)
write.csv(file = "TitanicPred", x = Predictions)

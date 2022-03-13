rm(list=ls())
install.packages("ISLR2")   ### Installing ISLR 2 package to retrieve data for Q1 and Q3
install.packages("GGally")   ### Pair Plot usage 
install.packages("MASS")
install.packages("arules")
install.packages("rpart")
install.packages("rattle")
install.packages('caret')

setwd("C:/Users/Shan/Desktop/Spring 22/Data Mining 2/Homework 1/Homework1") ### Setting directory for the program
getwd()

library(ISLR2)
data(College)
head(College)
df1= College[2:ncol(College)]  ### Copying only the numeric inputs to df1 dataset 


#******************* Pair plot and Histogram *********#
library(GGally)
GGally::ggpairs(df1)   ## Pair plot or dependency plot of each variable 
str(df1)
par(mfrow= c(4,4))
for(i in 1:ncol(df1)){
  hist(df1[,i],xlab=colnames(df1[i]),main= " ")  # Frequency distribution 
                                                #each feature vectors. 
}
#********************* Data transformation ******#
df2= log(df1)  ### Performing Log transform of the data
par(mfrow= c(4,4))
for(i in 1:ncol(df2)){  ### Visualizing the transformed data using Histogram 
  hist(df2[,i],xlab=colnames(df2[i]),main= " ")}

############ Sub-setting data frame   ##########
private= subset(College, College$Private =='Yes')
public=  subset(College, College$Private !='Yes')

############ Private and Public sorted data set ############

public= public[order(-public$Apps),]
private= private[order(-private$Apps),]

######### Data filtering ########
public= subset(public, public$Top25perc>median(public$Top25perc,na.rm = FALSE)) ### top 25% above median Public 
private= subset(private, private$Top25perc>median(private$Top25perc,na.rm = FALSE)) ### top 25% above median private 
#########################leveling of data ##############

##### Statistical analysis for data leveling #######
par(mfrow=c(1,1))
hist(public$Grad.Rate)
max(public$Grad.Rate)
mean(public$Grad.Rate,na.rm= FALSE)
boxplot(public$Grad.Rate,notch = TRUE ) 
median(public$Grad.Rate, na.ram= TRUE)
## Categorize graduation rate #######
public$category= cut(public$Grad.Rate, c(0,60,80,100), labels = c("Low","Medium","High"))
private$category= cut(private$Grad.Rate, c(0,60,80,100), labels = c("Low","Medium","High"))
########## Merging two data set in a list #######
Final_list= list(public, private) ### Generating List structure 
#########################################
#########################################

###########Question 3#####################
library(ISLR2)
library(MASS)
library(arules)
data(Boston)
head(Boston)
str(Boston)            
summary(Boston)
table(is.na(Boston))
train_data= Boston
### Removing ambiguous Parameters from the data set #######
Boston[['zn']] = NULL
Boston[['chas']] =NULL
Boston[['indus']] = NULL
Boston[['lstat']] = NULL
Boston[['rad']] = NULL




### Column split based on levels######
## All the below considerations are derived form graphical analysis 
hist(Boston[['black']])
max(Boston[['black']])
table(Boston[['black']]<356)

Boston[['crim']] <- ordered(cut(Boston[['crim']], c(0,0.06,100)),labels = c("Lowcrime", "Highcrime") )
Boston[['nox']] <- ordered(cut(Boston[['nox']], c(0,0.5,0.7,1)), labels = c(" safe ", "high", "Toxic"))
Boston[['rm']] <- ordered(cut(Boston[['rm']], c(0,7,10)), labels = c("Small house","Large house"))
Boston[['age']]  <- ordered(cut(Boston[['age']], c(1,19,45,55,100)), labels = c("Teenager", "Adult","Middle Age","Elderly"))
Boston[['dis']] <- ordered(cut(Boston[['dis']], c(0,4,10)), labels = c("Neartoworkplace", "farfromworkplace"))
Boston[['tax']] <- ordered(cut(Boston[['tax']], c(0,350,800)), labels = c("Low tax","High tax"))
Boston[['ptratio']] <- ordered(cut(Boston[['ptratio']], c(0,15,30)), labels = c("Lowp/tratio","Highp/tratio"))
Boston[['medv']] <- ordered(cut(Boston[['medv']], c(0,20,60)), labels = c("Moderate class","High class"))
Boston[['black']] <- ordered(cut(Boston[['black']], c(0,356,397)), labels = c("Less black","More black"))

### Binary Incidence matrix transformation #####
Boston_Inci <- as(Boston,"transactions")
#### Displaying the item frequency plot ######
itemFrequencyPlot(Boston_Inci, support = 0.08,  cex.names = 0.8)
##### Implementing apriori algorithm ########
rules <- apriori(Boston_Inci, parameter = list(support = 0.005, confidence = 0.4))
summary(rules)

#### Solution Part c####

student_pref= subset(rules,(rhs %in% c("crim=Lowcrime","dis=Neartoworkplace")))
inspect(head(sort(student_pref, by = "confidence"), n=5))

#### Solution Part d####

family_pref <- subset(rules, (rhs %in% "ptratio=Lowp/tratio"  & lift > 1))
inspect(head(sort(family_pref, by = "confidence"), n=3)) 

##########regression Model######
library(caret)
train_data[['ptratio']] <- ordered(cut(train_data[['ptratio']], c(0,15,30)), labels = c(0,1))
Model_test<- glm(ptratio~.,data = train_data, family ="binomial")       # Training the linear model
summary(Model_test)
lm.pred <- predict(Model_test, newdata = train_data, type = 'response')            # checking prediction on the train data 
train_data$predict=ifelse(lm.pred>0.5,1,0)
#confusionMatrix(train_data$predict,train_data$ptratio)


########### Question 2##########
str(marketing)
Income <- as.integer(runif(8993, min = min(marketing$Income), max = max(marketing$Income)))
Sex <- as.integer(runif(8993, min = min(marketing$Sex), max = max(marketing$Sex)))
Marital <- as.integer(runif(8993, min = min(marketing$Marital, na.rm = TRUE), max = max(marketing$Marital, na.rm = TRUE)))
Age <- as.integer(runif(8993, min = min(marketing$Age),max = max(marketing$Age)))
Edu <- as.integer(runif(8993, min = min(marketing$Edu, na.rm = TRUE), max = max(marketing$Edu, na.rm = TRUE)))
Occupation <- as.integer(runif(8993,min = min(marketing$Occupation,na.rm = TRUE), max = max(marketing$Occupation,na.rm = TRUE)))
Lived <- as.integer(runif(8993, min = min(marketing$Lived, na.rm = TRUE), max = max(marketing$Lived, na.rm = TRUE)))
Dual_Income <- as.integer(runif(8993, min = min(marketing$Dual_Income),max = max(marketing$Dual_Income) ))
Household <-as.integer(runif(8993, min = min(marketing$Household, na.rm = TRUE), max = max(marketing$Household, na.rm = TRUE) ))
Householdu18 <-as.integer(runif(8993, min = min(marketing$Householdu18, na.rm = TRUE), max = max(marketing$Householdu18, na.rm = TRUE) ))
Status <-as.integer(runif(8993, min = min(marketing$Status, na.rm = TRUE), max = max(marketing$Status, na.rm = TRUE)))
Home_Type <-as.integer( runif(8993, min = min(marketing$Home_Type, na.rm = TRUE), max = max(marketing$Home_Type, na.rm = TRUE)))
Ethnic <- as.integer(runif(8993, min = min(marketing$Ethnic, na.rm = TRUE), max = max(marketing$Ethnic, na.rm = TRUE)))
Language <- as.integer(runif(8993, min = min(marketing$Language, na.rm = TRUE), max = max(marketing$Language, na.rm = TRUE) ))

plot(Income)### checking individual plot 
### creating merged matrix #####
ref_set <- cbind(Income,Sex,Marital,Age,Edu,Occupation,Lived,Dual_Income,Household,Householdu18,Status,Home_Type,Ethnic,Language)
ref_set <- as.data.frame(ref_set)
marketing$Response <- 1
ref_set$Response <- 0

### stacking the data frames original and reference

ref_full = rbind(marketing,ref_set)
table(is.na(ref_full))
ref_full = na.omit(ref_full)
library(rpart)
control=rpart.control(minsplit=10, minbucket = 4,xval = 10)
fit <- rpart(Response~.,method="class", data=ref_full, control = control)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#### plotting the decision tree model ######
fancyRpartPlot(fit, caption = NULL)






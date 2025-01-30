
###########################################################################################################

#Lets open the libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(utils) 
library(sqldf) #for SQL
library(lubridate) #for age and time 
library(formattable) #for fancy tables
library(readxl) #for reading Excel tables 
#install.packages("patchwork")
library(patchwork) # To display 2 charts together
#install.packages("shiny")
library(shiny) #for the multi level table 
library(reshape2) #need this too to make the multi level table 
library(eeptools) #calcuate age
# For Logistic regression
#install.packages("caTools") 
# For ROC curve to evaluate model
#install.packages("ROCR")     
# Loading package
library(caTools)
library(ROCR)
#install.packages("splitstackshape")
library(splitstackshape)
#install.packages("sjPlot")
library(sjPlot)
#install.package("performance")
library(performance)
#install.packages("emmeans")
library(emmeans)
#install.packages("neuralnet")
library(neuralnet)

###########################################################################################################

#Read in the data

all201940 <- read_csv('H:/Tickets/W2407-0908/data/raw/all201940.csv')
all202010 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202010.csv')
all202040 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202040.csv')
all202110 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202110.csv')
all202140 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202140.csv')
all202210 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202210.csv')
all202240 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202240.csv')
all202310 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202310.csv')
all202340 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202340.csv')
all202410 <- read_csv('H:/Tickets/W2407-0908/data/raw/all202410.csv')

dfall<- rbind(all201940,
              all202010,
              all202040,
              all202110,
              all202140,
              all202210,
              all202240,
              all202310,
              all202340,
              all202410)
summary(dfall)

Spring<- rbind(all202010,
               all202110,
               all202210,
               all202310,
               all202410)

#We only need unique list of Spring students by term
Spring <- Spring %>% 
  group_by(Student_ID) %>%
  arrange(Reg_Term_Code)%>%
  slice(1)%>%
  ungroup()

Fall<- rbind(all201940,
             all202040,
             all202140,
             all202240,
             all202340)




#Only New fall students
#List of all new Fall students
NewFallStudents <- Fall %>%
  filter(Student_Type_Code=="N")

#I have a list of FYE classes
FYE_courses <- c(
  "BUS 109",
  "CIS 119",
  "EDU 114",
  "EGR 102",
  "PSY 105",
  "RSS 102", 
  "SDS 102", 
  "SDS 104",
  "SDS 105", 
  "SDS 106",
  "SDS 109",
  "SDS 110",
  "SDS 111", 
  "SDS 112")



#First I have  to combine subject and course_number
NewFallStudents$Course <- paste(NewFallStudents$Subject, NewFallStudents$Course_Number)
dfall$Course <- paste(dfall$Subject, dfall$Course_Number)
Fall$Course <- paste(Fall$Subject, Fall$Course_Number)
Spring$Course <- paste(Spring$Subject, Spring$Course_Number)

#Goal is to get a list of all new fall students and new column if they took an FYE class 
#and new column if they registered for spring

answer <- NewFallStudents %>%
  group_by(Student_ID) %>%
  reframe(
    Birth_Date,
    Term_Start_Date,
    Gender,
    FT_Fall = if_else( Registered_Credits >= 12, "1", "0"), 
    FYE_taken = any(Course %in% FYE_courses),
    FYE_Grade = if_else(FYE_taken, Grade_Code[Course %in% FYE_courses][1], NA),
    Only_Online = all(Course_Campus_Code == "D")
  )



#Great, now we only want distinct student id
answer <- answer %>% 
  distinct(Student_ID, .keep_all = TRUE)


#Change TRUE/FALSE to 1/0
#answer$Only_Online <- as.integer(as.logical(answer$Only_Online))
#answer$FYE_taken <- as.integer(as.logical(answer$FYE_taken))


#Next we see who enrolled in spring 
answer1 <- as.data.frame(
  sqldf( "
  SELECT 
  f.*,
  s.Reg_Term_Code as SpringTerm
  FROM answer as f
  Left JOIN Spring as s
  ON f.Student_ID = s.Student_ID 
  "))


#Clean it up a bit
#change the format to date 
head(answer1$Birth_Date)
answer1$Birth_Date <- as.Date(answer1$Birth_Date, "%m/%d/%Y")

head(answer1$Term_Start_Date)
answer1$Term_Start_Date <- as.Date(answer1$Term_Start_Date, "%m/%d/%Y")


answer1 <- answer1 %>% 
  mutate(
    Age = time_length(difftime(Term_Start_Date, Birth_Date, unit="weeks"), "years"),
    Gender_Male = if_else(Gender == "M", "1",  "0"),
    Enrolled_Spring = if_else(is.na(SpringTerm), 0, 1),
    FYE_Pass = if_else(
      is.na(FYE_Grade), "3",
      if_else(FYE_Grade == "D" | 
                FYE_Grade == "F" | 
                FYE_Grade == "W" | 
                FYE_Grade == "NA"| 
                FYE_Grade == "DH"| 
                FYE_Grade == "I"| 
                FYE_Grade == "IH", "0", "1")))
str(answer1)
summary(answer1)
#There is 1 na in gender, changed to 0 aka female
answer1$Gender_Male[is.na(answer1$Gender_Male)] <- 0 



#Simplify the list of variables
head(answer1) 
answer1 <- answer1%>% 
  select( FT_Fall, FYE_taken, Age, Enrolled_Spring, FYE_Pass, Gender_Male)

############################
#First question, is there a difference (in spring attendance) between FYE students and all other new students 
#Second question, is there a difference (in spring attendance)  between FYE students that passed and all other new students
#Third question, is there a difference (in spring attendance) between FYE students that failed and all other new students
answer1$FT_Fall <- as.numeric(answer1$FT_Fall)
answer1$FYE_taken       <- as.numeric(answer1$FYE_taken      )
answer1$Enrolled_Spring       <- as.numeric(answer1$Enrolled_Spring      )
answer1$FYE_Pass              <- as.numeric(answer1$FYE_Pass             )
answer1$Gender_Male           <- as.numeric(answer1$Gender_Male          )

#https://www.geeksforgeeks.org/multi-layer-perceptrons-mlp-in-r/

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
index <- sample(1:nrow(answer1), 0.8 * nrow(answer1))
train_data <- answer1[index, ]
test_data <- answer1[-index, ]





# Train the MLP model
model <- neuralnet(Enrolled_Spring~., data = train_data, 
                   hidden = 10, linear.output = FALSE, stepmax=1e7 )

plot(model)
# Make predictions on the test set
predictions <- predict(model, test_data, type = "response")

# Convert predictions to class labels
predicted_classes <- ifelse(predictions[,1] > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(predicted_classes == test_data$Enrolled_Spring)
print(paste("Accuracy: ", accuracy))



# Evaluating model accuracy
# using confusion matrix

library(caret)
predicted_classes <- as.data.frame(predicted_classes)
predicted_classes$predicted_classes<- as.factor(predicted_classes$predicted_classes)
test_data$Enrolled_Spring <- as.factor(test_data$Enrolled_Spring)

cm <- caret::confusionMatrix(data=predicted_classes$predicted_classes, reference=test_data$Enrolled_Spring )












is.atomic(predicted_classes )
predicted_classes <- as.data.frame(predicted_classes)

table(test_data$Enrolled_Spring, predictions)

# ROC-AUC Curve
ROCPred <- prediction(predicted_classes , test_data$Enrolled_Spring)
ROCPer <- performance(ROCPred, measure = "tpr", x.measure = "fpr") #fpr: false positive rate, tpr: true positive rate
plot(ROCPer, main = "ROC curve", colorize = T)
abline(a=0, b=1)

auc <- performance(ROCPred, measure = "auc") #auc: area under the ROC curve
auc <- auc@y.values[[1]]
auc

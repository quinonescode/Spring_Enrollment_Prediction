
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
install.packages("psych")
library(psych)


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
    FT_Fall = if_else( Registered_Credits >= 12, "Full-Time", "Part-time"), 
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
    Gender_Simp = if_else(Gender == "M", "Male", "Female"),
    Enrolled_Spring = if_else(is.na(SpringTerm), 0, 1),
    FYE_Pass = if_else(
      is.na(FYE_Grade), "Not Taken",
      if_else(FYE_Grade == "D" | 
                FYE_Grade == "F" | 
                FYE_Grade == "W" | 
                FYE_Grade == "NA"| 
                FYE_Grade == "DH"| 
                FYE_Grade == "I"| 
                FYE_Grade == "IH", "Fail", "Pass")))
#Age bins 
#answer1 <- answer1 %>% 
#  mutate(
#    Age_bin = if_else(
#      Age <= 20, "Under 20", "21 and Over" ))



#Simplify the list of variables
head(answer1) 
answer1 <- answer1%>% 
  select( FT_Fall, FYE_taken, Age, Enrolled_Spring, FYE_Pass, Gender_Simp)

########################################
#change answer1 0,1 to no,yes

#answer1 <- answer1 %>% 
#  mutate(
#    Enrolled_Spring =ifelse(
#      Enrolled_Spring=="0", "No","Yes"
#    )
#  )


########################################
#Random sampling of a variable (enrolled) to match not enrolled 
#then confusion matrix

answer1$Enrolled_Spring <- as.factor(answer1$Enrolled_Spring)
summary(answer1$Enrolled_Spring)

#First subset enrolled from not enrolled 
enrolled <- answer1$Enrolled_Spring == "Yes"
enrolled

dfenrolled <- subset(answer1, enrolled == "TRUE")
dfnotenrolled <- subset(answer1, enrolled == "FALSE")

#then randomly select 1756 ( to match not enrolled) - without replacement
set.seed(123)
dfenrolled.wo <- dfenrolled[sample(nrow(dfenrolled), size=1756, replace = FALSE),]

#then add them together
answer2 <- rbind(dfenrolled.wo, dfnotenrolled)
1756*2



############################
#First question, is there a difference (in spring attendance) between FYE students and all other new students 
#Second question, is there a difference (in spring attendance)  between FYE students that passed and all other new students
#Third question, is there a difference (in spring attendance) between FYE students that failed and all other new students


#Visualize the data 
# Plot FYE students against the target variable (spring attendance)
ggplot(answer2, aes(FYE_taken, fill = Enrolled_Spring)) +
  geom_bar() +
  coord_flip()

# Plot students gender against the target variable (spring attendance)
ggplot(answer2, aes(Gender_Simp, fill = Enrolled_Spring)) +
  geom_bar() +
  coord_flip()

# Plot students FT or PT status against the target variable (spring attendance)
ggplot(answer2, aes(FT_Fall, fill = Enrolled_Spring)) +
  geom_bar() +
  coord_flip()




#https://www.geeksforgeeks.org/logistic-regression-in-r-programming/


# Splitting dataset
set.seed(123)
split <- sample.split(answer2, SplitRatio = 0.8)
split

train_reg <- subset(answer2, split == "TRUE")
test_reg <- subset(answer2, split == "FALSE")

# Fit the logistic regression model
model <- glm(Enrolled_Spring ~  Gender_Simp + FT_Fall + FYE_Pass,  data = train_reg, family = "binomial")

#mysteps <- step(model,Enrolled_Spring ~  Gender_Simp + Age_bin + FT_Fall + FYE_Pass,  data = train_reg, family = "binomial" )

# View the summary of the model
summary(model)



predict_reg <- predict(model, test_reg, type = "response")




# Changing probabilities
predict_reg <- ifelse(predict_reg >0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table(test_reg$Enrolled_Spring, predict_reg)

missing_classerr <- mean(predict_reg != test_reg$Enrolled_Spring)
print(paste('Accuracy =', 1 - missing_classerr))

# ROC-AUC Curve
ROCPred <- prediction(predict_reg, test_reg$Enrolled_Spring)
ROCPer <- performance(ROCPred, measure = "tpr", x.measure = "fpr") #fpr: false positive rate, tpr: true positive rate
plot(ROCPer, main = "ROC curve", colorize = T)
abline(a=0, b=1)

auc <- performance(ROCPred, measure = "auc") #auc: area under the ROC curve
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1),
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)


#How many students actually when on to spring
answer1$Enrolled_Spring <- as.factor(answer1$Enrolled_Spring)
summary(answer1$Enrolled_Spring)

test_reg$Enrolled_Spring <- as.factor(test_reg$Enrolled_Spring)
summary(test_reg$Enrolled_Spring)


#Random sampling of a variable (enrolled) to match not enrolled 
#then confusion matrix

#First subset enrolled from not enrolled 


#then randomly select 1756 ( to match not enrolled)


#then add them together


#then glm 


#then confusion matrix 


#Enrolled_Spring change back to 0, 1 for confusion matrix
answer3 <- answer2 %>% 
  mutate(
    Gender_Simp = if_else(Gender_Simp == "Male", 0, 1), 
    Enrolled_Spring = if_else(Enrolled_Spring=="No", 0, 1),
    FT_Fall= if_else(FT_Fall=="Part-time", 0, 1),
    FYE_taken = if_else(FYE_taken=="FALSE", 0,1),
    FYE_Pass = if_else( FYE_Pass=="Not Taken", 0, 1)) 

#Correlation between student return in spring
answer3 <-drop_na(answer3)
M = cor(answer3)

corrplot(M, method = 'number')

mycor = cor(answer3) #create square correlation matrix
det(mycor) #Determinant of correlation matrix
mycov= cov(answer3) #create a square variance-covariance matrix
det(mycov) #determinant of variance-covariance matrix
options(scipen=999)
mymatrix = cov2cor(mycov) #Convert covariance matrix to correlation matrix
mymatrix #Print out correlation matrix
mycor #Another way to print out the correlation matrix from answer2 data set

describeBy(answer3, group = answer3$Enrolled_Spring)


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

############################
#First question, is there a difference (in spring attendance) between FYE students and all other new students 
#Second question, is there a difference (in spring attendance)  between FYE students that passed and all other new students
#Third question, is there a difference (in spring attendance) between FYE students that failed and all other new students


#Visualize the data 
# Plot FYE students against the target variable (spring attendance)
ggplot(answer1, aes(FYE_taken, fill = Enrolled_Spring)) +
  geom_bar() +
  coord_flip()

# Plot students gender against the target variable (spring attendance)
ggplot(answer1, aes(Gender_Simp, fill = Enrolled_Spring)) +
  geom_bar() +
  coord_flip()

# Plot students FT or PT status against the target variable (spring attendance)
ggplot(answer1, aes(FT_Fall, fill = Enrolled_Spring)) +
  geom_bar() +
  coord_flip()




#https://www.geeksforgeeks.org/logistic-regression-in-r-programming/


# Splitting dataset
set.seed(123)
split <- sample.split(answer1, SplitRatio = 0.8)
split

train_reg <- subset(answer1, split == "TRUE")
test_reg <- subset(answer1, split == "FALSE")

#######################################################################################
#https://www.youtube.com/watch?v=EIR9zN0tDPw

# Fit the logistic regression model
model <- glm(Enrolled_Spring ~  Gender_Simp + Age + FT_Fall + FYE_Pass,  data = train_reg, family = "binomial")

######check non-linearity (generalized additive model)
m1 <- mgcv::gam (Enrolled_Spring ~Gender_Simp +s(Age) + FT_Fall + FYE_Pass, answer1, family = "binomial" )

plot(m1)



#######################
#Conduct logistic regression with numeric predict (Age) 
m <- glm(Enrolled_Spring ~Age, answer1, family = binomial)

#plot predictions 
summary(m)
plot_model(m, type = "pred", terms = "Age")


#We see a very small but significant decline in enrollment with increased age
#Age         -0.023649   0.003326  -7.111 1.15e-12 ***p=0.001



#Check for non-linearity using polynomial degrees 
m2 = glm(Enrolled_Spring ~ poly(Age, 2), data=answer1, family =binomial)
m3 = glm(Enrolled_Spring ~ poly(Age, 3), data=answer1, family = binomial)
m4 = glm(Enrolled_Spring ~ poly (Age, 4), data=answer1, family= binomial)

#Compare all the models 
AIC(m2, m3, m4)
#the one with the lowest AIC is m4 
#display degrees
tab_model(m4)
#Interestingly all the polynomial degrees are significant

#Check model assumptions


check_model(m4)
#posterior predictive Check: Good. compares the predicted intervals (blue) with the observed values (green)
#Binned Residuals: We have a bunch of outliers (red)
#Influential Observations: the outliers are not influential (outside the contour lines)
#Uniformity of Residuals: Good, very uniform 
#Model seems to be ok

#Plot prediction 
plot_model(m3, type = "eff", terms = "Age [all]")
#the plot shows that students younger than 20 have the highest chance of enrollment
#decreases until age 30  and spikes at 50, then drops again


#To better visualize lets look at the specific ages (18, 30, 52)
b <- emmip(m3, ~Age, CIs = T, type = "response",
           at=list(Age= c(18, 30,52)))+
  scale_y_continuous(labels=scales::percent)
b

#how many 30 year old student do we have? #how many 52 year old students do we have? 
class(answer1$Age) #numeric
answer1$Age <- floor(answer1$Age)
answer1$Age <- as.factor(answer1$Age)
summary(answer1$Age)

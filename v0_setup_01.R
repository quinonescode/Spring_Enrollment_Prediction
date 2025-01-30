

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


dfall$Subject <- as.factor(dfall$Subject)
summary(dfall$Subject)

Spring<- rbind(all202010,
              all202110,
              all202210,
              all202310,
              all202410)

Fall<- rbind(all201940,
              all202040,
              all202140,
              all202240,
              all202340)

#List of all new Fall students
NewFallStudents <- Fall %>%
  filter(Student_Type_Code=="N")


#We only need unique list of Spring students by term
Spring <- Spring %>% 
  group_by(Student_ID) %>%
  arrange(Reg_Term_Code)%>%
  slice(1)%>%
  ungroup()


#I have a list of FYE classes
#First I have  to combine subject and course_number
NewFallStudents$Course <- paste(NewFallStudents$Subject, NewFallStudents$Course_Number)
dfall$Course <- paste(dfall$Subject, dfall$Course_Number)
Fall$Course <- paste(Fall$Subject, Fall$Course_Number)
Spring$Course <- paste(Spring$Subject, Spring$Course_Number)





#Only the students who attended the FYE classes 
FYE <- dfall %>%
  filter(Course == "BUS 109"|
           Course == "CIS 119"|
           Course == "EDU 114"|
           Course == "EGR 102"|
           Course == "PSY 105"|
           Course == "RSS 102"|
           Course == "SDS 102"|
           Course == "SDS 104"|
           Course == "SDS 105"|
           Course == "SDS 106"|
           Course == "SDS 109"|
           Course == "SDS 110"|
           Course == "SDS 111"|
           Course == "SDS 112" )

FYE$Course_Title <- as.factor(FYE$Course_Title )
summary(FYE$Course_Title)

#change the format to date 
head(FYE$Birth_Date)
FYE$Birth_Date <- as.Date(FYE$Birth_Date, "%m/%d/%Y")

head(FYE$Term_Start_Date)
FYE$Term_Start_Date <- as.Date(FYE$Term_Start_Date, "%m/%d/%Y")


#Ok, now just all new students 
FYE$Student_Type_Desc <- as.factor(FYE$Student_Type_Desc )
FYE <- FYE %>% 
  filter(Student_Type_Code == "N")

summary(FYE$Student_Type_Desc)

#change the format to date 
head(FYE$Birth_Date)
FYE$Birth_Date <- as.Date(FYE$Birth_Date, "%m/%d/%Y")

head(FYE$Term_Start_Date)
FYE$Term_Start_Date <- as.Date(FYE$Term_Start_Date, "%m/%d/%Y")


#Great,  age, race, gender, PT or FT course load, online-only Y/N, and successful completion of FYE course 
FYEdf <- FYE %>% 
  select(Student_ID, Reg_Term_Code, Gender, Birth_Date, Term_Start_Date, Registered_Credits, Course_Building, Grade_Code, Course, Course_Title ) %>%
  mutate(
    Age = time_length(difftime(Term_Start_Date, Birth_Date, unit="weeks"), "years"),
    FallStatus = if_else( Registered_Credits >= 12, "Full-Time", "Part-Time"),
    Online = if_else(Course_Building == "Online", "Yes", "No"),
    Pass_FYE = if_else(Grade_Code == "D" | 
                         Grade_Code == "F" | 
                         Grade_Code == "W" | 
                         Grade_Code == "NA"| 
                         Grade_Code == "DH"| 
                         Grade_Code == "I"| 
                         Grade_Code == "IH", "No", "Yes"))

FYE$Grade_Code <- as.factor( FYE$Grade_Code)

summary(FYE$Grade_Code)





#and response variable of enrollment the following spring.
answer <- as.data.frame(
sqldf( "
  SELECT 
  f.Student_ID,
  f.age,
  f.Gender,
  f.FallStatus,
  f.Online,  
  f.Course, 
  f.Course_Title,
  f.Reg_Term_Code as FallTerm, 
  f.Pass_FYE,
  s.Reg_Term_Code as SpringTerm
  FROM FYEdf as f
  left JOIN Spring as s
  ON f.Student_ID = s.Student_ID 
  "))


############################
#First question, is there a difference (in spring attendance) between FYE students and all other new students 
#Second question, is there a difference (in spring attendance)  between FYE students that passed and all other new students
#Third question, is there a difference (in spring attendance) between FYE students that failed and all other new students
#
#
#create a new dataframe with all the fall students, 
#a column to indicate if the student took an FYE class
#A column in they passed, 
#A column if they continued to spring

#ok, because we already have the list of FYE students, we should just add in the other new fall students 

answer <- as.data.frame(
  sqldf( "
  SELECT 
  f.Student_ID,
  f.age,
  f.Gender,
  f.FallStatus,
  f.Online,  
  f.Course, 
  f.Course_Title,
  f.Reg_Term_Code as FallTerm, 
  f.Pass_FYE,
  s.Reg_Term_Code as SpringTerm
  FROM FYEdf as f
  left JOIN Spring as s
  ON f.Student_ID = s.Student_ID 
  "))

#################################################
#####POST CHOICE EXPERIMENT ANALYSIS#############
#################################################

###LIBRARY SET UP####
library(dplyr)
library(ggplot2)
library(naniar)
library(tidyverse)
library(janitor)
library(skimr)
library(psych)
library(ggdist)
library(car)
library(correlation)
library(scales)
library(cluster)
library(factoextra)
library(GGally)
library(NbClust)
library(hopkins)
library(gt)
library(gtsummary)
library(RColorBrewer)

###LOAD DATA###
load("database_PT_final.Rda")
#NB - the data is called "database"
database_choices=read.csv("finaldata.csv",header=TRUE)
database<-inner_join(database_choices, database, by ='RID')

#Filter by rural areas#
database<-filter(database,p_place_inhab<3)


###ADJUSTING VARIABLE TYPES###
#This code is for changing the variables to the correct data type.
database <-
  database %>%
  mutate(sex_front = as.factor(sex_front),
         p_bundesland_front = as.factor(p_bundesland_front),
         p_educ_front = as.factor(p_educ_front),
         age_front = as.integer(age_front),
         m_known = as.factor(m_known),
         DRT_knowledge = as.factor(DRT_knowledge),
         p_place_inhab = as.factor(p_place_inhab),
         p_mtools_1 = as.factor(p_mtools_1),
         p_central_wien = as.factor(p_central_wien),
         p_central_nonwien = as.factor(p_central_nonwien),
         kilometrage_per_car_1 = as.factor(kilometrage_per_car_1),
         kilometrage_per_car_2 = as.factor(kilometrage_per_car_2),
         kilometrage_per_car_3 = as.factor(kilometrage_per_car_3),
         kilometrage_per_car_4 = as.factor(kilometrage_per_car_4),
         n_o_v_hh = as.factor(n_o_v_hh),
         income = as.factor(income),
         children_1 = as.factor(children_1),
         children_2 = as.factor(children_2),
         children_3 = as.factor(children_3),
         status = as.factor(status),
         n_o_hhm = as.factor(n_o_hhm),
         p_season_1 = as.factor(p_season_1),
         p_season_2 = as.factor(p_season_2),
         p_season_3 = as.factor(p_season_3),
         p_season_4 = as.factor(p_season_4)
  )

glimpse(database)

###########################################
###POST CHOICE EXPERIMENT ANALYSIS###
###########################################


##CREATE DUMMY VARIABLE FOR BUY MOBILITY PACKAGE YES/NO##
library(dplyr)
database <- database %>%
  mutate(buy_dummy = ifelse(buy_actually_e1 %in% c("1", "2", "3", "4", "5") | 
                                  buy_actually_e2 %in% c("1", "2", "3", "4", "5") |
                                  buy_actually_e3 %in% c("1", "2", "3", "4", "5") | 
                                  buy_actually_e4 %in% c("1", "2", "3", "4", "5") | 
                                  buy_actually_e5 %in% c("1", "2", "3", "4", "5"), "0", "1"))

database <-
  database %>%
  mutate(buy_dummy = as.factor(buy_dummy))


#######################
###PERSONA FILTERS###
######################

##THE PERIPHERAL RESIDENT##
#database<-filter(database,p_central_nonwien==4)

##THE ELDERLY##
#database<-filter(database,age_front>=60)

##THE PUPIL##
#database<-filter(database,status==3)

##THE STAY-AT-HOME PARENT##
#database<-filter(database,status==5)

##THE FULL-TIME WORKER##
#database<-filter(database,status==1)

##THE PARENT##
#nb - these are parents whose kids are still in the household#
#database<-filter(database,children_1==2 | children_2==2 | children_3==2)

##THE WORKING COMMUTER##
#database<-filter(database,status==1 | status==2)
#database<-filter(database,kilometrage_per_car_1==5 | kilometrage_per_car_2==5 | kilometrage_per_car_3==5 | kilometrage_per_car_4==5)
#database<-filter(database,kilometrage_per_car_1==6 | kilometrage_per_car_2==6 | kilometrage_per_car_3==6 | kilometrage_per_car_4==6)

##THE FREQUENT DRIVER##
#database<-filter(database,p_mob_rural==1)

##THE LATE NIGHT DRIVER##
#database<-filter(database,p_mob_time==1)

##THE LOW-INCOME RESIDENT##
#database<-filter(database,income==2 | income ==1)

##THE MIDDLE-INCOME RESIDENT##
#database<-filter(database,income==3 | income==4)

##THE HIGH-INCOME RESIDENT##
#database<-filter(database,income==5 | income==6)

##THE BLUE COLLAR WORKER##
#Assumption that former apprentices and technical students became blue-collar workers
#database<-filter(database,p_educ_front==2 | p_educ_front==3)

##THE ENVIRONMENTALIST##
#database<-filter(database,Statements_1==5)

##THE CLIMATE CHANGE DENIER##
#database<-filter(database,Statements_1==1)

##THE ENVIRONMENTALLY INDIFFERENT##
#database<-filter(database,Statements_1==3)

##THE INTREPID ENVIRONMENTALIST##
#database<-filter(database,Statements_1==4)

##THE SEASON TICKET HOLDER##
#database<-filter(database,p_season_1==2 | p_season_2==2 | p_season_3==2)

##THE MAN##
#database<-filter(database,sex_front==2)

##THE WOMAN##
#database<-filter(database,sex_front==1)


#0 = yes; 1 = no

#BUY MG?#
#table(database$buy_dummy)

#MG a good idea - considered buying?#
#table(database$YES_ask_support)

#MG a good idea - didn't consider buying?#
#table(database$NO_ask_support)

#################################
###########WHY AGAINST###########
#################################

#Not feasible anyway#
#table(database$No_Support_MG_1)

#Existing public transport is sufficient#
#table(database$No_Support_MG_2)

#MG system too complicated#
#table(database$No_Support_MG_3)

#Too expensive for taxpayer#
#table(database$No_Support_MG_4)


################################
############WHY FOR#############
################################

#State complementary benefits (parking, safe roads, etc)
#table(database$Yes_Support_MG_1)

#Reduce driving
#table(database$Yes_Support_MG_2)

#Back-up option for drivers
#table(database$Yes_Support_MG_3)

#Right to have guaranteed mobility
#table(database$Yes_Support_MG_4)


##################################
############EXTRAS################
##################################

#Higher discount for cab services
#table(database$Extra_1)

#Higher discount for car-sharing membership
#table(database$Extra_2)

#Discounts abroad
#table(database$Extra_3)

#Compensation for time lost due to cancellations or missed connections
#table(database$Extra_4)

#Discounts on online delivery services
#table(database$Extra_5)

##########################################
############CLIMATE ATTITUDES##############
############################################

#Concerned about climate change
table(database$Statements_1)

#Should use climate-friendly travel
table(database$Statements_2)

#Austrian government should do more for climate-friendly travel
table(database$Statements_3)

#MG journeys should be cheaper than personal car
table(database$Statements_4)

#MG journeys should be faster than personal car
table(database$Statements_5)

#####################################
###########VISUALISATION#############
####################################

#######MG GOOD IDEA - DID BUY#########
# Creating a data frame
data_buy_goodidea <- data.frame(Yes = 185, No = 39)

# Displaying the data frame
print(data_buy_goodidea)

# Making a pie chart
pie(c(data_buy_goodidea$Yes, data_buy_goodidea$No), labels = c(paste("Yes: ", data_buy_goodidea$Yes), paste("No: ", data_buy_goodidea$No)), main = "Bought MG: Is a MG a good idea?", col = c("blue", "red"))

#######MG GOOD IDEA - DIDN'T BUY#########
# Creating a data frame
data_nobuy_goodidea <- data.frame(Yes = 139, No = 89)

# Making a pie chart
pie(c(data_nobuy_goodidea$Yes, data_nobuy_goodidea$No), labels = c(paste("Yes: ", data_nobuy_goodidea$Yes), paste("No: ", data_nobuy_goodidea$No)), main = "Didn't Buy MG: Is a MG a good idea?", col = c("blue", "red"))



######LIKERT ATTEMPT#######

database_og = load("database_PT_final.Rda")
likert_database <- database[, c("RID", "Statements_3")]
# Remove rows with missing values in any of the selected columns
likert_database_1 <- likert_database[complete.cases(likert_database), ]

#Fake data set
RID <- c("99991", "99992", "99993", "99994", "99995")
Statements_3 <- c(1, 2, 3, 4, 5)
fake <- data.frame(RID, Statements_3)


#Some weird shit idk
likert_database_2 <- rbind(likert_database_1, fake)
likert_database_2$Statements_3_f <- as.factor(likert_database_2$Statements_3)

#Attach factor levels
factor_levels <- c("Strongly Disagree","Disagree","Neutral",
                   "Agree","Strongly Agree")

levels(likert_database_2$Statements_3_f) <- factor_levels

#Remove fake data
nrow(likert_database_2)
likert_database_3 <- subset(likert_database_2, RID < 99991)
nrow(likert_database_3)


###Remove RID
likert_database_4 <- as.data.frame(likert_database_3[, c("Statements_3_f")])

###Include Likert Statement
VarHeadings <- c("Gov Expand CF Transport")

names(likert_database_4) <- VarHeadings
colnames(likert_database_4)


##CHATGPT HELP
# Convert the Likert column to a factor with specified levels
#likert_database_4$Yes_Support_MG_4_f <- factor(likert_database_4$Yes_Support_MG_4_f, levels = factor_levels)

# Remove rows with missing values in any of the selected columns
#likert_database_4 <- likert_database_4[complete.cases(likert_database_4), ]

# Check if the factor levels are consistent
#if (!all(levels(likert_database_4$Yes_Support_MG_4_f) %in% factor_levels)) {
 # warning("Not all factor levels are present in the data. Adjusting levels.")
 # likert_database_4$Yes_Support_MG_4_f <- factor(likert_database_4$Yes_Support_MG_4_f, levels = factor_levels)
#}

##PLOT###


library(likert)
p <- likert(likert_database_4)
a <- likert.bar.plot(p, legend.position = "right", text.size = 4, bar.height=0.6) +
  theme(text = element_text(size = rel(4)),axis.text.y = element_text(size = rel(2))) +
  theme_update(legend.text = element_text(size = rel(0.7))) +
  theme_classic()
plot(a)



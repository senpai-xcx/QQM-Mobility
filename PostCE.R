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
pie(c(data_buy_goodidea$Yes, data_buy_goodidea$No), labels = c(paste("Yes: ", data_buy_goodidea$Yes), paste("No: ", data_buy_goodidea$No)), main = "Did buy: Is a mobility guarantee a good idea?", col = c("#5ab4ac", "#d8b365"))

#######MG GOOD IDEA - DIDN'T BUY#########
# Creating a data frame
data_nobuy_goodidea <- data.frame(Yes = 139, No = 89)

# Making a pie chart
pie(c(data_nobuy_goodidea$Yes, data_nobuy_goodidea$No), labels = c(paste("Yes: ", data_nobuy_goodidea$Yes), paste("No: ", data_nobuy_goodidea$No)), main = "Did not buy: Is a mobility guarantee a good idea?", col = c("#5ab4ac", "#d8b365"))



######LIKERT ATTEMPT#######
##Attitudes to climate change and transport policy##
database_og = load("database_PT_final.Rda")
#Filter by rural areas#
database<-filter(database,p_place_inhab<3)
likert_database <- database[, c("RID", "Statements_4", "Statements_3", "Statements_2", "Statements_1")]
# Remove rows with missing values in any of the selected columns
likert_database_1 <- likert_database[complete.cases(likert_database), ]

#Fake data set
RID <- c("99991", "99992", "99993", "99994", "99995")
Statements_4 <- c(1, 2, 3, 4, 5)
Statements_3 <- c(1, 2, 3, 4, 5)
Statements_2 <- c(1, 2, 3, 4, 5)
Statements_1 <- c(1, 2, 3, 4, 5)
fake <- data.frame(RID, Statements_4, Statements_3, Statements_2, Statements_1)


#Some weird shit idk
likert_database_2 <- rbind(likert_database_1, fake)
likert_database_2$Statements_4_f <- as.factor(likert_database_2$Statements_4)
likert_database_2$Statements_3_f <- as.factor(likert_database_2$Statements_3)
likert_database_2$Statements_2_f <- as.factor(likert_database_2$Statements_2)
likert_database_2$Statements_1_f <- as.factor(likert_database_2$Statements_1)

#Attach factor levels
factor_levels <- c("Strongly Disagree","Disagree","Neutral",
                   "Agree","Strongly Agree")

levels(likert_database_2$Statements_4_f) <- factor_levels
levels(likert_database_2$Statements_3_f) <- factor_levels
levels(likert_database_2$Statements_2_f) <- factor_levels
levels(likert_database_2$Statements_1_f) <- factor_levels

#Remove fake data
nrow(likert_database_2)
likert_database_3 <- subset(likert_database_2, RID < 99991)
nrow(likert_database_3)


###Remove RID
likert_database_4 <- as.data.frame(likert_database_3[, c("Statements_4_f", "Statements_3_f", "Statements_2_f", "Statements_1_f")])

###Include Likert Statement
VarHeadings <- c("A mobility guarantee should be cheaper than owning a car", "Austrian government should expand climate-friendly transport", "We should use climate-friendly transport", "I am concerned about climate change")

names(likert_database_4) <- VarHeadings
colnames(likert_database_4)

#Behold the Likert graph!
library(likert)
library(ggplot2)
p <- likert(likert_database_4)
a <- likert.bar.plot(p, legend.position = "right", text.size = 4, bar.height=0.6) +
  theme(text = element_text(size = rel(4)),axis.text.y = element_text(size = rel(2))) +
  theme_update(legend.text = element_text(size = rel(0.7))) +
  theme_classic()
a <- a + ggtitle("Attitudes to climate change and transport policy (n=452)")+
  labs(caption = "Applies to all rural residents") +
  theme(plot.caption = element_text(face = "italic"))

plot(a)

#Check number of results - 
table(database$Statements_1)

######LIKERT ATTEMPT#######
##REASONS WHY AGAINST MG##
database_og = load("database_PT_final.Rda")
#Filter by rural areas#
database<-filter(database,p_place_inhab<3)
likert_database3 <- database[, c("RID", "No_Support_MG_1", "No_Support_MG_2", "No_Support_MG_3", "No_Support_MG_4")]
# Remove rows with missing values in any of the selected columns
likert_database3_1 <- likert_database3[complete.cases(likert_database3), ]

#Fake data set
RID <- c("99991", "99992", "99993", "99994", "99995")
No_Support_MG_1 <- c(1, 2, 3, 4, 5)
No_Support_MG_2 <- c(1, 2, 3, 4, 5)
No_Support_MG_3 <- c(1, 2, 3, 4, 5)
No_Support_MG_4 <- c(1, 2, 3, 4, 5)
fake <- data.frame(RID, No_Support_MG_1, No_Support_MG_2, No_Support_MG_3, No_Support_MG_4)


#Some weird shit idk
likert_database3_2 <- rbind(likert_database3_1, fake)
likert_database3_2$No_Support_MG_1_f <- as.factor(likert_database3_2$No_Support_MG_1)
likert_database3_2$No_Support_MG_2_f <- as.factor(likert_database3_2$No_Support_MG_2)
likert_database3_2$No_Support_MG_3_f <- as.factor(likert_database3_2$No_Support_MG_3)
likert_database3_2$No_Support_MG_4_f <- as.factor(likert_database3_2$No_Support_MG_4)

#Attach factor levels
factor_levels <- c("Strongly Disagree","Disagree","Neutral",
                   "Agree","Strongly Agree")

levels(likert_database3_2$No_Support_MG_1_f) <- factor_levels
levels(likert_database3_2$No_Support_MG_2_f) <- factor_levels
levels(likert_database3_2$No_Support_MG_3_f) <- factor_levels
levels(likert_database3_2$No_Support_MG_4_f) <- factor_levels

#Remove fake data
nrow(likert_database3_2)
likert_database3_3 <- subset(likert_database3_2, RID < 99991)
nrow(likert_database3_3)


###Remove RID
likert_database3_4 <- as.data.frame(likert_database3_3[, c("No_Support_MG_1_f", "No_Support_MG_2_f", "No_Support_MG_3_f", "No_Support_MG_4_f")])

###Include Likert Statement
VarHeadings <- c("A mobility guarantee is not feasible anyway", "The existing public transport network in Austria is sufficient", "A mobility guarantee system is too complicated", "A mobility guarantee is too expensive for the taxpayer")

names(likert_database3_4) <- VarHeadings
colnames(likert_database3_4)

#Behold the Likert graph!
library(likert)
library(ggplot2)
p <- likert(likert_database3_4)
a <- likert.bar.plot(p, legend.position = "right", text.size = 4, bar.height=0.6) +
  theme(text = element_text(size = rel(4)),axis.text.y = element_text(size = rel(2))) +
  theme_update(legend.text = element_text(size = rel(0.7))) +
  theme_classic()
a <- a + ggtitle("Reasons why against mobility guarantee (n=128)")+
  labs(caption = "Applies to respondents who did not support a mobility guarantee") +
  theme(plot.caption = element_text(face = "italic"))

plot(a)

#Check number of results - 
table(database$No_Support_MG_1)

######LIKERT ATTEMPT#######
##REASONS WHY MG SUPPORTED##
database_og = load("database_PT_final.Rda")
#Filter by rural areas#
database<-filter(database,p_place_inhab<3)
likert_database2 <- database[, c("RID", "Yes_Support_MG_1", "Yes_Support_MG_2", "Yes_Support_MG_3", "Yes_Support_MG_4")]
# Remove rows with missing values in any of the selected columns
likert_database2_1 <- likert_database2[complete.cases(likert_database2), ]

#Fake data set
RID <- c("99991", "99992", "99993", "99994", "99995")
Yes_Support_MG_1 <- c(1, 2, 3, 4, 5)
Yes_Support_MG_2 <- c(1, 2, 3, 4, 5)
Yes_Support_MG_3 <- c(1, 2, 3, 4, 5)
Yes_Support_MG_4 <- c(1, 2, 3, 4, 5)
fake <- data.frame(RID, Yes_Support_MG_1, Yes_Support_MG_2, Yes_Support_MG_3, Yes_Support_MG_4)


#Some weird shit idk
likert_database2_2 <- rbind(likert_database2_1, fake)
likert_database2_2$Yes_Support_MG_1_f <- as.factor(likert_database2_2$Yes_Support_MG_1)
likert_database2_2$Yes_Support_MG_2_f <- as.factor(likert_database2_2$Yes_Support_MG_2)
likert_database2_2$Yes_Support_MG_3_f <- as.factor(likert_database2_2$Yes_Support_MG_3)
likert_database2_2$Yes_Support_MG_4_f <- as.factor(likert_database2_2$Yes_Support_MG_4)

#Attach factor levels
factor_levels <- c("Strongly Disagree","Disagree","Neutral",
                   "Agree","Strongly Agree")

levels(likert_database2_2$Yes_Support_MG_1_f) <- factor_levels
levels(likert_database2_2$Yes_Support_MG_2_f) <- factor_levels
levels(likert_database2_2$Yes_Support_MG_3_f) <- factor_levels
levels(likert_database2_2$Yes_Support_MG_4_f) <- factor_levels

#Remove fake data
nrow(likert_database2_2)
likert_database2_3 <- subset(likert_database2_2, RID < 99991)
nrow(likert_database2_3)


###Remove RID
likert_database2_4 <- as.data.frame(likert_database2_3[, c("Yes_Support_MG_1_f", "Yes_Support_MG_2_f", "Yes_Support_MG_3_f", "Yes_Support_MG_4_f")])

###Include Likert Statement
VarHeadings <- c("The Austrian state facilitates transport by car", "People drive less when public transport options are good", "A mobility guarantee is a good back-up option to driving", "Everyone has a right to guaranteed mobility")

names(likert_database2_4) <- VarHeadings
colnames(likert_database2_4)

#Behold the Likert graph!
library(likert)
library(ggplot2)
p <- likert(likert_database2_4)
a <- likert.bar.plot(p, legend.position = "right", text.size = 4, bar.height=0.6) +
  theme(text = element_text(size = rel(4)),axis.text.y = element_text(size = rel(2))) +
  theme_update(legend.text = element_text(size = rel(0.7))) +
  theme_classic()
a <- a + ggtitle("Reasons why a mobility guarantee is supported (n=324)")+
  labs(caption = "Applies to respondents who supported a mobility guarantee") +
  theme(plot.caption = element_text(face = "italic"))

plot(a)

#Check number of results - 
table(database$Yes_Support_MG_1)


###########################
###########################
######LIKERT CAR ATTEMPTS#######
###########################
###########################
##REDUCED DRIVING TING##

database_og = load("database_PT_final.Rda")
#Filter by rural areas#
database<-filter(database,p_place_inhab<3)
likert_database_carusage <- database[, c("RID", "car_usage_down")]
# Remove rows with missing values in any of the selected columns
likert_database_carusage_1 <- likert_database_carusage[complete.cases(likert_database_carusage), ]

#Fake data set
RID <- c("99991", "99992", "99993", "99994", "99995")
car_usage_down <- c(1, 2, 3, 4, 5)
fake <- data.frame(RID, car_usage_down)


#Some weird shit idk
likert_database_carusage_2 <- rbind(likert_database_carusage_1, fake)
likert_database_carusage_2$car_usage_down_f <- as.factor(likert_database_carusage_2$car_usage_down)

#Attach factor levels
factor_levels <- c("No reduction (0%)","Low reduction (1-10%)","Moderate reduction (10-50%)",
                   "High reduction (51-80%)","High to complete reduction (81-100%)")

levels(likert_database_carusage_2$car_usage_down_f) <- factor_levels

#Remove fake data
nrow(likert_database_carusage_2)
likert_database_carusage_3 <- subset(likert_database_carusage_2, RID < 99991)
nrow(likert_database_carusage_3)


###Remove RID
likert_database_carusage_4 <- as.data.frame(likert_database_carusage_3[, c("car_usage_down_f")])

###Include Likert Statement
VarHeadings <- c("Owners of >1 vehicle: By how much would you reduce your driving?")

names(likert_database_carusage_4) <- VarHeadings
colnames(likert_database_carusage_4)

#Behold the Likert graph!
library(likert)
library(ggplot2)
p <- likert(likert_database_carusage_4)
a <- likert.bar.plot(p, legend.position = "right", text.size = 4, bar.height=0.6) +
  theme(text = element_text(size = rel(4)),axis.text.y = element_text(size = rel(2))) +
  theme_update(legend.text = element_text(size = rel(0.7))) +
  theme_classic()
a <- a + ggtitle("Potential reduction of car usage if mobility guarantee purchased (n=143)")+
  labs(caption = "Applies to rural drivers who considerd purchasing mobility guarantee") +
  theme(plot.caption = element_text(face = "italic"))

plot(a)

#Check number of results - 143
table(database$car_usage_down)

##REDUCED COST TING##

database_og = load("database_PT_final.Rda")
#Filter by rural areas#
database<-filter(database,p_place_inhab<3)
#Turn the 5s into 4s
database$costs_down <- ifelse(database$costs_down == 5, 4, database$costs_down)
likert_database_carcost <- database[, c("RID", "costs_down")]
# Remove rows with missing values in any of the selected columns
likert_database_carcost_1 <- likert_database_carcost[complete.cases(likert_database_carcost), ]

#Fake data set
RID <- c("99991", "99992", "99993", "99994")
costs_down <- c(1, 2, 3, 4)
fake <- data.frame(RID, costs_down)


#Some weird shit idk
likert_database_carcost_2 <- rbind(likert_database_carcost_1, fake)
likert_database_carcost_2$costs_down_f <- as.factor(likert_database_carcost_2$costs_down)

#Attach factor levels
factor_levels <- c("No cost reduction","Cost reduction up to 100 Euros","Cost reduction from 100 to 200 Euros",
                   "Cost reduction over 200 Euros")

levels(likert_database_carcost_2$costs_down_f) <- factor_levels

#Remove fake data
nrow(likert_database_carcost_2)
likert_database_carcost_3 <- subset(likert_database_carcost_2, RID < 99991)
nrow(likert_database_carcost_3)


###Remove RID
likert_database_carcost_4 <- as.data.frame(likert_database_carcost_3[, c("costs_down_f")])

###Include Likert Statement
VarHeadings <- c("How much would your monthly costs reduce by selling a car and using a mobility guarantee?")

names(likert_database_carcost_4) <- VarHeadings
colnames(likert_database_carcost_4)

#Behold the Likert graph!
library(likert)
library(ggplot2)
p <- likert(likert_database_carcost_4)
a <- likert.bar.plot(p, legend.position = "right", text.size = 4, bar.height=0.6) +
  theme(text = element_text(size = rel(4)),axis.text.y = element_text(size = rel(2))) +
  theme_update(legend.text = element_text(size = rel(0.7))) +
  theme_classic()
a <- a + 
  ggtitle("Potential monthly cost reduction if mobility guarantee purchased and car(s) sold (n=106)") +
  labs(caption = "Applies to rural drivers considering selling car and buying mobility guarantee") +
  theme(plot.caption = element_text(face = "italic"))
plot(a)

#Check number of results - 106
table(database$costs_down)

###########################
##REASONS TO GET RID OF CAR - 1 car
########################
database_og = load("database_PT_final.Rda")
#Filter by rural areas#
database<-filter(database,p_place_inhab<3)
likert_database_carbye <- database[, c("RID", "reasons_getrid_1v_soso_1", "reasons_getrid_1v_soso_2", "reasons_getrid_1v_soso_3", "reasons_getrid_1v_soso_4")]
# Remove rows with missing values in any of the selected columns
likert_database_carbye_1 <- likert_database_carbye[complete.cases(likert_database_carbye), ]

#Fake data set
RID <- c("99991", "99992")
reasons_getrid_1v_soso_1 <- c(1, 2)
reasons_getrid_1v_soso_2 <- c(1, 2)
reasons_getrid_1v_soso_3 <- c(1, 2)
reasons_getrid_1v_soso_4 <- c(1, 2)
fake <- data.frame(RID, reasons_getrid_1v_soso_1, reasons_getrid_1v_soso_2, reasons_getrid_1v_soso_3, reasons_getrid_1v_soso_4)


#Some weird shit idk
likert_database_carbye_2 <- rbind(likert_database_carbye_1, fake)
likert_database_carbye_2$reasons_getrid_1v_soso_1_f <- as.factor(likert_database_carbye_2$reasons_getrid_1v_soso_1)
likert_database_carbye_2$reasons_getrid_1v_soso_2_f <- as.factor(likert_database_carbye_2$reasons_getrid_1v_soso_2)
likert_database_carbye_2$reasons_getrid_1v_soso_3_f <- as.factor(likert_database_carbye_2$reasons_getrid_1v_soso_3)
likert_database_carbye_2$reasons_getrid_1v_soso_4_f <- as.factor(likert_database_carbye_2$reasons_getrid_1v_soso_4)

#Attach factor levels
factor_levels <- c("Not selected","Selected")

levels(likert_database_carbye_2$reasons_getrid_1v_soso_1_f) <- factor_levels
levels(likert_database_carbye_2$reasons_getrid_1v_soso_2_f) <- factor_levels
levels(likert_database_carbye_2$reasons_getrid_1v_soso_3_f) <- factor_levels
levels(likert_database_carbye_2$reasons_getrid_1v_soso_4_f) <- factor_levels

#Remove fake data
nrow(likert_database_carbye_2)
likert_database_carbye_3 <- subset(likert_database_carbye_2, RID < 99991)
nrow(likert_database_carbye_3)


###Remove RID
likert_database_carbye_4 <- as.data.frame(likert_database_carbye_3[, c("reasons_getrid_1v_soso_1_f", "reasons_getrid_1v_soso_2_f", "reasons_getrid_1v_soso_3_f", "reasons_getrid_1v_soso_4_f")])

###Include Likert Statement
VarHeadings <- c("Cost saving thanks to mobility guarantee", "Using a mobility guarantee is more convenient", "I don't like driving a car anyway", "The mobility guarantee makes owning a car unnecessary")

names(likert_database_carbye_4) <- VarHeadings
colnames(likert_database_carbye_4)

#Behold the Likert graph!
library(likert)
library(ggplot2)
p <- likert(likert_database_carbye_4)
a <- likert.bar.plot(p, legend.position = "right", text.size = 4, bar.height=0.6) +
  theme(text = element_text(size = rel(4)),axis.text.y = element_text(size = rel(2))) +
  theme_update(legend.text = element_text(size = rel(0.7))) +
  theme_classic()

a <- a + ggtitle("One car owned: Reasons to get rid of car (n=32)")+
  labs(caption = "Applies to rural drivers who own one car and who considered purchasing a mobility guarantee") +
  theme(plot.caption = element_text(face = "italic"))
plot(a)

#Check number of results - 32
table(database$reasons_getrid_1v_soso_1)

###########################
##REASONS TO GET RID OF CAR - MANY CARS##
########################
database_og = load("database_PT_final.Rda")
#Filter by rural areas#
database<-filter(database,p_place_inhab<3)
likert_database_carsbye <- database[, c("RID", "reasons_getrid_2v_min1drop_1", "reasons_getrid_2v_min1drop_2", "reasons_getrid_2v_min1drop_3", "reasons_getrid_2v_min1drop_4")]
# Remove rows with missing values in any of the selected columns
likert_database_carsbye_1 <- likert_database_carsbye[complete.cases(likert_database_carsbye), ]

#Fake data set
RID <- c("99991", "99992")
reasons_getrid_2v_min1drop_1 <- c(1, 2)
reasons_getrid_2v_min1drop_2 <- c(1, 2)
reasons_getrid_2v_min1drop_3 <- c(1, 2)
reasons_getrid_2v_min1drop_4 <- c(1, 2)
fake <- data.frame(RID, reasons_getrid_2v_min1drop_1, reasons_getrid_2v_min1drop_2, reasons_getrid_2v_min1drop_3, reasons_getrid_2v_min1drop_4)


#Some weird shit idk
likert_database_carsbye_2 <- rbind(likert_database_carsbye_1, fake)
likert_database_carsbye_2$reasons_getrid_2v_min1drop_1_f <- as.factor(likert_database_carsbye_2$reasons_getrid_2v_min1drop_1)
likert_database_carsbye_2$reasons_getrid_2v_min1drop_2_f <- as.factor(likert_database_carsbye_2$reasons_getrid_2v_min1drop_2)
likert_database_carsbye_2$reasons_getrid_2v_min1drop_3_f <- as.factor(likert_database_carsbye_2$reasons_getrid_2v_min1drop_3)
likert_database_carsbye_2$reasons_getrid_2v_min1drop_4_f <- as.factor(likert_database_carsbye_2$reasons_getrid_2v_min1drop_4)

#Attach factor levels
factor_levels <- c("Not selected","Selected")

levels(likert_database_carsbye_2$reasons_getrid_2v_min1drop_1_f) <- factor_levels
levels(likert_database_carsbye_2$reasons_getrid_2v_min1drop_2_f) <- factor_levels
levels(likert_database_carsbye_2$reasons_getrid_2v_min1drop_3_f) <- factor_levels
levels(likert_database_carsbye_2$reasons_getrid_2v_min1drop_4_f) <- factor_levels

#Remove fake data
nrow(likert_database_carsbye_2)
likert_database_carsbye_3 <- subset(likert_database_carsbye_2, RID < 99991)
nrow(likert_database_carsbye_3)


###Remove RID
likert_database_carsbye_4 <- as.data.frame(likert_database_carsbye_3[, c("reasons_getrid_2v_min1drop_1_f", "reasons_getrid_2v_min1drop_2_f", "reasons_getrid_2v_min1drop_3_f", "reasons_getrid_2v_min1drop_4_f")])

###Include Likert Statement
VarHeadings <- c("Cost saving thanks to mobility guarantee", "Using a mobility guarantee is more convenient", "I don't like driving a car anyway", "The mobility guarantee makes owning a car unnecessary")

names(likert_database_carsbye_4) <- VarHeadings
colnames(likert_database_carsbye_4)

#Behold the Likert graph!
library(likert)
library(ggplot2)
p <- likert(likert_database_carsbye_4)
a <- likert.bar.plot(p, legend.position = "right", text.size = 4, bar.height=0.6) +
  theme(text = element_text(size = rel(4)),axis.text.y = element_text(size = rel(2))) +
  theme_update(legend.text = element_text(size = rel(0.7))) +
  theme_classic()

a <- a + ggtitle("Many cars owned: Reasons to get rid of car (n=74)")+
  labs(caption = "Applies to rural drivers who own several cars and who considered purchasing a mobility guarantee") +
  theme(plot.caption = element_text(face = "italic"))
plot(a)

#Check number of results - 74
table(database$reasons_getrid_2v_min1drop_1)

#################################
##Distances####
#################################

table(database$distances)
# Creating a data frame
data_distances <- data.frame(no_change = 87, travel_more = 97, travel_less = 40)

# Making a pie chart
pie(c(data_distances$no_change, data_distances$travel_more, data_distances$travel_less), labels = c(paste("No change:", data_distances$no_change), 
                                                                          paste("Travel farther:", data_distances$travel_more),
                                                                          paste("Travel less far:", data_distances$travel_less)), 
                                                                          main = "How would the distance you travel change if you used a mobility guarantee?", 
                                                                          col = c("#e5e5e5","#5ab4ac", "#d8b365"))



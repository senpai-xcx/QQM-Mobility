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
###MODEL TO SEE IF PERSONA WOULD BUY###
###########################################

#Jump to line XXXXX for the model. Code below is creating dummy variables#


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
#0 = yes; 1 = no

table(database$buy_dummy)

##CREATE DUMMY VARIABLE FOR PERSONAS##
#Peripheral Resident#
library(dplyr)
database <- database %>%
  mutate(peripheral_dummy = ifelse(p_central_nonwien %in% c("4"), "0", "1"))

database <-
  database %>%
  mutate(peripheral_dummy = as.factor(peripheral_dummy))
#0 = yes; 1 = no

table(database$peripheral_dummy)

#The Frequent Driver#
library(dplyr)
database <- database %>%
  mutate(frequentdriver_dummy = ifelse(p_mob_rural %in% c("1"), "0", "1"))

database <-
  database %>%
  mutate(frequentdriver_dummy = as.factor(frequentdriver_dummy))
#0 = yes; 1 = no

table(database$frequentdriver_dummy)

#The Environmentalist#
library(dplyr)
database <- database %>%
  mutate(environmentalist_dummy = ifelse(p_mob_rural %in% c("1"), "0", "1"))

database <-
  database %>%
  mutate(environmentalist_dummy = as.factor(environmentalist_dummy))
#0 = yes; 1 = no

table(database$environmentalist_dummy)

#The Elderly#
library(dplyr)
database <- database %>%
  mutate(elderly_dummy = ifelse(age_front >= 60, 0, 1))


database <-
  database %>%
  mutate(elderly_dummy = as.factor(elderly_dummy))
#0 = yes; 1 = no

table(database$elderly_dummy)

#The Pupil#
library(dplyr)
database <- database %>%
  mutate(pupil_dummy = ifelse(status %in% c("3"), "0", "1"))

database <-
  database %>%
  mutate(pupil_dummy = as.factor(pupil_dummy))
#0 = yes; 1 = no

table(database$pupil_dummy)

#The Stay-At-Home-Parent#
library(dplyr)
database <- database %>%
  mutate(homeparent_dummy = ifelse(status %in% c("5"), "0", "1"))

database <-
  database %>%
  mutate(homeparent_dummy = as.factor(homeparent_dummy))
#0 = yes; 1 = no

table(database$homeparent_dummy)

#The Full-Time Worker#
library(dplyr)
database <- database %>%
  mutate(worker_dummy = ifelse(status %in% c("1"), "0", "1"))

database <-
  database %>%
  mutate(worker_dummy = as.factor(worker_dummy))
#0 = yes; 1 = no

table(database$worker_dummy)

#The Parent#
library(dplyr)
database <- database %>%
  mutate(parent_dummy = ifelse(children_1 %in% c("2") | 
                                 children_2 %in% c("2") |
                                 children_3 %in% c("2"), "0", "1"))

database <-
  database %>%
  mutate(parent_dummy = as.factor(parent_dummy))
#0 = yes; 1 = no

table(database$parent_dummy)

#The Working Commuter UPDATE#
library(dplyr)
database <- database %>%
  mutate(parent_dummy = ifelse(children_1 %in% c("2") | 
                                 children_2 %in% c("2") |
                                 children_3 %in% c("2"), "0", "1"))

database <-
  database %>%
  mutate(parent_dummy = as.factor(parent_dummy))
#0 = yes; 1 = no

table(database$parent_dummy)

##ORDINAL LOGISTIC REGRESSION MODEL##
model <- glm(buy_dummy ~ peripheral_dummy + 
                frequentdriver_dummy + 
               environmentalist_dummy + 
               elderly_dummy + 
               pupil_dummy + 
               homeparent_dummy + 
               worker_dummy +
               parent_dummy
               
             , data = database, family = "binomial")
summary(model)

#NB - I think I have issues with collinearity, and possibly something called the "dummy variable trap".

##PROBABILITIES##

#NB - I think these aren't the results I'm looking for. I'd like to calculate the probability of buying a mobility guarantee for each persona.

# Predict probabilities for each observation
predicted_probabilities <- predict(model, type = "response")

# Combine the original dataset with the predicted probabilities
database_with_probs <- cbind(database, predicted_probabilities)

# Show the results
table(database_with_probs$predicted_probabilities)


##NEXT STEPS - LOOK AT OTHER POST-CHOICE EXPERIMENT VARIABLES#




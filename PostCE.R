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


###MODEL TO SEE IF PERSONA WOULD BUY###
#Creating Dummy Variable#
# Assuming 'category_var' is your categorical variable
#database$category_var <- factor(mydata$category_var, levels = c("Value1", "Value2", "Value3", "Value4", "Value5", "Value6"))

# Create dummy variables
#dummy_variables <- model.matrix(~ category_var - 1, data = mydata)

# Give meaningful column names to the dummy variables
#colnames(dummy_variables) <- c("Yes_Value1", "Yes_Value2", "Yes_Value3", "Yes_Value4", "Yes_Value5", "No_Value6")

# Attach the dummy variables to your dataset
#mydata <- cbind(mydata, dummy_variables)

library(dplyr)
database <- database %>%
  mutate(combined_dummy = ifelse(buy_actually_e1 %in% c("1", "2", "3", "4", "5") | 
                                  buy_actually_e2 %in% c("1", "2", "3", "4", "5") |
                                  buy_actually_e3 %in% c("1", "2", "3", "4", "5") | 
                                  buy_actually_e4 %in% c("1", "2", "3", "4", "5") | 
                                  buy_actually_e5 %in% c("1", "2", "3", "4", "5"), "1", "2"))

#1 = yes; 2 = no

table(database$combined_dummy)

#INCOME#
#model <- glm(buy_actually_e1 ~ predictor1 + predictor2 + ..., data = mydata, family = "binomial")




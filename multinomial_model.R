# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)

# My version
library(dplyr)
load("database_PT_final.Rda")
#NB - the data is called "database"

database_choices=read.csv("finaldata.csv",header=TRUE)

database<-inner_join(database_choices, database, by ='RID')

#Stef continued

i<-!is.na(database$pref1)
database<-filter(database,i==TRUE)

database$buy_actually<-ifelse(database$SEQ==1,database$buy_actually_e1,
                              ifelse(database$SEQ==2,database$buy_actually_e2,
                                     ifelse(database$SEQ==3,database$buy_actually_e3, 
                                            ifelse(database$SEQ==4,database$buy_actually_e4,
                                                   database$buy_actually_e5 ))))

database$klimaticket<-(database$p_season_1-1)+(database$p_season_2-1)
database$klimaticket<-ifelse(database$klimaticket<2,database$klimaticket,1)

#database<-filter(database,buy_actually==6)
#database<-filter(database,p_mtools_1==2)

#PT usage
#database<-filter(database,p_mob_ptintensity<4)
#INCOME
#database<-filter(database,income<4)
#intention to sell car
#database<-filter(database,p_mtools_1==2&buy_sellcar>0)
#travel in rural areas
#database<-filter(database,p_mob_rural==1)
#travel during night time
#database<-filter(database,p_mob_time<3)
#vollzeit
#database<-filter(database,status==1)
#low climate concern (<4)
#database<-filter(database,Statements_1<4)
#has matura
#database<-filter(database,p_matura==1)
#went through survey fast
#database<-filter(database,DURATION<500)

#//////////////////////////////////////#
#########PERSONAS################
#//////////////////////////////////////#

#RURAL/LOCATION
database<-filter(database,p_place_inhab<3)
#database<-filter(database,p_place_inhab>2)

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
#database<-filter(database,kilometrage_per_car_1>=5 | kilometrage_per_car_2>=5 | kilometrage_per_car_3>=5 | kilometrage_per_car_4>=5)

##THE FREQUENT DRIVER##
#database<-filter(database,p_mob_rural==1)

##THE LATE NIGHT DRIVER##
#database<-filter(database,p_mob_time==1)

##THE LOW-INCOME RESIDENT##
#database<-filter(database,income<=2)

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

####MOBILITY GUARANTEE CUSTOMIATION####
#remove those who never consider buying a mobility guarantee
#database<-filter(database,buy_never==0)
#remove those who always opt to buy mobility guarantee
#database<-filter(database,buy_not>0) 

database <- database %>% rename("Spatial_1" = "a1_x2",
                                "Temporal_1" = "a1_x3",
                                "DRTPattern_1" = "a1_x4",
                                "Waitingtime_1" = "a1_x5",
                                "Discount_1" = "a1_x6",
                                "Compensation_1" = "a1_x7",
                                "Price_1" = "a1_x8",
                                "Spatial_2" = "a3_x2",
                                "Temporal_2" = "a2_x3",
                                "DRTPattern_2" = "a2_x4",
                                "Waitingtime_2" = "a2_x5",
                                "Discount_2" = "a2_x6",
                                "Compensation_2" = "a2_x7",
                                "Price_2" = "a2_x8",
                                "CHOICE" = "pref1"
)

save(database,file="database_PT_final_choices.Rda")

#Framing-Variable ist "deck" (1=wie bisher, 2=Mindeststandard) und Kaufen ist 
#"buy_actually_e1 - buy_actually_e5" (6=Nein, 1 und 3=Kaufen ja, aber behalte, 
#4 und 5=Kaufen ja und abschaffen <- 1-5 je nach: ein oder mehr als ein Auto im HH)


### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_SP",
  modelDescr      = "Simple MNL model on PT guarantee choice",
  indivID         = "RID", 
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(
  b_spatial  = 0,
  b_temporal  = 0,
  b_drtpattern  = 0,
  b_waiting = 0,
  b_discount  = 0,
  b_compensation  = 0,
  b_price = 0
  #              b_temporal17=0,
  #              b_temporal24=0,
  #              b_comp_improved=0,
  #              b_comp_generous=0
)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[["alt1"]]  = b_spatial  * Spatial_1 + b_temporal * Temporal_1 + b_drtpattern * DRTPattern_1 +
    b_waiting * Waitingtime_1 + b_discount * Discount_1 + b_compensation * Compensation_1 + b_price * Price_1
  V[["alt2"]]  = b_spatial  * Spatial_2 + b_temporal * Temporal_2 + b_drtpattern * DRTPattern_2 +
    b_waiting * Waitingtime_2 + b_discount * Discount_2 + b_compensation * Compensation_2 + b_price * Price_2
  
  # V[["alt1"]]  = b_price*(b_spatial  * Spatial_1 + b_temporal * Temporal_1 + b_drtpattern * DRTPattern_1 +
  #   b_waiting * Waitingtime_1 + b_discount * Discount_1 + b_compensation * Compensation_1 + Price_1)
  # V[["alt2"]]  = b_price*(b_spatial  * Spatial_2 + b_temporal * Temporal_2 + b_drtpattern * DRTPattern_2 +
  #   b_waiting * Waitingtime_2 + b_discount * Discount_2 + b_compensation * Compensation_2 + Price_2)
  # 
  
  #  V[["alt1"]]  = asc_1 + b_spatial  * Spatial_1 + b_temporal17 * (Temporal_1==17) +b_temporal24 * (Temporal_1==24) + b_drtpattern * DRTPattern_1 +
  #    b_waiting * Waitingtime_1 + b_discount * Discount_1 + b_comp_improved * (Compensation_1==2) + b_comp_generous * (Compensation_1==3) + b_price * Price_1
  #  V[["alt2"]]  = b_spatial  * Spatial_2 + + b_temporal17 * (Temporal_2==17) +b_temporal24 * (Temporal_2==24) + + b_drtpattern * DRTPattern_2 +
  #    b_waiting * Waitingtime_2 + b_discount * Discount_2 + b_comp_improved * (Compensation_2==2) + b_comp_generous * (Compensation_2==3) + b_price * Price_2
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2), 
    avail         = 1, 
    choiceVar     = CHOICE,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)



deltaMethod_settings=list(expression=c(WTP_spatial="b_spatial/b_price",
                                       WTP_temporal  = "b_temporal/b_price",
                                       WTP_drtpattern  = "b_drtpattern/b_price",
                                       WTP_waiting = "b_waiting/b_price",
                                       WTP_discount  = "b_discount/b_price",
                                       WTP_compensation  = "b_compensation/b_price"))

apollo_deltaMethod(model, deltaMethod_settings)


#PRINTING COEFFICIENT DATA
# Assuming model is your Apollo model
Coefficients <- model[["betaStart"]]

# Convert beta_start into a data frame
Coefficients_df <- as.data.frame(Coefficients)

# Display the table
print(Coefficients_df)

#PRINTING T VALUES
# Assuming model is your Apollo model
Tvalues <- model[["tstatBGW"]]

# Convert beta_start into a data frame
Tvalues_df <- as.data.frame(Tvalues)

# Display the table
print(Tvalues_df)

model_wtp_df <- apollo_deltaMethod(model, deltaMethod_settings)
#print(database$age_front)



############################################
##############DATA FRAMES###################
############################################

#DOOR TO DOOR#
data_drt <- data.frame(
  Persona = c("Full Sample", 
              "Rural Person", 
              "The Urban Resident", 
              "The Peripheral Resident",
              "The Environmentalist",
              "The Rail Season Ticket Holder",
              "The Working Commuter"),
  WTP_Euro = c(14.4433,
               14.6847,
               11.1858,
               27.7081,
               21.4257,
               19.423,
               17.5899),
  WTP_T_Ratio = c(-4.94,
                  -3.12,
                  -2.37,
                  -3.25,
                  -2.18,
                  -1.7,
                  -1.86)
)

# Displaying the data frame
print(data_drt)

#SPATIAL COVERAGE#
# Creating a data frame
data_spatial <- data.frame(
  Persona = c("Full Sample",
              "Rural Person",
              "The Peripheral Resident",
              "The Full-Time Worker",
              "The Parent",
              "The Frequent Driver",
              "The Late Night Driver",
              "The Environmentalist",
              "The Rail Season Ticket Holder"),
  WTP_Euro = c(14.6, 15.077, 17.206, 20.611, 20.606, 17.361, 25.12, 10.754, 17.704),
  WTP_T_Ratio = c(0.16, -5.86, -4.01, -4.98, -4.87, -4.16, -3.51, -2.22, -2.7)
)

# Display the created data frame
print(data_spatial)

#COMPENSATION#
# Creating a data frame
data_comp <- data.frame(
  Persona = c("Full Sample",
              "Rural Person",
              "The Full-Time Worker",
              "The Frequent Driver",
              "The Middle-Income Resident"),
  WTP_Euro = c(3.90, 7.63, 8.91, 10.11, 12.01)
)

# Display the created data frame
print(data_comp)

#COMPENSATION#
# Creating a data frame
data_temp<- data.frame(
  Persona = c("Full Sample",
              "Rural Person",
              "The Elderly",
              "The Late Night Driver",
              "The Low-Income Resident",
              "The Middle-Income Resident",
              "The High-Income Resident",
              "The Climate Change Skeptic"),
  WTP_Euro = c(7.04, 7.21, 2.86, 12.91, 5.64, 7.79, 4.54, 9.88)
)

# Display the created data frame
print(data_comp)

###############################################
#############VISUALISATIONS####################
###############################################

#########D2D#############
#TABLE#
library(formattable)

formattable(data_spatial, 
            align = c("l", rep("r", NCOL(data_drt) - 1)),
            list(
              `WTP_Euro` = formatter("span", 
                                     style = ~ style(
                                       color = ifelse(`WTP_Euro` > 14.6, "green", 
                                                      ifelse(`WTP_Euro` < 14.6, "red", "black"))
                                     ),
                                     ~ icontext(sapply(`WTP_Euro`, 
                                                       function(x) if (x < 14.6) "arrow-down" else if (x > 14.6) "arrow-up" else ""),
                                                `WTP_Euro`)
              ),
              `Persona` = formatter("span", 
                                           style = ~ style(
                                             color = "grey",
                                             font.weight = "bold"
                                           )
              ),
              `WTP_T_Ratio` = formatter("span", 
                                     style = ~ style(
                                       #color = ifelse(`WTP_T_Ratio` < -1.96, "blue","black"),
                                       font.weight = ifelse(`WTP_T_Ratio` < -1.96, "bold", "none")
                                     ))
              
            ),
            caption = "Willingness to Pay for 10p.p. spatial coverage"
)




###############################
#######TABLE###################
###############################

#SPATIAL COVERAGE#
# Creating a data frame
# Creating the data frame
data_WTP <- data.frame(
  Persona = c("The Rural Resident", "The Urban Resident", "The Pupil", "The Elderly", 
              "The Peripheral Resident", "The Full-Time Worker", 
              "The Frequent Driver", "The Late-Night Driver", 
              "The Rail Season-Ticket Holder", "The Environmentalist", 
              "The Climate Change Denier", "The Low-Income Resident", 
              "The Middle-Income Resident", "The High-Income Resident"),
  WTP_door_to_door = round(c(-14.6847, -11.1858, -13.0446, -15.0797, -27.7081, 
                       -11.9369, -16.6494, -11.4078, -19.423, -21.4257, 
                       -17.6626, -12.7072, -12.1471, -30.5703),2),
  t_ratio_1 = c(-3.12, -2.37, -0.86, -1.73, -3.25, -1.67, -2.03, -0.87, -1.74, 
                -2.18, -1.06, -1.43, -1.72, -1.54),
  WTP_spatial_coverage = round(c(-15.077, -6.5189, -14.749, -16.135, -17.206, 
                           -20.611, -17.361, -25.12, -17.704, -10.754, 
                           -16.611, -17.246, -12.455, -10.3929),2),
  t_ratio_2 = c(-5.86, -9.38, -1.5, -3.36, -4.01, -4.98, -4.16, -3.51, -2.7, 
                -2.22, -1.78, -3.98, -3.4, 0.22),
  WTP_temporal = round(c(-7.2137, -2.7584, -7.9756, -2.8552, -7.9685, -8.8137, 
                   -8.4016, -12.9122, -7.0452, -6.0431, -9.8846, -12.9122, 
                   -5.6441, -7.7913),2),
  t_ratio_3 = c(-10.71, 0.37, -2.89, -2.47, -6.81, -7.98, -7.09, -5.38, -4.02, 
                -4.6, -4.01, -5.38, -4.99, -7.41),
  WTP_compensation = c(-7.6332, -9.38, -5.9414, -3.421, -4.22, -12.0085, 
                       -10.1105, -2.9797, -5.6904, -6.1323, -13.9086, -5.4281, 
                       -8.9105, -5.8858),
  t_ratio_4 = round(c(-2.7, 0.37, -0.51, -0.64, -0.97, -2.64, -2.27, 0.38, -0.74, 
                -1.08, -1.49, -1.09, -2.07, -0.51),2)
)

# Print the created data frame
print(data_WTP)

data_WTP$WTP_door_to_door <- as.numeric(as.character(data_WTP$WTP_door_to_door))
data_WTP$WTP_spatial_coverage <- as.numeric(as.character(data_WTP$WTP_spatial_coverage))
data_WTP$WTP_temporal <- as.numeric(as.character(data_WTP$WTP_temporal))
data_WTP$WTP_compensation <- as.numeric(as.character(data_WTP$WTP_compensation))

data_WTP$t_ratio_1 <- as.numeric(as.character(data_WTP$t_ratio_1))
data_WTP$t_ratio_2 <- as.numeric(as.character(data_WTP$t_ratio_2))
data_WTP$t_ratio_3 <- as.numeric(as.character(data_WTP$t_ratio_3))
data_WTP$t_ratio_4 <- as.numeric(as.character(data_WTP$t_ratio_4))


library(formattable)
formattable(
  data_WTP, 
  align = c("l", rep("r", NCOL(data_WTP) - 1)),
  list(
    Persona = formatter("span", 
                        style = ~ style(
                          color = "grey",
                          font.weight = "bold"
                        )
    ),
    area(col = 2:2) ~ color_tile("#5ab4ac", "#e5e5e5"),
    area(col = 4:4) ~ color_tile("#5ab4ac", "#e5e5e5"),
    area(col = 6:6) ~ color_tile("#5ab4ac", "#e5e5e5"),
    area(col = 8:8) ~ color_tile("#5ab4ac", "#e5e5e5"),
    #area(col = 8:8) ~ color_tile("#5ab4ac", "#ebd9b2"),
    #`WTP_door_to_door` = color_bar("#acd9d5"),
    #`WTP_spatial_coverage` = color_bar("#ebd9b2"),
    #`WTP_temporal` = color_bar("#acd9d5"),
   # `WTP_compensation` = color_bar("#acd9d5"),
    `t_ratio_1` = formatter("span", 
                              style = ~ style(
                                font.weight = ifelse(`t_ratio_1` < -1.96, "bold", "none")
                              )),
    `t_ratio_2` = formatter("span", 
                            style = ~ style(
                              font.weight = ifelse(`t_ratio_2` < -1.96, "bold", "none")
                            )),
    `t_ratio_3` = formatter("span", 
                            style = ~ style(
                              font.weight = ifelse(`t_ratio_3` < -1.96, "bold", "none")
                            )),
    `t_ratio_4` = formatter("span", 
                            style = ~ style(
                              font.weight = ifelse(`t_ratio_4` < -1.96, "bold", "none")
                            ))
  ), caption = ("Willingness to Pays for DRT attributes")
)


####TEST
library(formattable)

formattable(data_spatial, 
            align = c("l", rep("r", NCOL(data_spatial) - 1)),
            list(
              `Persona` = formatter("span", 
                                    style = ~ style(
                                      color = "grey",
                                      font.weight = "bold"
                                    )
              ),
              `WTP_door_to_door` = color_bar("#acd9d5"),
              
            )
)

################
#CLUSTERED BAR#
###############

#library(ggplot2)

# Reorder the levels of Persona based on WTP_Euro
#data_spatial$Persona <- reorder(data_spatial$Persona, -data_spatial$WTP_Euro)

#ggplot(data_spatial, aes(x = Persona, y = WTP_Euro, fill = Persona)) +
 # geom_bar(stat = "identity", position = "dodge") +
 # geom_text(aes(label = sprintf("%.2f", WTP_Euro)), 
 #           vjust = -0.5, 
  #          position = position_dodge(width = 0.9), 
 # ##          size = 3, angle = 20, hjust = 0.5) +
 # labs(title = "Willingness to Pay for 10p.p. spatial coverage",
  #     x = "",
  #     y = "WTP in Euro") +
 # geom_hline(yintercept = 14.6, linetype = "dashed", size = 0.75, color="red") +
 # theme_minimal()


##TRY 2
library(ggplot2)

# Define a color scale manually for Persona column
color_scale <- scale_fill_manual(values = c(
  "#5ab4ac", "#7bb8ad", "#9dbdae", "#bec1af", "#dfc5b0",
  "#f1ccb3", "#f3d6bb", "#f5e1c3", "#f7ebcb", "#f9f5d3",
  "#faf1dc", "#f8e8e7", "#f5dee3", "#f3d4de"
))

# Reorder the levels of Persona based on WTP_Euro
data_spatial$Persona <- reorder(data_spatial$Persona, -data_spatial$WTP_Euro)

# Create the plot with the defined color scale
ggplot(data_spatial, aes(x = Persona, y = WTP_Euro)) +
  geom_bar(stat = "identity", aes(fill = Persona), position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", WTP_Euro)), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3, angle = 20, hjust = 0.5) +
  labs(title = "Willingness to pay for 10p.p. increase in spatial coverage",
       x = "",
       y = "WTP in Euro") +
  geom_hline(yintercept = 14.6, linetype = "dashed", size = 0.75, color = "red") +
  theme_minimal() +
  color_scale

##TRY 3 - DRT

library(ggplot2)

# Define a color scale manually for Persona column
color_scale <- scale_fill_manual(values = c(
  "#5ab4ac", "#7bb8ad", "#9dbdae", "#bec1af", "#dfc5b0",
  "#f1ccb3", "#f3d6bb", "#f5e1c3", "#f7ebcb", "#f9f5d3",
  "#faf1dc", "#f8e8e7", "#f5dee3", "#f3d4de"
))

# Reorder the levels of Persona based on WTP_Euro
data_drt$Persona <- reorder(data_drt$Persona, -data_drt$WTP_Euro)

# Create the plot with the defined color scale
ggplot(data_drt, aes(x = Persona, y = WTP_Euro)) +
  geom_bar(stat = "identity", aes(fill = Persona), position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", WTP_Euro)), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3, angle = 20, hjust = 0.5) +
  labs(title = "Willingness to pay for door to door service",
       x = "",
       y = "WTP in Euro") +
  geom_hline(yintercept = 14.6, linetype = "dashed", size = 0.75, color = "red") +
  theme_minimal() +
  color_scale

##TRY 3 - Compensation
library(ggplot2)

# Define a color scale manually for Persona column
color_scale <- scale_fill_manual(values = c(
  "#5ab4ac", "#7bb8ad", "#9dbdae", "#bec1af", "#dfc5b0",
  "#f1ccb3", "#f3d6bb", "#f5e1c3", "#f7ebcb", "#f9f5d3",
  "#faf1dc", "#f8e8e7", "#f5dee3", "#f3d4de"
))

# Reorder the levels of Persona based on WTP_Euro
data_comp$Persona <- reorder(data_comp$Persona, -data_comp$WTP_Euro)

# Create the plot with the defined color scale
ggplot(data_comp, aes(x = Persona, y = WTP_Euro)) +
  geom_bar(stat = "identity", aes(fill = Persona), position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", WTP_Euro)), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3, angle = 20, hjust = 0.5) +
  labs(title = "Willingness to pay for better compensation",
       x = "",
       y = "WTP in Euro") +
  geom_hline(yintercept = 3.9, linetype = "dashed", size = 0.75, color = "red") +
  theme_minimal() +
  color_scale



###TRY 3 - Temporal


library(ggplot2)

# Define a color scale manually for Persona column
color_scale <- scale_fill_manual(values = c(
  "#5ab4ac", "#7bb8ad", "#9dbdae", "#bec1af", "#dfc5b0",
  "#f1ccb3", "#f3d6bb", "#f5e1c3", "#f7ebcb", "#f9f5d3",
  "#faf1dc", "#f8e8e7", "#f5dee3", "#f3d4de"
))

# Reorder the levels of Persona based on WTP_Euro
data_temp$Persona <- reorder(data_temp$Persona, -data_temp$WTP_Euro)

# Create the plot with the defined color scale
ggplot(data_temp, aes(x = Persona, y = WTP_Euro)) +
  geom_bar(stat = "identity", aes(fill = Persona), position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", WTP_Euro)), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3, angle = 20, hjust = 0.5) +
  labs(title = "Willingness to pay to increase temporal coverage by one hour",
       x = "",
       y = "WTP in Euro") +
  geom_hline(yintercept = 7.04, linetype = "dashed", size = 0.75, color = "red") +
  theme_minimal() +
  color_scale


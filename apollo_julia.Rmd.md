
# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #
```{r}
### Clear memory
rm(list = ls())
setwd("~/Desktop/quanti/QQM-Mobility")

### Load data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)
load("database_PT_final.Rda")

### remove empty rows
library(dplyr)
database_choices=read.csv("finaldata.csv",header=TRUE)

database<-inner_join(database_choices, database, by ='RID')


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
#RURAL/LOCATION
#database<-filter(database,p_place_inhab<3)
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




### ESTIMATE LATENT CLASS MODEL

# ################################################################# #
# ################################################################# #
####### LATENT CLASS MODEL #############
# ################################################################# #
# ################################################################# #

### Initialise code
apollo_initialise()


apollo_control = list(
  modelName  ="LC_PT_guarantee",
  modelDescr ="LC model",
  indivID    ="RID"
)



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(b_spatial_1  = 0,
              b_temporal_1  = 0,
              b_drtpattern_1  = 0,
              b_waiting_1 = 0,
              b_discount_1  = 0,
              b_compensation_1  = 0,
              b_price_1 = 0,
              b_spatial_2  = 0,
              b_temporal_2  = 0,
              b_drtpattern_2  = 0,
              b_waiting_2 = 0,
              b_discount_2  = 0,
              b_compensation_2  = 0,
              b_price_2 = 0,
              delta_1 =0,
              delta_2 =1
 #             gamma_framing=0,
 #            gamma_notbought=0
              )

#gamma additional as class membership equations

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("delta_1")

# fix all for b to zero

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #


apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["b_spatial"]] = list(b_spatial_1, b_spatial_2)
  lcpars[["b_temporal"]] = list(b_temporal_1, b_temporal_2)
  lcpars[["b_drtpattern"]] = list(b_drtpattern_1, b_drtpattern_2)
  lcpars[["b_waiting"]] = list(b_waiting_1,  b_waiting_2)
  lcpars[["b_discount"]] = list(b_discount_1, b_discount_2)
  lcpars[["b_compensation"]] = list(b_compensation_1, b_compensation_2)
  lcpars[["b_price"]] = list(b_price_1, b_price_2)

  
  V=list() #class membership equations
  V[["class_1"]] = delta_1 
  V[["class_2"]] = delta_2 #+ gamma_notbought*buy_never+gamma_framing*deck
  
  mnl_settings = list(
    alternatives = c(class_1=1, class_2=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  
  
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}



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
  
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    avail        = list(1),
    choiceVar    = CHOICE
  )
  

  ### Loop over classes
  s=1
  while(s<=2){
    
    ### Compute class-specific utilities
    V = list()
    
    V[["alt1"]]  = b_spatial[[s]]  * Spatial_1 + b_temporal[[s]] * Temporal_1 + b_drtpattern[[s]] * DRTPattern_1 +
      b_waiting[[s]] * Waitingtime_1 + b_discount[[s]] * Discount_1 + b_compensation[[s]] * Compensation_1 + b_price[[s]] * Price_1
    V[["alt2"]]  = b_spatial[[s]]  * Spatial_2 + b_temporal[[s]] * Temporal_2 + b_drtpattern[[s]] * DRTPattern_2 +
      b_waiting[[s]] * Waitingtime_2 + b_discount[[s]] * Discount_2 + b_compensation[[s]] * Compensation_2 + b_price[[s]] * Price_2

    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs,
                        estimate_settings=list(writeIter=FALSE))

### Show output in screen
apollo_modelOutput(model)

### Save output to file(s)
apollo_saveOutput(model)


deltaMethod_settings=list(expression=c(WTP_spatial1="b_spatial_1/b_price_1",
                                       WTP_temporal1  = "b_temporal_1/b_price_1",
                                       WTP_drtpattern1  = "b_drtpattern_1/b_price_1",
                                       WTP_waiting1 = "b_waiting_1/b_price_1",
                                       WTP_discount1  = "b_discount_1/b_price_1",
                                       WTP_compensation1  = "b_compensation_1/b_price_1",
                                       WTP_spatial2="b_spatial_2/b_price_2",
                                       WTP_temporal2  = "b_temporal_2/b_price_2",
                                       WTP_drtpattern2  = "b_drtpattern_2/b_price_2",
                                       WTP_waiting2 = "b_waiting_2/b_price_2",
                                       WTP_discount2  = "b_discount_2/b_price_2",
                                       WTP_compensation2  = "b_compensation_2/b_price_2"))

apollo_deltaMethod(model, deltaMethod_settings)





# 
# #######LATENT CLASS MODEL WITH 3!! CLASSES - does not really yield interesting insights

# 
# 
# 
# ### ESTIMATE LATENT CLASS MODEL
# 
# # ################################################################# #
# # ################################################################# #
# ####### LATENT CLASS MODEL #############
# # ################################################################# #
# # ################################################################# #
# 
# ### Initialise code
# apollo_initialise()
# 
# 
# apollo_control = list(
#   modelName  ="LC_PT_guarantee_3 classes",
#   modelDescr ="LC model_3cl",
#   indivID    ="RID"
# )
# 
# 
# 
# # ################################################################# #
# #### DEFINE MODEL PARAMETERS                                     ####
# # ################################################################# #
# 
# ### Vector of parameters, including any that are kept fixed in estimation
# apollo_beta=c(b_spatial_1  = 0,
#               b_temporal_1  = 0,
#               b_drtpattern_1  = 0,
#               b_waiting_1 = 0,
#               b_discount_1  = 0,
#               b_compensation_1  = 0,
#               b_price_1 = 0,
#               b_spatial_2  = 0,
#               b_temporal_2  = 0,
#               b_drtpattern_2  = 0,
#               b_waiting_2 = 0,
#               b_discount_2  = 0,
#               b_compensation_2  = 0,
#               b_price_2 = 0,
#               b_spatial_3  = 0,
#               b_temporal_3  = 0,
#               b_drtpattern_3  = 0,
#               b_waiting_3 = 0,
#               b_discount_3  = 0,
#               b_compensation_3  = 0,
#               b_price_3 = 0,
#               delta_1 =0,
#               delta_2 =1,
#               delta_3=0.5
#               #             gamma_framing=0,
#               #            gamma_notbought=0
# )
# 
# #gamma additional as class membership equations
# 
# ### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
# apollo_fixed = c("delta_1")
# 
# # fix all for b to zero
# 
# # ################################################################# #
# #### DEFINE LATENT CLASS COMPONENTS                              ####
# # ################################################################# #
# 
# 
# apollo_lcPars=function(apollo_beta, apollo_inputs){
#   lcpars = list()
#   lcpars[["b_spatial"]] = list(b_spatial_1, b_spatial_2,b_spatial_3)
#   lcpars[["b_temporal"]] = list(b_temporal_1, b_temporal_2,b_temporal_3)
#   lcpars[["b_drtpattern"]] = list(b_drtpattern_1, b_drtpattern_2,b_drtpattern_3)
#   lcpars[["b_waiting"]] = list(b_waiting_1,  b_waiting_2,b_waiting_3)
#   lcpars[["b_discount"]] = list(b_discount_1, b_discount_2,b_discount_3)
#   lcpars[["b_compensation"]] = list(b_compensation_1, b_compensation_2,b_compensation_3)
#   lcpars[["b_price"]] = list(b_price_1, b_price_2,b_price_3)
#   
#   
#   V=list() #class membership equations
#   V[["class_1"]] = delta_1 
#   V[["class_2"]] = delta_2 #+ gamma_notbought*buy_never+gamma_framing*deck
#   V[["class_3"]] = delta_3
#   
#   mnl_settings = list(
#     alternatives = c(class_1=1, class_2=2,class_3=3), 
#     avail        = 1, 
#     choiceVar    = NA, 
#     V            = V
#   )
#   
#   
#   lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
#   
#   lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
#   
#   return(lcpars)
# }
# 
# 
# 
# # ################################################################# #
# #### GROUP AND VALIDATE INPUTS                                   ####
# # ################################################################# #
# 
# apollo_inputs = apollo_validateInputs()
# 
# # ################################################################# #
# #### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# # ################################################################# #
# 
# apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
#   
#   ### Attach inputs and detach after function exit
#   apollo_attach(apollo_beta, apollo_inputs)
#   on.exit(apollo_detach(apollo_beta, apollo_inputs))
#   
#   ### Create list of probabilities P
#   P = list()
#   
#   
#   ### Define settings for MNL model component that are generic across classes
#   mnl_settings = list(
#     alternatives  = c(alt1=1, alt2=2),
#     avail        = list(1),
#     choiceVar    = CHOICE
#   )
#   
#   
#   ### Loop over classes
#   s=1
#   while(s<=3){
#     
#     ### Compute class-specific utilities
#     V = list()
#     
#     V[["alt1"]]  = b_spatial[[s]]  * Spatial_1 + b_temporal[[s]] * Temporal_1 + b_drtpattern[[s]] * DRTPattern_1 +
#       b_waiting[[s]] * Waitingtime_1 + b_discount[[s]] * Discount_1 + b_compensation[[s]] * Compensation_1 + b_price[[s]] * Price_1
#     V[["alt2"]]  = b_spatial[[s]]  * Spatial_2 + b_temporal[[s]] * Temporal_2 + b_drtpattern[[s]] * DRTPattern_2 +
#       b_waiting[[s]] * Waitingtime_2 + b_discount[[s]] * Discount_2 + b_compensation[[s]] * Compensation_2 + b_price[[s]] * Price_2
#     
#     
#     mnl_settings$V = V
#     mnl_settings$componentName = paste0("Class_",s)
#     
#     ### Compute within-class choice probabilities using MNL model
#     P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
#     
#     ### Take product across observation for same individual
#     P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
#     
#     s=s+1}
#   
#   ### Compute latent class model probabilities
#   lc_settings   = list(inClassProb = P, classProb=pi_values)
#   P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
#   
#   ### Prepare and return outputs of function
#   P = apollo_prepareProb(P, apollo_inputs, functionality)
#   return(P)
# }
# # ################################################################# #
# #### MODEL ESTIMATION                                            ####
# # ################################################################# #
# 
# #apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
# #apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
# 
# ### Estimate model
# model = apollo_estimate(apollo_beta, apollo_fixed, 
#                         apollo_probabilities, apollo_inputs,
#                         estimate_settings=list(writeIter=FALSE))
# 
# ### Show output in screen
# apollo_modelOutput(model)
# 
# ### Save output to file(s)
# apollo_saveOutput(model)
# 
# 
# 

####### 2 Class LC model with covariates
### ESTIMATE LATENT CLASS MODEL

# ################################################################# #
# ################################################################# #
####### LATENT CLASS MODEL #############
# ################################################################# #
# ################################################################# #

### Initialise code
apollo_initialise()


apollo_control = list(
  modelName  ="LC_PT_guarantee and explan var",
  modelDescr ="LC model and explan var",
  indivID    ="RID"
)



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(b_spatial_1  = 0,
              b_temporal_1  = 0,
              b_drtpattern_1  = 0,
              b_waiting_1 = 0,
              b_discount_1  = 0,
              b_compensation_1  = 0,
              b_price_1 = 0,
              b_spatial_2  = 0,
              b_temporal_2  = 0,
              b_drtpattern_2  = 0,
              b_waiting_2 = 0,
              b_discount_2  = 0,
              b_compensation_2  = 0,
              b_price_2 = 0,
              delta_1 =0,
              delta_2 =1,
              gamma_framing=0.5,
              gamma_notbought=0.5,
              gamma_hascar=0.5,
               gamma_lowinc=0.5,
               gamma_noincinf=0.5,
 #              gamma_higheduc=0.5,
  #             gamma_rural=0.5,
               gamma_highptuse=0.5,
               gamma_travelnight=0.5,
               gamma_travelrural=0.5
  #              gamma_vienna=0.5,
  #             gamma_klima=1
              #              
)

#gamma additional as class membership equations

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("delta_1")

# fix all for b to zero

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #


apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["b_spatial"]] = list(b_spatial_1, b_spatial_2)
  lcpars[["b_temporal"]] = list(b_temporal_1, b_temporal_2)
  lcpars[["b_drtpattern"]] = list(b_drtpattern_1, b_drtpattern_2)
  lcpars[["b_waiting"]] = list(b_waiting_1,  b_waiting_2)
  lcpars[["b_discount"]] = list(b_discount_1, b_discount_2)
  lcpars[["b_compensation"]] = list(b_compensation_1, b_compensation_2)
  lcpars[["b_price"]] = list(b_price_1, b_price_2)
  
  
  V=list() #class membership equations
  V[["class_1"]] = delta_1 
  V[["class_2"]] = delta_2 + gamma_notbought*(buy_actually==6)+gamma_hascar*(p_mtools_1==2)+gamma_framing*deck+
    +gamma_lowinc*(income<4)+gamma_noincinf*(income==7)+
    gamma_travelnight*(p_mob_time<3) + gamma_travelrural*(p_mob_rural<3)+
    gamma_highptuse*(p_mob_ptintensity<4) 
  # + gamma_klima * (klimaticket==1)  #gamma_rural*(p_place_inhab<3)#+gamma_framing*deck+gamma_highptuse*(p_mob_ptintensity<4) 
  
 # V[["class_2"]] = delta_2 + gamma_notbought*buy_never+gamma_hascar*(p_mtools_1==2)+
 # +gamma_lowinc*(income<4)+gamma_noincinf*(income==7)+gamma_higheduc*(p_matura==1)+
 #   gamma_travelnight*(p_mob_time<3) + gamma_travelrural*(p_mob_rural<3)+gamma_vienna*(p_bundesland_front==9)+
 #  + gamma_klima * (klimaticket==1)  #gamma_rural*(p_place_inhab<3)#+gamma_framing*deck+gamma_highptuse*(p_mob_ptintensity<4) 

  
  mnl_settings = list(
    alternatives = c(class_1=1, class_2=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  
  
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}



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
  
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    avail        = list(1),
    choiceVar    = CHOICE
  )
  
  
  ### Loop over classes
  s=1
  while(s<=2){
    
    ### Compute class-specific utilities
    V = list()
    
    V[["alt1"]]  =  (b_spatial[[s]]  * Spatial_1 + b_temporal[[s]] * Temporal_1 + b_drtpattern[[s]] * DRTPattern_1 +
      b_waiting[[s]] * Waitingtime_1 + b_discount[[s]] * Discount_1 + b_compensation[[s]] * Compensation_1 + b_price[[s]] *Price_1)
    V[["alt2"]]  =  (b_spatial[[s]]  * Spatial_2 + b_temporal[[s]] * Temporal_2 + b_drtpattern[[s]] * DRTPattern_2 +
      b_waiting[[s]] * Waitingtime_2 + b_discount[[s]] * Discount_2 + b_compensation[[s]] * Compensation_2 + b_price[[s]]*Price_2)
    
  #  V[["alt1"]]  =  b_price[[s]] * (b_spatial[[s]]  * Spatial_1 + b_temporal[[s]] * Temporal_1 + b_drtpattern[[s]] * DRTPattern_1 +
   #                    b_waiting[[s]] * Waitingtime_1 + b_discount[[s]] * Discount_1 + b_compensation[[s]] * Compensation_1 + Price_1)
  #  V[["alt2"]]  =  b_price[[s]]* (b_spatial[[s]]  * Spatial_2 + b_temporal[[s]] * Temporal_2 + b_drtpattern[[s]] * DRTPattern_2 +
  #                     b_waiting[[s]] * Waitingtime_2 + b_discount[[s]] * Discount_2 + b_compensation[[s]] * Compensation_2 + Price_2)
    
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs,
                        estimate_settings=list(writeIter=FALSE))

### Show output in screen
apollo_modelOutput(model)

### Save output to file(s)
apollo_saveOutput(model)

#########################
##### JULIA #########

#database$buy_actually<-ifelse(database$SEQ==1,database$NO_against_personal_1,
                            #  ifelse(database$SEQ==2,database$NO_against_personal_2,
                                   #  ifelse(database$SEQ==3,database$NO_against_personal_3, 
                                           # ifelse(database$SEQ==4,database$NO_against_personal_5,
                                                 #  ifelse(database$SEQ==5, database$NO_against_personal_6)
                                                 #  database$NO_against_personal_7 )))))

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


#V[["alt1"]]  = asc_1 + b_spatial  * Spatial_1 + b_temporal17 * (Temporal_1==17) +b_temporal24 * (Temporal_1==24) + b_drtpattern * DRTPattern_1 +
#   b_waiting * Waitingtime_1 + b_discount * Discount_1 + b_comp_improved * (Compensation_1==2) + b_comp_generous * (Compensation_1==3) + b_price * Price_1
#V[["alt2"]]  = b_spatial  * Spatial_2 + + b_temporal17 * (Temporal_2==17) +b_temporal24 * (Temporal_2==24) + + b_drtpattern * DRTPattern_2 +
#   b_waiting * Waitingtime_2 + b_discount * Discount_2 + b_comp_improved * (Compensation_2==2) + b_comp_generous * (Compensation_2==3) + b_price * Price_2


#### Nested Logit
#apollo_probabilities=function(apollo_beta,apollo_inputs,functionality="estimate"){ 
  ###SpecifynestsforNLmodel 
  #nlNests =list(root=1,PT=lambda_PT,fastPT=lambda_fastPT)  
  ###SpecifytreestructureforNLmodel  
  #nlStructure=list()  
  #nlStructure[["root"]] =c("car","PT") 
  #nlStructure[["PT"]] =c("bus","fastPT")  
  #nlStructure[["fastPT"]]=c("air","rail") 
  ###DefinesettingsforNLmodel 
  #nl_settings<-list( 15 alternatives=c(car=1,bus=2,air=3,rail=4), 
                     #avail =list(car=av_car,bus=av_bus,air=av_air,rail=av_rail), 
                     #choiceVar =choice, 
                     #utilities =V, 
                     #nlNests =nlNests,  
                     #nlStructure =nlStructure 
  )  
  
  ###ComputeprobabilitiesusingNLmodel  
  #P[["model"]]=apollo_nl(nl_settings, functionality) 
  }
  
  ```

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

setwd("F:\\prod\\4-2\\CE-400\\Analysis\\R-Model\\Mixed Logit Model\\Combined")

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_Combined_Significants",
  modelDescr      = "Simple MNLtotal model on mode choice egress data - for bus, rick, walk",
  indivID         = "ID", 
  outputDirectory = "Sig_with_time_both"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
database = read.csv("F:/prod/4-2/CE-400/Analysis/R-Model/Mixed Logit Model/Combined/Combined_data_S12.csv",header=TRUE)

#Filtering only rick, walk, bus chosen data
database = subset(database, mode %in% c(1, 2, 7))
database$comp = ((database$c1==1) | (database$c2==1) | (database$c3==1) | (database$c4==1))
#database$age = ((database$age==20)*1 + (database$age==30)*2 + (database$age==40)*3 + (database$age==60)*4 + (database$age==70)*5)
database$month_inc = ((database$month_inc==5)*5 +(database$month_inc==20)*20 + (database$month_inc==30)*30 + (database$month_inc==60)*60 + ((database$month_inc==80) | (database$month_inc==150) | (database$month_inc==250))*80)
database$age = ((database$age == 20)*20 + (database$age == 30)*30 + (database$age == 40)*40 + ((database$age == 60) | (database$age == 70))*60)
#database = apollo_modeChoiceData
### for data dictionary, use ?apollo_modeChoiceData


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  
            asc_walk  = 0,
            asc_rick  = 0,
            asc_bus   = 0,
            
            
            b_tt_walk = 0,
            b_ac_tt_walk = 0,
            
            #b_tt_rick = 0,
            
            b_tt_bus  = 0,
            b_ac_tt_bus = 0,
            
            b_cost_rick = 0,
            b_ac_cost_rick = 0,
            
            mu_b_cost_bus = -0.1,
            sigma_b_cost_bus = 0.01,
            
            #b_cost_bus = 0,
            b_ac_cost_bus = 0,
            
            b_ac_rick = 0,
            b_ac_bus = 0,
  
            
            #b_11am_rick = 0,
            #b_1pm_rick = 0,
            #b_3pm_rick = 0,
            #b_5pm_rick = 0,
            #b_7pm_rick = 0,
            
            b_11am_bus = 0,
            b_1pm_bus = 0,
            b_3pm_bus = 0,
            #b_5pm_bus = 0,
            #b_7pm_bus = 0,
            
            
            #b_tp_business_rick = 0,
            b_tp_edu_rick = 0,
            #b_tp_medi_rick = 0,
            b_tp_job_rick = 0,
            #b_tp_pw_rick = 0,
            #b_tp_rec_rick = 0,
            #b_tp_rtrn_rick = 0,
            #b_tp_shop_rick = 0,
            
            #b_tp_business_bus = 0,
            b_tp_edu_bus = 0,
            b_tp_medi_bus = 0,
            #b_tp_job_bus = 0,
            #b_tp_pw_bus = 0,
            #b_tp_rec_bus = 0,
            b_tp_rtrn_bus = 0,
            #b_tp_shop_bus = 0,
            
            
            #b_TF1_rick = 0,
            #b_TF2_rick = 0,
            #b_TF3_rick = 0,
            b_TF4_rick = 0,
            b_TF5_rick = 0,
            #b_TF6_rick = 0,
            
            #b_TF1_bus = 0,
            #b_TF2_bus = 0,
            #b_TF3_bus = 0,
            b_TF4_bus = 0,
            b_TF5_bus = 0,
            #b_TF6_bus = 0,
            
            #b_freq_bus = 0,
            

            #b_bag_rick = 0,
            #b_obag_rick = 0,
            #b_shpbag_rick = 0,
            #b_lugg_rick = 0,
            #b_other_rick = 0,
            
            #b_bag_bus = 0,
            #b_obag_bus = 0,
            #b_shpbag_bus = 0,
            #b_lugg_bus = 0,
            #b_other_bus = 0,
            
            
            #b_comp_walk = 0,
            b_comp_rick = 0,
            #b_comp_bus = 0,
            
            
            #b_gp_business_rick = 0,
            b_gp_edu_rick = 0,
            #b_gp_medi_rick = 0,
            b_gp_job_rick = 0,
            #b_gp_pw_rick = 0,
            #b_gp_rec_rick = 0,
            #b_gp_shop_rick = 0,
            
            #b_gp_business_bus = 0,
            b_gp_edu_bus = 0,
            #b_gp_medi_bus = 0,
            b_gp_job_bus = 0,
            #b_gp_pw_bus = 0,
            #b_gp_rec_bus = 0,
            #b_gp_shop_bus = 0,
            
            
            #b_age1_rick = 0,
            #b_age2_rick = 0,
            #b_age3_rick = 0,
            #b_age4_rick = 0,
            #b_age5_rick = 0,
            
            #b_age1_bus = 0,
            b_age2_bus = 0,
            #b_age3_bus = 0,
            #b_age4_bus = 0,
            #b_age5_bus = 0,
            
            #b_fmale_walk = 0,
            b_fmale_rick = 0,
            #b_fmale_bus = 0,
            
            
            b_stud_rick = 0,
            #b_hwife_rick = 0,
            b_gov_rick = 0,
            b_pvt_rick = 0,
            #b_bzman_rick = 0,
            #b_rtd_rick = 0,
            #b_unemp_rick = 0,
            
            b_stud_bus = 0,
            #b_hwife_bus = 0,
            #b_gov_bus = 0,
            #b_pvt_bus = 0,
            #b_bzman_bus = 0,
            #b_rtd_bus = 0,
            #b_unemp_bus = 0,
            

            
            #b_inc1_rick = 0,
            #b_inc2_rick = 0,
            #b_inc3_rick = 0,
            b_inc4_rick = 0,
            b_inc5_rick = 0,
            #b_inc6_rick = 0,
            #b_inc7_rick = 0,
            
            #b_inc1_bus = 0,
            b_inc2_bus = 0,
            #b_inc3_bus = 0,
            #b_inc4_bus = 0,
            #b_inc5_bus = 0,
            #b_inc6_bus = 0,
            #b_inc7_bus = 0,
            

            #b_own_car_rick = 0,
            b_own_bike_rick = 0,
            #b_own_cyc_rick = 0,
            
            #b_own_car_bus = 0,
            #b_own_bike_bus = 0,
            #b_own_cyc_bus = 0,
            
            #b_mrt_walk = 0,
            #b_mrt_rick = 0,
            b_mrt_bus = 0
  
)
### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 800,
  interUnifDraws = c(),
  interNormDraws = c("draws_bus_cost"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  #randcoeff[["b_tt_walk"]] = -exp( mu_b_tt_walk + sigma_b_tt_walk * draws_walk_tt)
  #randcoeff[["b_tt_rick"]] = -exp( mu_b_tt_rick + sigma_b_tt_rick * draws_rick_tt)
  #randcoeff[["b_cost_rick"]] = -exp( mu_b_cost_rick + sigma_b_cost_rick * draws_rick_cost)
  randcoeff[["b_cost_bus"]] = -exp(mu_b_cost_bus + sigma_b_cost_bus * draws_bus_cost)
  
  #randcoeff[["b_wlk_tt"]] = -exp( mu_lg_b_wlk_tt + sigma_lg_b_wlk_tt * draws_walk_tt )
  
  
  return(randcoeff)
}

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_walk")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  

  V[["walk"]] = asc_walk + (b_tt_walk + b_ac_tt_walk*feed) * walk_tt                          
               
  
  V[["rick"]] = asc_rick + (b_cost_rick + b_ac_cost_rick*feed) * rick_cost + b_ac_rick*feed +  #b_tt_rick * rick_tt 
                #b_tp_business_rick*(trip_purp==1) + 
                b_tp_edu_rick*(trip_purp==2) + b_tp_job_rick*(trip_purp==4) + #b_tp_rtrn_rick *(trip_purp==7) + #b_tp_shop_rick*(trip_purp==8)+ 
                #b_gp_business_rick*(gen_purp==1) + 
                b_gp_edu_rick*(gen_purp==2) + #b_gp_medi_rick*(gen_purp==3) + 
                b_gp_job_rick*(gen_purp==4) + #b_gp_rec_rick*(gen_purp==6) +
                #b_TF3_rick*(trip_freq==3) + 
                b_TF4_rick*(trip_freq==4) + b_TF5_rick*(trip_freq==5) + #b_TF6_rick*(trip_freq==6) +
                #b_bag_rick*b1 + b_obag_rick*b2 + b_shpbag_rick*b4 + b_other_rick*b5 + #b_lugg_rick*b3
                b_comp_rick*comp + 
                #b_age1_rick * (age==20) + b_age2_rick * (age==30) + b_age3_rick * (age==40) + #b_age4_rick * (age==60)
                b_fmale_rick*(gen==1)+ 
                b_stud_rick*(occ==1) + b_gov_rick*(occ==3) + b_pvt_rick*(occ==4) + #b_rtd_rick*(occ==6) + #b_unemp_rick*(occ==7) +
                #b_inc3_rick*(month_inc==30) + 
                b_inc4_rick*(month_inc==60) + b_inc5_rick*(month_inc==80) + 
                b_own_bike_rick*veh_own_bike 
                #b_mrt_rick*mrt_pass
  
  V[["bus"]]  = asc_bus + (b_tt_bus + b_ac_tt_bus*feed) * bus_tt + (b_cost_bus + b_ac_cost_bus*feed) * bus_cost + 
                b_ac_bus*feed +
                b_11am_bus*(surv_t==2) + b_1pm_bus*(surv_t==3) + b_3pm_bus*(surv_t==4) + #b_5pm_bus*(surv_t==5) +
                #b_tp_business_bus*(trip_purp==1) + 
                b_tp_edu_bus*(trip_purp==2) + b_tp_medi_bus*(trip_purp==3) + b_tp_rtrn_bus*(trip_purp==7) + #b_tp_shop_bus*(trip_purp==8) +
                #b_gp_business_bus*(gen_purp==1)+ 
                b_gp_edu_bus*(gen_purp==2) + b_gp_job_bus*(gen_purp==4) + #b_gp_pw_bus*(gen_purp==5)+
                #b_TF1_bus*(trip_freq==1) + b_TF2_bus*(trip_freq==2) + b_TF3_bus*(trip_freq==3) +
                b_TF4_bus*(trip_freq==4) + b_TF5_bus*(trip_freq==5) + #b_TF6_bus*(trip_freq==6) +
                #b_bag_bus*b1 + b_obag_bus*b2 + b_shpbag_bus*b4 + b_other_bus*b5 + #b_lugg_bus*b3 
                #b_comp_bus*comp +  
                b_age2_bus*(age==30) + #b_age3_bus*(age==40) + #b_age4_bus*(age==60)
                #b_fmale_bus*(gen==1) +
                b_stud_bus*(occ==1) + #b_hwife_bus*(occ==2) + b_pvt_bus*(occ==4) + b_bzman_bus*(occ==5) + #b_rtd_bus*(occ==6)+ #b_unemp_bus*(occ==7) +
                b_inc2_bus*(month_inc==20) + #b_inc3_bus*(month_inc==30) + b_inc4_bus*(month_inc==60) + b_inc5_bus*(month_inc==80) +  
                b_mrt_bus*mrt_pass
              
 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(walk=1, rick=2, bus=7), 
    avail         = list(walk=walk_av, bus=bus_av, rick=rick_av), 
    choiceVar     = mode,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
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

#apollo_saveOutput(model)


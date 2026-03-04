
# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

setwd("F:\\prod\\4-2\\CE-400\\Analysis\\R-Model\\Mixed Logit Model\\Egress\\Survey 1+2")

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Mixed_MNL_EGRESS_Model_cost_rick_distributed_draws-800",
  modelDescr      = "Mixed MNL model for significant parameters on mode choice egress data - for bus, rick, walk",
  indivID         = "ID", 
  outputDirectory = "Significant_Mixed_Model_Estimates_Egress"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
database = read.csv("F:/prod/4-2/CE-400/Analysis/R-Model/Mixed Logit Model/Egress/Survey 1+2/ENCODED_EGRESS_s1+s2_sorted.csv",header=TRUE)

#Filtering only rick, walk, bus chosen data
database = subset(database, eg_mode %in% c(1, 2, 7))
database$eg_comp = ((database$ec1==1) | (database$ec2==1) | (database$ec3==1) | (database$ec4==1))
#database$age = ((database$age==20)*1 + (database$age==30)*2 + (database$age==40)*3 + (database$age==60)*4 + (database$age==70)*5)
database$month_inc = ((database$month_inc==5)*5 +(database$month_inc==20)*20 + (database$month_inc==30)*30 + (database$month_inc==60)*60 + ((database$month_inc==80) | (database$month_inc==150) | (database$month_inc==250))*80)
database$age = ((database$age == 20)*20 + (database$age == 30)*30 + (database$age == 40)*40 + ((database$age == 60) | (database$age == 70))*60)
#database = apollo_modeChoiceData
### for data dictionary, use ?apollo_modeChoiceData


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_walk")

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  
              asc_walk  = 0,
              asc_rick  = 0.94371235,
              asc_bus   = -0.81331117,
              
              b_tt_walk = -0.074436683,
              b_tt_bus  = -0.029060554,
              
              b_cost_rick = -0.051886884,
              #b_cost_bus = -0.096768055,
              
              #mu_b_cost_rick = -0.096768,
              #sigma_b_cost_rick = 0.01,
              
              mu_b_cost_bus = -0.096768,
              sigma_b_cost_bus = 0.01,
              
              b_1pm_bus  = -0.394551629,
              b_5pm_bus  = 0.562019858,
              
              b_tp_business_rick = -0.88895837,
              b_tp_edu_rick = -0.54259903,
              b_tp_job_rick = -1.016582857,
              
              b_tp_business_bus = -0.692253976,
              b_tp_medi_bus = -1.524468175,
              b_tp_rtrn_bus = 1.102408974,
              
              b_TF3_rick = -0.387155218,
              b_TF4_rick = -0.836908689,
              b_TF5_rick = -0.46820156,
              
              b_comp_rick = 0.277838298,
              b_comp_bus = -0.248069302,
              
              b_gp_medi_rick = -0.848274922,
              b_gp_job_rick = -1.457177409,
              b_gp_rec_rick = -0.310937244,
              b_gp_job_bus = -0.847621891,
              
              b_age1_bus = 1.459535632,
              b_age2_bus = 0.514747994,
              b_age3_bus = 1.310689692,
              
              b_fmale_rick = 0.351043532,
              b_fmale_bus = -0.224847191,
              
              b_gov_rick = 1.787956431,
              b_pvt_rick = 0.969333068,
              b_rtd_rick = -1.512859568,
              
              b_stud_bus = -1.539415815,
              b_hwife_bus = -0.834052295,
              b_pvt_bus = -0.623842305,
              b_bzman_bus = -0.458785303,
              b_rtd_bus = 1.114552941,
              
              b_inc3_rick = 0.477838481,
              b_inc4_rick = 1.255402781,
              b_inc5_rick = 1.528310532,
              
              b_inc2_bus = 0.958509057,
              
              b_own_bike_rick = -0.606369556,
              b_mrt_rick = -0.293373616,
              b_mrt_bus = -0.744482177
  
)

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 500,
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
  randcoeff[["b_cost_bus"]] = -exp( mu_b_cost_bus + sigma_b_cost_bus * draws_bus_cost)
  
  #randcoeff[["b_wlk_tt"]] = -exp( mu_lg_b_wlk_tt + sigma_lg_b_wlk_tt * draws_walk_tt )
  
  
  return(randcoeff)
}


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
  
  #V[["car"]]  = asc_car + b_tt_car * car_tt+ b_dist_car * car_dist + b_cost * car_cost
  
  V[["walk"]] = asc_walk + b_tt_walk * walk_tt                           #+ # + b_dist_walk * walk_dist +
                #b_tp_business_walk*(trip_purp==1) + 
                #b_tp_edu_walk*(trip_purp==2) + b_tp_medi_walk*(trip_purp==3) + b_tp_job_walk*(trip_purp==4) + b_tp_rec_walk*(trip_purp==6) + b_tp_rtrn_walk*(trip_purp==7) + #b_tp_shop_walk*(trip_purp==8) +
                #b_gp_business_walk*(gen_purp==1) + b_gp_edu_walk*(gen_purp==2) + b_gp_medi_walk*(gen_purp==3) + b_gp_job_walk*(gen_purp==4) + b_gp_rec_walk*(gen_purp==6) + b_gp_shop_walk*(gen_purp==8) +
                #b_TF1_walk*(trip_freq==1) + b_TF2_walk*(trip_freq==2) + b_TF3_walk*(trip_freq==3) + b_TF4_walk*(trip_freq==4) + b_TF5_walk*(trip_freq==5) + 
                #b_bag_walk*b1 + b_obag_walk*b2 + b_shpbag_walk*b4 + b_other_walk*b5 + #b_lugg_walk*b3
                #b_comp_walk*eg_comp + 
                #b_age1_walk * (age==20) + b_age2_walk * (age==30)+ b_age3_walk * (age==40) + #b_age4_walk * (age==60)
                #b_fmale_walk*(gen==1) +
                #b_stud_walk*(occ==1) + b_hwife_walk*(occ==2) + b_gov_walk*(occ==3) + b_pvt_walk*(occ==4) + b_bzman_walk*(occ==5) + b_unemp_walk*(occ==7) +
                #b_inc1_walk*(month_inc==5) + b_inc2_walk*(month_inc==20) + b_inc3_walk*(month_inc==30) + #+ b_inc4_walk*(month_inc==60) + #+ b_inc5_walk*(month_inc==80) + b_inc6_walk*(month_inc==150)
                #b_own_car_walk*veh_own_car + b_own_bike_walk*veh_own_bike + b_own_cyc_walk*veh_own_cyc + 
                #b_mrt_walk*mrt_pass
  
  V[["rick"]] = asc_rick + b_cost_rick * rick_cost +  #b_tt_rick * rick_tt #  b_dist_walk * walk_dist + b_dist_rick * rick_dist
                b_tp_business_rick*(trip_purp==1) + b_tp_edu_rick*(trip_purp==2) + b_tp_job_rick*(trip_purp==4) + #b_tp_shop_rick*(trip_purp==8)+ 
                b_gp_medi_rick*(gen_purp==3) + b_gp_job_rick*(gen_purp==4) + b_gp_rec_rick*(gen_purp==6) +
                b_TF3_rick*(trip_freq==3) + b_TF4_rick*(trip_freq==4) + b_TF5_rick*(trip_freq==5) + #b_TF6_rick*(trip_freq==6) +
                #b_bag_rick*b1 + b_obag_rick*b2 + b_shpbag_rick*b4 + b_other_rick*b5 + #b_lugg_rick*b3
                b_comp_rick*eg_comp + 
                #b_age1_rick * (age==20) + b_age2_rick * (age==30) + b_age3_rick * (age==40) + #b_age4_rick * (age==60)
                b_fmale_rick*(gen==1)+ 
                b_gov_rick*(occ==3) + b_pvt_rick*(occ==4) + b_rtd_rick*(occ==6) + #b_unemp_rick*(occ==7) +
                b_inc3_rick*(month_inc==30) + b_inc4_rick*(month_inc==60) + b_inc5_rick*(month_inc==80) + #b_inc6_walk*(month_inc==150)
                b_own_bike_rick*veh_own_bike +
                b_mrt_rick*mrt_pass
  
  V[["bus"]]  = asc_bus + b_tt_bus * bus_tt + b_cost_bus * bus_cost + # b_dist_bus * bus_dist + b_dist_bus * bus_dist+
                b_1pm_bus*(surv_t==3) + b_5pm_bus*(surv_t==5) +
                b_tp_business_bus*(trip_purp==1) + b_tp_medi_bus*(trip_purp==3) + b_tp_rtrn_bus*(trip_purp==7) + #b_tp_shop_bus*(trip_purp==8) +
                b_gp_job_bus*(gen_purp==4) + #b_gp_pw_bus*(gen_purp==5)+
                #b_TF1_bus*(trip_freq==1) + b_TF2_bus*(trip_freq==2) + b_TF3_bus*(trip_freq==3) + b_TF4_bus*(trip_freq==4) + b_TF5_bus*(trip_freq==5) + #b_TF6_bus*(trip_freq==6) +
                #b_bag_bus*b1 + b_obag_bus*b2 + b_shpbag_bus*b4 + b_other_bus*b5 + #b_lugg_bus*b3 
                b_comp_bus*eg_comp +  
                b_age1_bus*(age==20) + b_age2_bus*(age==30) + b_age3_bus*(age==40) + #b_age4_bus*(age==60)
                b_fmale_bus*(gen==1) +
                b_stud_bus*(occ==1) + b_hwife_bus*(occ==2) + b_pvt_bus*(occ==4) + b_bzman_bus*(occ==5) + b_rtd_bus*(occ==6)+ #b_unemp_bus*(occ==7) +
                b_inc2_bus*(month_inc==20) + #b_inc6_bus*(month_inc==150)
                b_mrt_bus*mrt_pass
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(walk=1, rick=2, bus=7), 
    avail         = list(walk=walk_av, bus=bus_av, rick=rick_av), 
    choiceVar     = eg_mode,
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


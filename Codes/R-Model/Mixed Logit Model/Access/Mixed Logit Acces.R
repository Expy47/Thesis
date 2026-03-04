# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #
setwd("F://prod//4-2//CE-400//Analysis//R-Model//ACCESS")
### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "mixed_MNL_access_bus_dist",
  modelDescr      = "Mixed MNL model on mode choice for access -  considering walk, bus and rick",
  indivID         = "ID", 
  nCores          = 4,
  outputDirectory = "output_Mixed_Logit_LoS_cost_distr"
)

## ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
database = read.csv("ENCODED_ACCESS1.csv",header=TRUE)

#Filtering only rick, walk, bus chosen data
database = subset(database, ac_mode %in% c(1, 2, 7))
database$ac_comp = ((database$ac1==1) | (database$ac2==1) | (database$ac3==1) | (database$ac4==1))
database$month_inc = ((database$month_inc==5)*5 +(database$month_inc==20)*20 + (database$month_inc==30)*30 + ((database$month_inc==60) | (database$month_inc==80) | (database$month_inc==150) | (database$month_inc==250))*60)
database$age = ((database$age == 20)*20 + (database$age == 30)*30 + ((database$age == 40)*40 | (database$age == 60) | (database$age == 70))*40)
database$gen = ((database$gen == 1)*0 + (database$gen == 0)*1)



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_walk")

apollo_beta=c(
  
  
  asc_walk  = 0,
  asc_rick  = 0,
  asc_bus   = 0,
  
  
  b_tt_walk = 0,
  b_tt_rick = 0,
  b_tt_bus  = 0,
  
  b_cost_rick = 0,
  #b_cost_bus = 0,
  
  #mu_b_tt_walk = -0.1,
  #sigma_b_tt_walk = 0.01,
  
  #mu_b_tt_rick = -0.1,
  #sigma_b_tt_rick = 0.01
  
  #mu_b_tt_bus = -0.1,
  #sigma_b_tt_bus = 0.01
  
  #mu_b_cost_rick = -0.1,
  #sigma_b_cost_rick = 0.01
  
  mu_b_cost_bus = -1.29723,
  sigma_b_cost_bus = -0.084
  
)
  
  
### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
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
  #randcoeff[["b_tt_bus"]] = -exp( mu_b_tt_bus + sigma_b_tt_bus * draws_bus_tt )

  #randcoeff[["b_cost_rick"]] = -exp( mu_b_cost_rick + sigma_b_cost_rick * draws_rick_cost)
  randcoeff[["b_cost_bus"]] = -exp( mu_b_cost_bus + sigma_b_cost_bus * draws_bus_cost)
  
  
  
  return(randcoeff)
}

# norm = rnorm(10000, mean = 0, sd = 1)
# hist(norm)
# b_bus_tc_est = -exp( -1.217927 + 0.002618 * norm )
# hist(b_bus_tc_est)
# summary(b_bus_tc_est)


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
  
  
  V[["walk"]] = asc_walk + b_tt_walk * walk_tt                           #+ # + b_dist_walk * walk_dist +
  #b_p1_walk*(trip_purp==1) + b_p2_walk*(trip_purp==2) + b_p3_walk*(trip_purp==3) + b_p4_walk*(trip_purp==4) + b_p6_walk*(trip_purp==6) + b_p7_walk*(trip_purp==7) + #b_p8_walk*(trip_purp==8) +
  #b_gp1_walk*(gen_purp==1) + b_gp2_walk*(gen_purp==2) + b_gp3_walk*(gen_purp==3) + b_gp4_walk*(gen_purp==4) + b_gp6_walk*(gen_purp==6) + b_gp8_walk*(gen_purp==8) +
  #b_TF1_walk*(trip_freq==1) + b_TF2_walk*(trip_freq==2) + b_TF3_walk*(trip_freq==3) + b_TF4_walk*(trip_freq==4) + b_TF5_walk*(trip_freq==5) + 
  #b_bag_walk*b1 + b_obag_walk*b2 + b_shpbag_walk*b4 + b_other_walk*b5 + #b_lugg_walk*b5
  #b_comp_walk*ac_comp + #b_ec1_walk*ec1 + b_ec2_walk*ec2 + b_ec3_walk*ec3 + b_ec4_walk*ec4
  #b_age1_walk * (age==20) + b_age2_walk * (age==30)+ b_age3_walk * (age==40) + #b_age4_walk * (age==60)
  #b_fmale_walk*(gen==0) +
  #b_occ1_walk*(occ==1) + b_occ2_walk*(occ==2) + b_occ3_walk*(occ==3) + b_occ4_walk*(occ==4) + b_occ5_walk*(occ==5) + b_occ6_walk*(occ==7) +
  #b_inc1_walk*(month_inc==5) + b_inc2_walk*(month_inc==20) + b_inc3_walk*(month_inc==30) + #+ b_inc4_walk*(month_inc==60) + #+ b_inc5_walk*(month_inc==80) + b_inc6_walk*(month_inc==150)
  #b_own_car_walk*veh_own_car + b_own_bike_walk*veh_own_bike + b_own_cyc_walk*veh_own_cyc + 
  #b_mrt_walk*mrt_pass
  
  V[["rick"]] = asc_rick + b_tt_rick * rick_tt + b_cost_rick * rick_cost  # + b_dist_walk * walk_dist#+ b_dist_rick * rick_dist
  #b_p1_rick*(trip_purp==1) + b_p2_rick*(trip_purp==2) + b_p3_rick*(trip_purp==3) + b_p4_rick*(trip_purp==4) + b_p6_rick*(trip_purp==6) + b_p7_rick*(trip_purp==7) + #b_p8_rick*(trip_purp==8) 
  #b_gp1_rick*(gen_purp==1) + b_gp2_rick*(gen_purp==2) + b_gp3_rick*(gen_purp==3) + b_gp4_rick*(gen_purp==4) + b_gp5_rick*(gen_purp==5) + b_gp6_rick*(gen_purp==6) + #b_gp8_rick*(gen_purp==8) +
  #b_TF3_rick*(trip_freq==3) + b_TF4_rick*(trip_freq==4) + b_TF5_rick*(trip_freq==5) + b_TF6_rick*(trip_freq==6) +
  #b_bag_rick*b1 + b_obag_rick*b2 + b_shpbag_rick*b4 + #+ b_other_rick*b5 #b_lugg_rick*b5
  #b_comp_rick*ac_comp + #b_ec1_rick*ec1 + b_ec2_rick*ec2 + b_ec3_rick*ec3 + b_ec4_rick*ec4
  #b_age1_rick * (age==20) + b_age2_rick * (age==30) + #+ b_age4_rick * (age==60)
  #b_fmale_rick*(gen==0) +
  #b_occ1_rick*(occ==1) + b_occ2_rick*(occ==2) + b_occ3_rick*(occ==3) + b_occ4_rick*(occ==4) + #+ b_occ7_rick*(occ==7) 
  #b_inc2_rick*(month_inc==20) + b_inc3_rick*(month_inc==30) + b_inc4_rick*(month_inc==60) + #+ b_inc5_rick*(month_inc==80) + b_inc6_walk*(month_inc==150)
  #b_own_car_rick*veh_own_car + b_own_bike_rick*veh_own_bike + b_own_cyc_rick*veh_own_cyc +
  #b_mrt_rick*mrt_pass
  
  V[["bus"]]  = asc_bus + b_tt_bus * bus_tt + b_cost_bus * bus_cost # + b_dist_bus * bus_dist + b_dist_bus * bus_dist
  #b_p1_bus*(trip_purp==1) + b_p2_bus*(trip_purp==2) + b_p3_bus*(trip_purp==3) + b_p4_bus*(trip_purp==4) + b_p6_bus*(trip_purp==6) + b_p7_bus*(trip_purp==7) + #b_p8_bus*(trip_purp==8) 
  #b_gp1_bus*(gen_purp==1) + b_gp2_bus*(gen_purp==2) + b_gp3_bus*(gen_purp==3) + b_gp4_bus*(gen_purp==4) + b_gp5_bus*(gen_purp==5) + b_gp6_bus*(gen_purp==6) + #b_gp8_bus*(gen_purp==8) +
  #b_TF3_bus*(trip_freq==3) + b_TF4_bus*(trip_freq==4) + b_TF5_bus*(trip_freq==5) + b_TF6_bus*(trip_freq==6) +
  #b_bag_bus*b1 + b_obag_bus*b2 + b_shpbag_bus*b4 + #+ b_other_bus*b5  #b_lugg_bus*b5
  #b_comp_bus*ac_comp +  #b_ec1_bus*ec1 + b_ec2_bus*ec2 + b_ec3_bus*ec3 + b_ec4_bus*ec4
  #b_age1_bus*(age==20) + b_age2_bus*(age==30) + #+ b_age4_bus*(age==60)
  #b_fmale_bus*(gen==0) +
  #b_occ1_bus*(occ==1) + b_occ2_bus*(occ==2) + b_occ3_bus*(occ==3) + b_occ4_bus*(occ==4) + #+ b_occ7_bus*(occ==7) 
  #b_inc2_bus*(month_inc==20) + b_inc3_bus*(month_inc==30) + b_inc4_bus*(month_inc==60) + #+ b_inc5_bus*(month_inc==80) + b_inc6_bus*(month_inc==150)
  #b_own_car_bus*veh_own_car + b_own_bike_bus*veh_own_bike + b_own_cyc_bus*veh_own_cyc +
  #b_mrt_bus*mrt_pass
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(walk=1, rick=2, bus=7), 
    avail         = list(walk=walk_av, rick=rick_av, bus=bus_av), 
    choiceVar     = ac_mode,
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

apollo_saveOutput(model)


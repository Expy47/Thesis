#setting working directory
setwd("G:\\4-2\\CE-400\\Analysis\\R-Model\\Model & Result (Junaid Nusrat)\\Descriptive Analysis")

### Clear memory
  rm(list = ls())

#read csv and putting in df table
  df = df = read.csv("G:/4-2/CE-400/Analysis/R-Model/Model & Result (Junaid Nusrat)/Descriptive Analysis/Access_Actual_vs_prefered.csv",header=TRUE)
  
  
#plotting for tt
  plot(df$tt, df$pref_tt, main = "Given tt vs Prefered tt\n EQN: y = 1.03 * x",
        xlab = "Actual Time", ylab = "Preffered Time", col = "red")
  
  # Fit a linear model
  model_tt = lm(tt ~ 0 + pref_tt, data = df)
  
  # Add the best-fit line
  abline(model_tt, col = "blue", lwd = 2)
  
  # Get the coefficients of the best-fit line
  coefficients <- coef(model_tt)
  slope <- coefficients[1]  # Intercept (b0)
  
  # Display the equation of the line
  cat("The equation of the best-fit line is: y =", round(slope, 2), "* x\n")
  
  
  
  #plotting for tc
  plot(df$tc, df$pref_tc, main = "Preferred tc vs Actual tc\n EQN: y = 0.91 * x",
       xlab = "Actual cost", ylab = "Preferred cost", col = "red")
  
  # Fit a linear model
  model_tc = lm(tc ~ 0 + pref_tc, data = df)
  
  # Add the best-fit line
  abline(model_tc, col = "blue", lwd = 2)
  
  # Get the coefficients of the best-fit line
  coefficients <- coef(model_tc)
  slope <- coefficients[1]      # Slope (b1)
  
  # Display the equation of the line
  cat("The equation of the best-fit line is: y =", round(slope, 2), "* x\n")
  
  
  #data table when choice mode is same as prefered mode
  db1 = subset(df,df$choice_mode==df$pref_mode)
  table(db1$choice_mode)*100/nrow(db1$choice_mode)
  
  
  #plotting for tt where given mode = pref mode
  plot(db1$tt, db1$pref_tt, main = "Prefered tt vs Actual tt\n EQN: y = 0.82 + 0.96 * x",
        xlab = "Actual Time", ylab = "Preffered Time", col = "red")
  
  # Fit a linear model
  model1_tt = lm(tt ~ 0 + pref_tt, data = db1)
  
  # Add the best-fit line
  abline(model_tt, col = "blue", lwd = 2)
  
  # Get the coefficients of the best-fit line
  coefficients <- coef(model1_tt)
  slope <- coefficients[1]      # Slope (b1)
  
  # Display the equation of the line
  cat("The equation of the best-fit line is: y =", round(slope, 2), "* x\n")
  
  
  #plotting for tc where given mode = actual mode
  plot(db1$tc, db1$pref_tc, main = "Given tc vs Actual tc\n EQN:  y = 0.52 + 0.99 * x ",
       xlab = "Actual cost", ylab = "Preffered cost", col = "red")
  
  # Fit a linear model
  model1_tc = lm(tc ~ 0+  pref_tc, data = db1)
  
  # Add the best-fit line
  abline(model1_tc, col = "blue", lwd = 2)
  
  # Get the coefficients of the best-fit line
  coefficients <- coef(model1_tc)
  slope <- coefficients[1]      # Slope (b1)
  
  # Display the equation of the line
  cat("The equation of the best-fit line is: y =", round(slope, 2), "* x\n")
  

  #database for given mode is not preferred mode
  db2 = subset(df,df$choice_mode!=df$pref_mode)
  table(db2$choice_mode)*100/nrow(db2$choice_mode)
  
  #plotting for tt when given mode is not preferred mode
  plot(db2$tt, db2$pref_tt, main = "Given tt vs Prefered tt\n EQN:  y = 5.22 + 0.83 * x",
       xlab = "Actual Time", ylab = "Preffered Time", col = "red")
  
  # Fit a linear model
  model2_tt = lm(tt ~ 0 + pref_tt, data = db2)
  
  # Add the best-fit line
  abline(model2_tt, col = "blue", lwd = 2)
  
  # Get the coefficients of the best-fit line
  coefficients <- coef(model2_tt)
  slope <- coefficients[1]      # Slope (b1)
  
  # Display the equation of the line
  cat("The equation of the best-fit line is: y =", round(slope, 2), "* x\n")
  
  
  
  #plotting for tc when given mode is not preffered mode
  plot(db2$tc, db2$pref_tc, main = "Given tc vs Actual tc\n EQN: y = -0.71 + 0.51 * x",
       xlab = "Actual cost", ylab = "Preffered cost", col = "red")
  
  # Fit a linear model
  model2_tc = lm(tc ~ 0 + pref_tc, data = db2)
  
  # Add the best-fit line
  abline(model2_tc, col = "blue", lwd = 2)
  
  # Get the coefficients of the best-fit line
  coefficients <- coef(model2_tc)
  slope <- coefficients[1]      # Slope (b1)
  
  # Display the equation of the line
  cat("The equation of the best-fit line is: y =", round(slope, 2), "* x\n")
  
  
  
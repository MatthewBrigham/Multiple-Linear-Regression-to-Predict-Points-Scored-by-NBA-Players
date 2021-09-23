# Matthew Brigham
# STA 567
# Final Project Code
# November 2020

# this was the final submission, dated Nov 30 2020

# refer to "filter 15ppg since 1997" for how data set was created

# The results in this code does not match previous calculations or plots in report because 
# it removed duplicate players starting from the 1997 season and moving up.
# Originally, the model was built on complete 2016-2017 season, then tested 
# on only the new players of 2017-2018 and 2018-2019 seasons.  There were 451
# observations in 2016-2017 season (excluding outliers), but there are only 77 
# using this dataset. A last second change of datasets used resulted in this error.
# The final report was modified in the data description for this dataset below, 
# however the plots and figures are based on 2016-2017 data. Use s1617 data, 
# then clean to see results used in paper.  Try using 111 Final Data Set.csv or
# Final Data Set 1.csv to see seasons 2016-2019 (you must change code in this 
# file to select the right seasons, 1 2 or 3)

library(MASS)
library(car)

# Import Data

        d0 = read.csv("1997-2019 Complete Data Set.csv", header = T) #read.csv("STA 567 Final Data Set.csv", header = T) #read.csv("STA 567 Final Data Set.csv", header = T)
        
        d= d0[which(d0$Season == 20), ] # select season 2016-2017
        

# Exploratory data analysis
        
        # Pairwise Scatterplots of Transformed and Untransformed Predictors
        
        x1 = d[c("PTS", "FT.", "FG.", "MP", "FGA", "FTA", "eFG."  , "ORB"  ,  
                 "DRB" , "TRB" , "TOV", "PF")]
        pairs(x1)
        
        x4 =  d[c("logPTS", "logFTpct", "logFGpct", "logMP", "logFGA", "logFTA", 
                  "logEFG", "logORB",  "logDRB", "logTRB", "logTOV", "logPF")]
        pairs(x4)

        # Remove outliers
        d = d[-(which(d$Rk == 408)), ]
        d = d[-(which(d$Rk == 248)), ]
        d = d[-(which(d$Rk == 122)), ]

        # Remove Players who scored less than 8 points per game
        train = d
        dptsfil_train = train[-which(train$PTS < 8), ]   

        # Individual scatter plots of response~predictor (Untransformed)
        par(mfrow=c(1,4))
        plot(dptsfil_train$PTS ~ dptsfil_train$FGA, main = 
               "Points Per Game vs Field Goal Attempted", xlab = "Field Goal Attempts (FGA)", 
             ylab = "Points Per Game (PTS)")
        
        plot(dptsfil_train$PTS ~ dptsfil_train$FTA, main = 
               "Points Per Game vs Free Throws Attempted", xlab = "Free Throws Attempted (FTA)", 
             ylab = "Points Per Game (PTS)")
        
        plot(dptsfil_train$PTS ~ dptsfil_train$eFG., main = 
               "Points Per Game vs Effective Field Goal %", xlab = "Effective Field Goal Percentage (EFGpct)", 
             ylab = "Points Per Game (PTS)")
        
        plot(dptsfil_train$PTS ~ dptsfil_train$FT., main = 
               "Points Per Game vs Free Throws %", xlab = "Free Throw Percentage (FTpct)", 
             ylab = "Points Per Game (PTS)")

        # Individual scatter plots of response~predictor (Transformed)
        plot(dptsfil_train$logPTS ~ dptsfil_train$logFGA, main = "log(PTS) vs log(FGA)", 
             xlab = "log(FGA)", ylab = "log(PTS)")
        plot(dptsfil_train$logPTS ~ dptsfil_train$logFTA, main = "log(PTS) vs log(FTA)", 
             xlab = "log(FTA)", ylab = "log(PTS)")
        plot(dptsfil_train$logPTS ~ dptsfil_train$logEFG, main = "log(PTS) vs log(EFG)", 
             xlab = "log(EFG)", ylab = "log(PTS)")
        plot(dptsfil_train$logPTS ~ (dptsfil_train$logFTpct), main = "log(PTS) vs log(FT%)", 
             xlab = "log(FT%)", ylab = "log(PTS)")

        # Density plots (look for normality, transform to correct normality)
        par(mfrow=c(1,2))
        plot(density(dptsfil_train$PTS), xlab = "Points per Game (PPG)", 
             ylab = "Density", main = "Density Plot of PTS Season 2016-2017")
        plot(density(dptsfil_train$logPTS), xlab = "log(PPG)", ylab = "Density", 
             main = "Density Plot of log(PTS)")
        
#################################################################################
#################################################################################
#Create a Model of Log-Transformed Variables With Interaction on 2016-2017 Season
#################################################################################
#################################################################################

# Variable Selection

        log_full_lm = lm(logPTS ~ logFGA+ logFGpct + logFTA*logFTpct + logTRB + logMP + logEFG + logTOV + logPF, data = dptsfil_train)
        summary(log_full_lm)
        
        #removed logMP
        lm_step1 = lm(logPTS ~ logFGA + logFGpct + logFTA*logFTpct + logTRB + logEFG + logTOV + logPF, data = dptsfil_train)
        summary(lm_step1)
        
        #removed logTRB
        lm_step2 = lm(logPTS ~ logFGA + logFGpct + logFTA*logFTpct + logEFG + logTOV + logPF, data = dptsfil_train)
        summary(lm_step2)
        
        #removed logFGpct
        lm_step3 = lm(logPTS ~ logFGA + logFTA*logFTpct + logEFG + logPF + logTOV, data = dptsfil_train)
        summary(lm_step3)
        
        #removed logTOV
        lm_step4 = lm(logPTS ~ logFGA + logFTA*logFTpct + logEFG + logPF, data = dptsfil_train)
        summary(lm_step4)
        
        #removed logPF
        lm_step5 = lm(logPTS ~ logFGA + logFTA*logFTpct + logEFG, data = dptsfil_train)
        summary(lm_step5)
        
# Assess Model
        
        # Adjusted R-Squared
        summary(lm_step5)$r.squared
        summary(lm_step5)$adj.r.squared
        
        # Examine residual plots of reduced model   
        par(mfrow=c(1,4))
        plot(lm_step5, which = 1)
        plot(lm_step5, which = 2)
        plot(lm_step5, which = 3)
        plot(lm_step5, which = 4)
        
        # ANOVA - F test of overall significance of regression 
        reduced = lm_step5
        full = log_full_lm
        anova(reduced, full)  # p value is 0.1178 
        
        # Test for Mulitcollinearity
        vif(lm_step5)
        
        # Mean Square Error
        SSE_red = anova(reduced)["Residuals", "Sum Sq"]
        MSE = SSE_red/(nrow(dptsfil_train) - 8 - 1)
        MSE # 0.0002302268
        
        # Mean Absolute Error
        mean(abs(resid(lm_step5))) # 0.01117734
        
        # min/max error/residual
        min(resid(lm_step5)) # -0.01675371
        max(resid(lm_step5)) # 0.06299008
        
        # Calculate Cooks Value, Studentized Resid, and Leverage
        dptsfil_train$cooks = with(dptsfil_train, cooks.distance(lm_step5))
        dptsfil_train$hat = with(dptsfil_train, hatvalues(lm_step5))
        dptsfil_train$studres = with(dptsfil_train, studres(lm_step5))
        
        # Count Influential observations
        nrow(dptsfil_train[which(dptsfil_train$cooks>=0.9), ]) #0 potential influential data point
        nrow(dptsfil_train[which(dptsfil_train$studres >= 2), ]) #13 potential influential data points
        nrow(dptsfil_train[which(dptsfil_train$hat > 2*length(lm_step5$coefficients)/nrow(dptsfil_train)), ]) #15 potential influential data points
        
        # Identify Influential OObservations
        dptsfil_train[which(dptsfil_train$cooks>=0.9), ] #potentially influential Cooks distances
        dptsfil_train[which(dptsfil_train$studres >= 2), ] #potentially influential studentized residuals 
        dptsfil_train[which(dptsfil_train$hat > 2*length(lm_step5$coefficients)/nrow(dptsfil_train)), ] #potentially influential leverages 
        
        
# Assessment of Fit on Training Data
        
        # log_pred = predict(lm_step5, newdata = dptsfil_test, se.fit = TRUE)
        # pred_Pts = exp(log_pred$fit)
        # 
        # # Calculate Prediction Error
        # error = 100*(pred_Pts - dptsfil_test$PTS)/dptsfil_test$PTS
        # par(mfrow=c(2,1))
        # plot(error ~ dptsfil_test$Rk, xlab = "observation #", ylab = "error %", 
        #      main = "Percentage Prediction Error by Player") # shows skewed to under predict PPG
        # plot(abs(error) ~ dptsfil_test$Rk, xlab = "observation #", ylab = "error %", 
        #      main = "Percentage Prediction Error by Player") # absolute value of error
        # abline(mean(abs(error)), 0) # average of 0.9059528% error with several extreme values

#########################################################
#########################################################
### Test model on 2017-2018 Season
#########################################################
#########################################################

# Import Data Set        
        
        s1718 = d0[which(d0$Season == 21), ] # select season 2017-2018
        
        test1 = s1718
        
# Remove players who scored less than 8 points per game
        
        dptsfil_test1 = test1[-which(test1$PTS < 8), ]   
        
#Get Size and Density Plot of Prediction Set
        
        nrow(dptsfil_test1)
        plot(density(dptsfil_test1$PTS), xlab = "Points per Game (PPG)", 
             ylab = "Density", main = "Density Plot of PTS Season 2017-18")
        
# Prediction
        
        log_pred = predict(lm_step5, newdata = dptsfil_test1, se.fit = TRUE)
        pred_Pts = exp(log_pred$fit)
        
        # Calculate prediction error
        error2 = 100*(pred_Pts - dptsfil_test1$PTS)/dptsfil_test1$PTS
        error2 = error2[!is.na(error2)]
        par(mfrow=c(1,1))
        plot(abs(error2) ~ dptsfil_test1$Rk, xlab = "observation #", ylab = "error %", 
             main = "Percentage Prediction Error by Player 2017-2018")
        abline(mean(abs(error2)), 0) # average of 1.387193% error with several extreme values
        mean(abs(error2))
        mean(abs(pred_Pts - dptsfil_test1$PTS)) #Mean absolute error of prediction
        
        plot(error2 ~ dptsfil_test1$Rk, xlab = "observation #", ylab = "error %", 
             main = "Percentage Prediction Error by Player 2017-2018") #shows that predictions are skewed to underpredict ppg
        
# Get Number of predictions that are less than 3% error
        
        numobsless3pct = length(which(abs(error2)<3)) #number of predictions 
        numobsless3pct
        numobsmore3pct = length(which(abs(error2)>=3))
        numobsmore3pct

#########################################################
#########################################################
### Test model on 2018-2019 Season
#########################################################
#########################################################

# Import Data Set   
        
        s1819 = d0[which(d0$Season == 22), ] # select season 2018-2019
    
        test2 = s1819

# Remove players who scored less than 8 points per game
        
        dptsfil_test2 = test2[-which(test2$PTS < 8), ]   
        
# Prediction
        
        log_pred = predict(lm_step5, newdata = dptsfil_test2, se.fit = TRUE)
        pred_Pts = exp(log_pred$fit)
        
        # Calculate prediction error
        error3 = 100*(pred_Pts - dptsfil_test2$PTS)/dptsfil_test2$PTS
        error3 = error3[!is.na(error3)]
        par(mfrow=c(1,1))
        plot(abs(error3) ~ dptsfil_test2$Rk, xlab = "observation #", ylab = "error %", 
             main = "Percentage Prediction Error by Player")
        abline(mean(abs(error3)), 0) # average of 1.387193% error with several extreme values
        mean(abs(error3))
        mean(abs(pred_Pts - dptsfil_test2$PTS)) #Mean absolute error of prediction
        
        plot(error3 ~ dptsfil_test2$Rk, xlab = "observation #", ylab = "error %", 
             main = "Percentage Prediction Error by Player 2018-2019") #shows that predictions are skewed to underpredict ppg
    
# Get Number of predictions that are less than 3% error 
        
        numobsless3pct = length(which(abs(error3)<3))
        numobsless3pct
        numobsmore3pct = length(which(abs(error3)>=3))
        numobsmore3pct
        
        nrow(dptsfil_test2)
        
        test2[which(abs(error3) > 5),] #identify predictions with error greater than 5%
        
        plot(density(dptsfil_test2$PTS), xlab = "Points per Game (PPG)", 
             ylab = "Density", main = "Density Plot of PTS Season 2017-18")
        
        nrow(dptsfil_test2) #number of players being tested from 2018-2019 season
        
        
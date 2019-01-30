# Causal Inference Assignment - by Jose A. Alvarez Cabrera






# In line 74 starts the code for replicating the figure 8 from King's paper.
# In line 194 starts the comparison of tmt effect estimates code.







# Load necessary libraries and set seed.
library(Matching)
set.seed(08121997)

# *****INSTRUCTIONS*****
#   
#   (1) Debugging--in the 3 cases below (a through c), identify the major coding error in each case and explain how to fix it, in 1-2 
# sentences. DO NOT actually copy/paste corrected code:
#   
#       (a) https://gist.github.com/diamonaj/2e5d5ba5226b7b9760f5d1bf1e7bf765


# The code inputs a GenMatch object (mainly a set of weights) into the match.out parameter (which should be mainly a match dataset) of MatchBalance().


# 
#       (b) https://gist.github.com/diamonaj/3b6bc83d040098486634184d99fc4c55
# 

# The code runs GenMatch with estimand="ATE" and does not run Match() with the estimand parameter specified. As the default is "ATT", the properties of
# the matching originally (and "internally") induced by GenMatch() will not translate to the output of Match().


#       (c) https://gist.github.com/diamonaj/a88cb40132ed8584e5182b585e1c84c8


# The code runs Match() with M=2 (number of matched (control/treated) untis per (control/treated) unit), but does not initially run GenMatch() in the same way.
# In general, GenMatch() should be run with the same specifications as Match() if one desires the matching internally induced by GenMatch() to translate to the output of Match()
# as well.


 
# Questions 2-4 below require the peacekeeping data set that we worked on in class, as well as this codebook:
#   http://www.nyu.edu/gsas/dept/politics/faculty/cohen/codebook.pdf.
# 
# The class breakout instructions (including data download code) are here:
#   https://gist.github.com/diamonaj/3795bfc2e6349d00aa0ccfe14102858d
# 


# Import peace keeping dataset

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
# extract relevant columns
foo <- foo[, c(6:8, 11:16, 34:35, 99, 50, 114, 49, 55, 63, 136, 109, 126, 48, 160, 142, 10)]
# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ] # these are controls
# one of the pbs5l is NA... let's make it be the same value as pbs2l
foo[which(is.na(foo$pbs5l)), ]$pbs5l = foo[which(is.na(foo$pbs5l)), ]$pbs2l 

# check that all missing data is gone...
which(is.na(foo) == TRUE)
# take a peek at the data set (identify the columns)
head(foo)


# (2) Replicate figure 8 in https://gking.harvard.edu/files/counterf.pdf.
# 
#         A few suggestions:
#               a. Read the class breakout instructions above to get the data and relevant columns,
#               b. If you are not clear on the model, read the relevant sections of the paper and focus on understanding Table 2;
#               c. To plot the figure, you should use a strategy similar to the one we used in the statistics scavenger hunt, which was also used
#                  in a previous assignment (e.g., holding predictors at their medians and looping through values of one variable to obtain treatment
#                  effects at different levels of the variable--you may want to review the answer key for that previous assignment, but please note
#                  that you WON'T have to simulate coefficients this time because there is no need to estimate uncertainty e.g., intervals).  
#                  However, you don't need to simulate coefficients this time.

# logistic regression models for pbs2s3...
# ORIGINAL
pbs2s3.logit.original = glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap +
                     + develop + exp + decade + treaty + untype4, data = foo, family = binomial)
# MODIFIED (with an interaction term)
pbs2s3.logit.modified = glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap +
                     + develop + exp + decade + treaty + untype4 + I(wardur*untype4), data = foo, family = binomial)


# We need to hold the variables at their median, and vary wardur
wartype.mean = mean(foo$wartype) # recall this is wartype, not wardur. 
logcost.mean = mean(foo$logcost)
factnum.mean = mean(foo$factnum)
factnum2.mean = mean(foo$factnum2)
trnsfcap.mean = mean(foo$trnsfcap)
develop.mean = mean(foo$develop)
exp.mean = mean(foo$exp)
decade.mean = mean(foo$decade)
treaty.mean = mean(foo$treaty) # weird value... but works
# untype4.mean = mean(foo$untype4) # you don't use this one at its median!


# Function for plotting Marginal Effects of ONU's peacekeeping efforts.
# logit model shall be either pbs2s3.logit.original or pbs2s3.logit.modified
plot.marginal.effect = function(log.model, color = "blue", plot.points = FALSE,
                                xlab="", ylab="") {
  
  # Array to hold the marginal effects
  meffects = c()
  
  # For loop that goes from i = 1 to 315, where i stands for a possible value of wardur
  for (i in 1:315) {
    
    # variable that varies
    war_duration_i = i
    
    ### *** For TREATED ***
    
    # accompanying covariates
    X_i = list(wartype.mean, logcost.mean, war_duration_i, factnum.mean, 
               factnum2.mean, trnsfcap.mean, develop.mean, exp.mean, 
               decade.mean, treaty.mean, 1) # Note that we set untype4 here to 1
    names(X_i) = c("wartype", "logcost", "wardur", "factnum", "factnum2", "trnsfcap",
                   "develop", "exp", "decade", "treaty", "untype4")
    
    
    X_i = rbind(data.frame(c()), X_i)
    
    # linear predictor from logit model
    linear_predictor = predict(log.model, X_i)
    
    # treated probability of success
    Y_t = exp(linear_predictor) / (1 + exp(linear_predictor))
    
    
    ### **** For CONTROLS ***
    
    # accompanying covariates
    X_i = list(wartype.mean, logcost.mean, war_duration_i, factnum.mean, 
               factnum2.mean, trnsfcap.mean, develop.mean, exp.mean, 
               decade.mean, treaty.mean, 0) # Note that we set untype4 here to 0
    names(X_i) = c("wartype", "logcost", "wardur", "factnum", "factnum2", "trnsfcap",
                   "develop", "exp", "decade", "treaty", "untype4")
    
    
    X_i = rbind(data.frame(c()), X_i) # just for the sake of having it as a horizontal daraframe...
    # there must be a better way to do this...
    
    # linear predictor from logit model
    linear_predictor = predict(log.model, X_i)
    
    # treated probability of success
    Y_c = exp(linear_predictor) / (1 + exp(linear_predictor))
    
    
    # MARGINAL EFFECT is the difference between Y_t and Y_c
    Y = Y_t - Y_c
    
    # Save the marginal effect for a given value of wardur (i)
    meffects = append(meffects, Y)
  }
  
  # After the for loop, we'll have a vector meffects of marginal effects
  # Plot it as the Y's of values of wardur that go from 1:315
  if (plot.points == TRUE) {
    points(1:315, meffects, col = color)
    # ?points
  } else {
    plot(1:315, meffects, col = color, xlab = xlab, ylab=ylab)
  }
    
}

# Plot estimation of marginal effects based on two slightly different models!!! (I.g., REPLICATE FIGURE 8)
plot.marginal.effect(pbs2s3.logit.modified, color = "red", xlab="Duration of war (years)", ylab="Marginal Effect  (Prob. of success)")
plot.marginal.effect(pbs2s3.logit.original, color = "blue", plot.points = TRUE)
title("Marginal Effect of ONU's efforts on War")
legend("bottomright", c("Original model", "Model with interaction term"), col=c("blue","red"),
       text.col="black",lty=c(1,1),lwd=c(3,3),bg="gray95")





#                           
#                           
# (3) Define treatment as below:
#                           Tr <- rep(0, length(foo$untype))
#                           Tr[which(foo$untype != "None")] <- 1
# foo2$untype
treatment2 <- rep(0, length(foo$untype))
treatment2[which(foo$untype != "None")] <- 1

# Let's append Tr to foo
foo = cbind(foo, treatment2)


#  What does this mean? What is "treatment"? 
#                             
#                             
# (4) Let's pretend you work for an NGO and your manager asks you to estimate the impact of the treatment identified above on lenient
#     peacebuilding success 2 years and 5 years after the war. You will have to search for these two outcomes variables in the codebook.
#                           
#               (a) In no more than 1 sentence, articulate the causal question as best you can (being as clear as you can about treatment and control):




#                           
#               (b) In no more than 1 sentence, explain how/why SUTVA might be violated here. In no more than 1 additional sentence, explain how you 
#                   could in theory use the "restrict" argument (in Match()/GenMatch()) to help address this potential problem.



#                           
#               (c) Use simple logistic regression, propensity score matching, and genetic matching to try to answer these questions. 


# I gotta make this adjustment somehow... it's better to have "success" as 1 and "failure" as 0
# Let's make "Success" be a (numeric) 1
# And "Failure" a (numeric) 0
foo$pbs2l = as.character(foo$pbs2l)
foo$pbs2l[foo$pbs2l == "Success"] = 1
foo$pbs2l[foo$pbs2l == "Failure"] = 0
foo$pbs2l = as.numeric(foo$pbs2l)

foo$pbs5l = as.character(foo$pbs5l)
foo$pbs5l[foo$pbs5l == "Success"] = 1
foo$pbs5l[foo$pbs5l == "Failure"] = 0
foo$pbs5l = as.numeric(foo$pbs5l)


#### Estimation of Tmt effects through logistic regression


#foo2$pbs2l
# Logistic Regression Model for pbs2l
pbs2l.logit.fit = glm(pbs2l ~ wartype + logcost + wardur + factnum + factnum2 + 
                        trnsfcap + develop + exp + decade + treaty + treatment2,
                      data = foo, family = binomial)
# summary(pbs2l.logit.fit)


#foo2$pbs5l
# Logistic Regression Model for pbs5l
pbs5l.logit.fit = glm(pbs5l ~ wartype + logcost + wardur + factnum + factnum2 + 
                        trnsfcap + develop + exp + decade + treaty + treatment2,
                      data = foo, family = binomial)

# summary(pbs5l.logit.fit)


# This function calculates the treatment effect based on either of the regression models.
# model can be either pbs2l or pbs5l
effect.from.model = function(model) {
  
  Y_t_vector = c()
  Y_c_vector = c()
  
  for (i in 1:nrow(foo)){
    
    
    # SIMULATE FOR TREATED
    # prepare values
    X_i = list(foo[i,]$wartype, foo[i,]$logcost,foo[i,]$wardur,foo[i,]$factnum, 
               foo[i,]$factnum2,foo[i,]$trnsfcap,foo[i,]$develop, foo[i,]$exp, 
               foo[i,]$decade,foo[i,]$treaty, 1)
    names(X_i) = c("wartype", "logcost", "wardur", "factnum", "factnum2", "trnsfcap",
                   "develop", "exp", "decade", "treaty", "treatment2")
    X_i = rbind(data.frame(c()), X_i)
    
    # predict based on model
    # linear predictor from logit model
    linear_predictor = predict(model, X_i)
    # treated probability of success
    Y_t = exp(linear_predictor) / (1 + exp(linear_predictor))
    
    
    
    # SIMULATE FOR CONTROL UNIT
    # prepare values
    X_i = list(foo[i,]$wartype, foo[i,]$logcost,foo[i,]$wardur,foo[i,]$factnum, 
               foo[i,]$factnum2,foo[i,]$trnsfcap,foo[i,]$develop, foo[i,]$exp, 
               foo[i,]$decade,foo[i,]$treaty, 0)
    names(X_i) = c("wartype", "logcost", "wardur", "factnum", "factnum2", "trnsfcap",
                   "develop", "exp", "decade", "treaty", "treatment2")
    X_i = rbind(data.frame(c()), X_i)
    
    # predict based on model
    # linear predictor from logit model
    linear_predictor = predict(model, X_i)
    # treated probability of success
    Y_c = exp(linear_predictor) / (1 + exp(linear_predictor))
    
    
    Y_t_vector = append(Y_t_vector, Y_t)
    Y_c_vector = append(Y_c_vector, Y_c)
    
  }
  
  # Determine the AVG treatment effect by subtracting hte avg of the control from the avg of the treated.
  avg_treatment_effect = mean(Y_t_vector) - mean(Y_c_vector)
  
  return (avg_treatment_effect)
}

# Use the function to get the treatment effects based on each model...

# Treatment effects calculated through logistic regression.
pbs2l.logit.effect = effect.from.model(pbs2l.logit.fit)
pbs5l.logit.effect = effect.from.model(pbs5l.logit.fit)




#### Estimation of Tmt effects with MATCHING


# PROPENSITY SCORE MATCHING
prop.scr.model = glm(treatment2 ~ wartype + logcost + I(logcost^2) + I(logcost^3) + 
                       wardur + I(wardur ^ 2) + I(wardur ^ 3) 
                     + factnum + factnum2 + trnsfcap + develop + exp + decade + treaty,
                     data = foo, family=binomial)

# For pbs2l
prp.scr.mout.pbs2l = Match(Y=foo$pbs2l, Tr=foo$treatment2, X=prop.scr.model$fitted.values,
                           BiasAdjust = TRUE)
summary(prp.scr.mout.pbs2l) 
# ESTIMANDS
prp.scr.mout.pbs2l$est # BIAS ADJ
prp.scr.mout.pbs2l$est.noadj # NO BIAS ADJ
# MEASURE BALANCE
MatchBalance(treatment2 ~ wartype + logcost + wardur + factnum + factnum2 + 
               trnsfcap + develop + exp + decade + treaty,
             data = foo,
             match.out=prp.scr.mout.pbs2l, nboots=1000)


# foo$pbs5l[which(is.na(foo$pbs5l))]  # it doesn't like NA's... minor adjustment.
# foo$pbs5l[4] # solved in a better way while importing data
# For pbs5l
prp.scr.mout.pbs5l = Match(Y=foo$pbs5l, Tr=foo$treatment2, X=prop.scr.model$fitted.values,
                           BiasAdjust = TRUE)
summary(prp.scr.mout.pbs5l)
# ESTIMANDS
prp.scr.mout.pbs5l$est # WITH BIAS ADJ
prp.scr.mout.pbs5l$est.noadj # WITHOUG BIAS ADJ
# MEASURE BALANCE
MatchBalance(treatment2 ~ wartype + logcost + wardur + factnum + factnum2 + 
               trnsfcap + develop + exp + decade + treaty,
             data = foo,
             match.out=prp.scr.mout.pbs5l, nboots=1000)

# which(is.na(foo$pbs2l))
#?Match







# GENETIC MATCHING
X_gm = as.matrix(foo[,c("wartype", "logcost", "wardur", "factnum", "factnum2", "trnsfcap",
              "develop", "exp", "decade", "treaty")])

BalanceMat_gm = as.matrix(cbind(
                        # same covariates as X_gm
                        foo$wartype,
                        foo$logcost,
                        foo$wardur,
                        foo$factnum,
                        foo$factnum2,
                        foo$trnsfcap,
                        foo$develop,
                        foo$exp,
                        foo$decade,
                        foo$treaty,
                        
                        # I actually ended up achieving better balance without the first order interaction terms...
                        # Plus their first order interaction terms
                        # I(foo$wartype * foo$logcost),
                        # I(foo$wartype * foo$wardur),
                        # I(foo$wartype * foo$factnum),
                        # I(foo$wartype * foo$factnum2),
                        # I(foo$wartype * foo$trnsfcap),
                        # I(foo$wartype * foo$develop),
                        # I(foo$wartype * foo$exp),
                        # I(foo$wartype * foo$decade),
                        # I(foo$wartype * foo$treaty),
                        # 
                        # I(foo$logcost * foo$wardur),
                        # I(foo$logcost * foo$factnum),
                        # I(foo$logcost * foo$factnum2),
                        # I(foo$logcost * foo$trnsfcap),
                        # I(foo$logcost * foo$develop),
                        # I(foo$logcost * foo$exp),
                        # I(foo$logcost * foo$decade),
                        # I(foo$logcost * foo$treaty),
                        # 
                        # I(foo$wardur * foo$factnum),
                        # I(foo$wardur * foo$factnum2),
                        # I(foo$wardur * foo$trnsfcap),
                        # I(foo$wardur * foo$develop),
                        # I(foo$wardur * foo$exp),
                        # I(foo$wardur * foo$decade),
                        # I(foo$wardur * foo$treaty),
                        # 
                        # I(foo$factnum * foo$factnum2),
                        # I(foo$factnum * foo$trnsfcap),
                        # I(foo$factnum * foo$develop),
                        # I(foo$factnum * foo$exp),
                        # I(foo$factnum * foo$decade),
                        # I(foo$factnum * foo$treaty),
                        # 
                        # I(foo$factnum2 * foo$trnsfcap),
                        # I(foo$factnum2 * foo$develop),
                        # I(foo$factnum2 * foo$exp),
                        # I(foo$factnum2 * foo$decade),
                        # I(foo$factnum2 * foo$treaty),
                        # 
                        # I(foo$trnsfcap * foo$develop),
                        # I(foo$trnsfcap * foo$exp),
                        # I(foo$trnsfcap * foo$decade),
                        # I(foo$trnsfcap * foo$treaty),
                        # 
                        # I(foo$develop * foo$exp),
                        # I(foo$develop * foo$decade),
                        # I(foo$develop * foo$treaty),
                        # 
                        # I(foo$exp * foo$decade),
                        # I(foo$exp * foo$treaty),
                        # 
                        # I(foo$decade * foo$treaty),
                        # 
                        # and quadratic terms
                        #I(foo$wartype ^ 2),
                        I(foo$logcost ^ 2), # better with just this
                        I(foo$wardur ^ 2),
                        # I(foo$factnum ^ 2),
                        # I(foo$factnum2 ^ 2),
                        # I(foo$trnsfcap ^ 2),
                        # I(foo$develop ^ 2),
                        # I(foo$exp ^ 2),
                        # I(foo$decade ^ 2),
                        # I(foo$treaty ^ 2),
                        # 
                        # and some cubic terms
                        I(foo$wardur ^ 3), # better with just this (intuition allows to know that wardur and logcost are particularly important continuous variables...)
                        I(foo$logcost ^ 3)

                        
)
  
)


# Add propensity score...
# Rather let's used the one previously defined, which is better and obviously more consistent!
# Wrong... we achieve better balance with a simpler version of the prp score model in GenMatch
# which is counter intuitive...
model.prp.scr = foo$treatment2 ~ foo$wartype + foo$logcost + foo$wardur +
                foo$factnum + foo$factnum2 + foo$trnsfcap + foo$develop +
                foo$exp + foo$decade + foo$treaty
p.scores = glm(model.prp.scr, family = binomial)

X_gm.plus.p.scores = cbind(p.scores$fitted.values, X_gm)
#head(X_gm.plus.p.scores)



# X_gm[foo$untype4 == 0,]

genout = GenMatch(Tr=foo$treatment2, 
                  X = X_gm.plus.p.scores, BalanceMatrix = BalanceMat_gm,
                  pop.size = 1000, 
                  max.generations = 100, 
                  wait.generations = 35,
                  M=1)

# for testing how it would have been with just X_gm. 
# genout = GenMatch(Tr=foo$treatment2, 
#                   X = X_gm,
#                   pop.size = 200, 
#                   max.generations = 100, 
#                   wait.generations = 100)




# For pbs2l
genm.mout.pbs2l = Match(Y = foo$pbs2l, Tr = foo$treatment2, X =  X_gm.plus.p.scores,
                        Weight.matrix = genout,
                        BiasAdjust = TRUE,
                        M=1)

# for testing how it would have been with just X_gm. 
# genm.mout.pbs2l = Match(Y = foo$pbs2l, Tr = foo$treatment2, X = X_gm,
#                         Weight.matrix = genout,
#                         BiasAdjust = TRUE)
# Apparently, balance can improve from 0.089 to 0.1792...


summary(genm.mout.pbs2l)
# ESTIMANDS
genm.mout.pbs2l$est # WITH BIAS ADJ
genm.mout.pbs2l$est.noadj # WITHOUT BIAS ADJ
MatchBalance(treatment2 ~ wartype + logcost + wardur + factnum + factnum2 + 
               trnsfcap + develop + exp + decade + treaty,
             data = foo,
             match.out=genm.mout.pbs2l, nboots=1000)



# For pbs5l (the matching is the same, obviously, but the function returns estimates based on the Y, naturally)
genm.mout.pbs5l = Match(Y = foo$pbs5l, Tr = foo$treatment2, X =  X_gm.plus.p.scores, 
                        Weight.matrix = genout,
                        BiasAdjust = TRUE,
                        M=1)
summary(genm.mout.pbs5l)
# ESTIMANDS
genm.mout.pbs5l$est # WITH BIAS ADJUSTMENT
genm.mout.pbs5l$est.noadj # WITHOUT BIAS ADJUSTMENT

MatchBalance(treatment2 ~ wartype + logcost + wardur + factnum + factnum2 + 
               trnsfcap + develop + exp + decade + treaty,
             data = foo,
             match.out=genm.mout.pbs5l, nboots=1000)




#                           
#                           For the matching exercises, measure balance on AT LEAST the basic variables we considered in the class exercise. 
#                           
#                           For the genetic matching exercise, population size should be at least 200 and you should run it for at least 25 generations 
#                           (which may require you to modify the number of non-changing generations). When performing genetic matching, take a little time to try
#                           different approaches to producing excellent balance. You can tweak the values of "M", you can do caliper matching, you can match
#                           on quadratic and/or interaction terms, you can add a propensity score, you can attempt exact matching, etc.
#                           
#                           JUST ONE WORD OF ADVICE: The precise way you run GenMatch is how you have to run Match. For example, if you run GenMatch with M = 2 and 
#                           X includes interaction terms etc., then in the next line of code you have to run Match exactly the same way (using the GenMatch output
#                           as the weight.matrix). Then in the next line you run MatchBalance, using the Match output.
#                           
#                           Match with replacement and allow ties. Ideally, you would measure/optimize balance on the interaction terms and quadratic terms 
#                           as well (but this will make things a bit harder than simply balancing on the basic variables). 
#                           
#                           Your final answer should include:
#                           
#                           (i) a table like this one--the caption below the table should include the asterisked footnotes AS WELL AS **the functional forms of 
#                           the propensity score model, **the variables you've genetically matched on, and **the MatchBalance variables used for 
#                           genetic matching:
#                             
#                             ******TABLE FORMAT******* (Please give it a title)
#                                                     tmt effect (bias adj) 	tmt effect (no bias adj)	p-value (from MatchBalance)
#                           logistic regression
#                           len success 2 years 		                  NA*           
#                             len success 5 years 		                NA*           
#                             
#                             p- score matching	  	
#                           len success 2 years 					                            **                 
#                             len success 5 years 					                          **                 
#                             
#                             gen match
#                           len success 2 years 					                            **                  
#                             len success 5 years 					                          **                  
#                             
#                             *No need to provide bias-adjusted results for logistic regression--only for matching estimates.
#                           **Only provide a treatment effect for matching results if your leximin p-value is above 0.10. Otherwise write in "NA".
#                           
#                           (ii) Let's pretend you have to write a decision memo for policy purposes summarizing all your work (above). Your memo would begin with a 
#                           a brief executive summary summarizing what you've done and your policy advice, and it would end with a brief concluding passage 
#                           restating your analysis and what you want your reader to take away from it (including the policy advice). The executive summary
#                           and the conclusion would be very similar--to the extent the two are at all different, there is scope for the conclusion to be a bit 
#                           more technical and/or nuanced, and the conclusion could also include some recommendations for relevant future analysis. 
#                           DO NOT WRITE the ENTIRE decision memo. Instead, just provide a 3-5 sentence executive summary AND a separate 
#                           3-5 sentence conclusion. DO ADDRESS THE MEMO TO A SPECIFIC PERSON (USE YOUR IMAGINATION, BUT TAKE THE EXERCISE SERIOUSLY.)




















# SOME ADVICE

# 
# JOSE:
#   
#   
#   Y = (constant + a bunch of Xs times their coefficients) + B1*treat + B2*wardur + B3*treat*wardur
# 
# For a treated unit: 
#   Y(T) = (some big constant + B2*wardur) + B1 + B3*wardur
# 
# For a control unit: 
#   Y(C) = (some big constant + B2*wardur)
# 
# So the treatment effect is:
#   Y(T) - Y(C) for every month of wardur, in the logistic function
# 
# exp(Y(T)/(1 + exp(Y(T)) - exp(Y(C)/(1 + exp(Y(C)) rolling through for all the different months…
#                                     
#                                     predict(glm1, newdata = new data set whatever that is, type = “response”)
#                                     # but please note that when you put your new data set in, make sure your data frame is exactly like the data frame that you put in originally when you fit your original model--in other words, the same variables, in the same order, with the same variable names.
#                                     
#                                     If you’re going to use predict, then make sure when you fit your original model (at the beginning), you specify your data set using data = ... , NOT  NOT NOT using foo$ format.
#                                     
#                                     ATT
#                                     
#                                     # PLOT STUFF
#                                     

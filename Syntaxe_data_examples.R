
#1. Import data_example

data_examples <-
  read.csv2("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")

###################################################################################################
#install and activate lavaan package

install.packages("lavaan")
library(lavaan) # only need once per session
################################################################################################################################
################################################################################################################################

#3.1Path Analysis

#3.1.1 Model specification and identification#

model.example1 <- " 
Self_efficacy ~ QEB_F + SA_F + IEI_F
Optimism ~ QEB_F + SA_F + IEI_F 

#correlations
QEB_F ~~ SA_F + IEI_F
SA_F ~~ IEI_F
"

#3.1.2 Model estimation
fit.example1 <- cfa(model.example1, data = data_examples)

#3.1.3 Model evaluation
fitMeasures(fit.example1)

#3.1.4 Model interpretation (extract the results)
summary(fit.example1)  

#Additional specifications to summary()function
summary(fit.example1, standardized = TRUE, rsquare = TRUE, 
        modindices=TRUE)

#Instead to add this additional argument to summary()function we can opt to run the
#modificationindices() function to determine which model parameters would be result in 
#a significant fit improvement

modificationindices(fit.example1)

###############################################################################################
#3.2. The measurement model (CFA) 

#3.2.1 Model specification and identification

model.example2 <- " 
QEB_father =~ QEB2 + QEB4 + QEB9 + QEB13 + QEB15
SA_father =~ SA3 + SA5 + SA6 + SA12 + SA14
IEI_father =~ IEI1 + IEI7 + IEI8 + IEI10 + IEI11

QEB_father ~~ SA_father + IEI_father
SA_father ~~ IEI_father
"

#3.2.2 Model estimation
fit.example2 <- cfa(model.example2, data = data_examples)

#3.2.3 Model evaluation
fitMeasures(fit.example2)

#Model does not present an adequate fit to the data (RMSEA > .08); therefore new specifications
#should be added to model (model re-specification). We should run the modificationindices() 
#function to analyze the local diagnostic.

modificationindices(fit.example2)


#3.2.3.1 Model re-specification

model.example2.1 <- " 
QEB_father =~ QEB2 + QEB4 + QEB9 + QEB13 + QEB15
SA_father =~ SA3 + SA5 + SA6 + SA12 + SA14
IEI_father =~ IEI1 + IEI7 + IEI8 + IEI10 + IEI11

QEB_father ~~ SA_father + IEI_father
SA_father ~~ IEI_father

SA5 ~~ SA6 
SA3 ~~ SA12
QEB4 ~~ QEB9
"

#Estimate and evaluate re-specified model
fit.example2.1 <- cfa(model.example2.1, data = data_examples)
fitMeasures(fit.example2.1)

# Model comparison
anova(fit.example2, fit.example2.1)

#Model interpretation
summary (fit.example2.1, standardized = TRUE, rsquare = TRUE)

###############################################################################################
#3.3.  Structural Equation Model - Multiple regression model

#3.3.1 Model specification and identification

model.example3 <- "
#define independent latent variables (attachment to father)
QEB_father =~ QEB2 + QEB4 + QEB9 + QEB13 + QEB15

#define dependent latent variables (self-efficacy and optimism)
self_efficacy =~ SE1 + SE2 + SE3 + SE4 + SE5 + SE6 + SE7 + SE8 + SE9 + SE10
optimism =~ OPT1 + OPT2 + OPT3 + OPT4 + OPT5 + OPT7

#define the links that we intend to test
self_efficacy ~ QEB_father 
optimism ~ QEB_father
"

#3.3.2 Model estimation
fit.example3 <- cfa(model.example3, data = data_examples)

#3.3.3 Model evaluation
fitMeasures(fit.example3)
fitMeasures(fit.example3, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr")) #if we 
#have interest in only a single or a few fit measures we can specified them by name and only 
#those are computed and returned 

#3.3.4 model interpretation
summary(fit.example3, standardized = TRUE, rsquare = TRUE)

##############################################################################################

#3.4. Structural Equation Model - Mediation

#3.4.1 Model specification and identification

model.example4 <- "
#define independent latent variables (quality of emotional bound to father)
QEB_father =~ QEB2 + QEB4 + QEB9 + QEB13 + QEB15

#define mediating latent variable (self-efficacy)
self_efficacy =~ SE1 + SE2 + SE3 + SE4 + SE5 + SE6 + SE7 + SE8 + SE9 + SE10

#define dependent latent variable (optimism)
optimism =~ OPT1 + OPT2 + OPT3 + OPT4 + OPT5 + OPT7

#define the direct links 
#direct link between independent and dependent factors
optimism ~ a*QEB_father

#direct link between independent and mediating factors
self_efficacy ~ b*QEB_father

#direct link between mediating and dependent factors
optimism ~ c*self_efficacy

#indirect effect (an indirect effect is obtained by multiplying two direct paths)
indirect_effect := b*c

#total effect (sum of a direct effect with indirect effect)
total_effect := a + (b*c)
"

#3.4.2 Model estimation
fit.example4 <- cfa(model.example4, data = data_examples, bootstrap = TRUE)


#3.4.3 Model evaluation
fitMeasures(fit.example4)


#3.4.4 Model interpretation
summary(fit.example4, standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit.example4)#To obtain the Interval confidences of parameters estimates


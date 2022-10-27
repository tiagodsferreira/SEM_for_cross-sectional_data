
#1. Import data_example

url_datafile<- "https://raw.githubusercontent.com/tiagodsferreira/SEM_for_cross-sectional_data/main/data_examples.csv"
data <-read.csv2(url_datafile, sep=";", fileEncoding="UTF-8-BOM")

###################################################################################################
#install and activate lavaan package

install.packages("lavaan")
library(lavaan) # only need once per session

################################################################################################################################


#Path Analysis

Model_1 <- " 
Self_efficacy ~ QEB_F + SA_F + IEI_F
Optimism ~ QEB_F + SA_F + IEI_F 

#correlations
QEB_F ~~ SA_F + IEI_F
SA_F ~~ IEI_F
"

fit.Model_1 <- sem(Model_1, data = data)

summary(fit.Model_1)  

#Additional specifications to summary()function
summary(fit.Model_1, standardized = TRUE, rsquare = TRUE, modindices=TRUE)

#Instead this additional argument to summary()function we can opt to run the
#modificationindices() function to determine which model parameters would be 
#result in a significant fit improvement.

modificationindices(fit.Model_1)

######################################################################################################
#Confirmatory Factor Analysis (CFA)

Model_2 <- " 
QEB_father =~ QEB2 + QEB4 + QEB9 + QEB13 + QEB15
SA_father =~ SA3 + SA5 + SA6 + SA12 + SA14
IEI_father =~ IEI1 + IEI7 + IEI8 + IEI10 + IEI11

QEB_father ~~ SA_father + IEI_father
SA_father ~~ IEI_father
"

fit.Model_2 <- cfa(Model_2, data = data)


fitMeasures(fit.Model_2)

#Model does not present an adequate fit to the data (RMSEA > .08); therefore 
#new specifications should be added to model (model re-specification).

modificationindices(fit.Model_2)


#Model re-specification

Model_3 <- " 
QEB_father =~ QEB2 + QEB4 + QEB9 + QEB13 + QEB15
SA_father =~ SA3 + SA5 + SA6 + SA12 + SA14
IEI_father =~ IEI1 + IEI7 + IEI8 + IEI10 + IEI11

QEB_father ~~ SA_father + IEI_father
SA_father ~~ IEI_father

SA5 ~~ SA6 
SA3 ~~ SA12
QEB4 ~~ QEB9
"

fit.Model_3 <- cfa(Model_3, data = data)
fitMeasures(fit.Model_3)

anova(fit.Model_2, fit.Model_3)

summary (fit.Model_3, standardized = TRUE, rsquare = TRUE)

######################################################################################################
#Full Structural Equation Model (SEM)

Model_4 <- "
#define independent latent variables (attachment to father)
QEB_father =~ QEB2 + QEB4 + QEB9 + QEB13 + QEB15

#define dependent latent variables (self-efficacy and optimism)
self_efficacy =~ SE1 + SE2 + SE3 + SE4 + SE5 + SE6 + SE7 + SE8 + SE9 + SE10
optimism =~ OPT1 + OPT2 + OPT3 + OPT4 + OPT5 + OPT7

#define the links that we intend to test
self_efficacy ~ QEB_father 
optimism ~ QEB_father
"

fit.Model_4 <- sem(Model_4, data = data)

fitMeasures(fit.Model_4)
fitMeasures(fit.Model4, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr")) #if we 
#have interest in only a single or a few fit measures we can specified them by name and only 
#those are computed and returned 


summary(fit.Model4, standardized = TRUE, rsquare = TRUE)

##############################################################################################

#Structural Equation Model - Mediation

Model_5 <- "
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


fit.Model_5 <- sem(Model_5, data = data, se = "bootstrap", bootstrap = 5000)


fitMeasures(fit.Model_5)


summary(fit.Model_5, standardized = TRUE, rsquare = TRUE)
parameterEstimates(fit.Model_5) # To obtain the Interval confidences of parameters estimates


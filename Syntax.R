
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

fitMeasures(fit.Model_1)

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
QEB =~ QEB2 + QEB4 + QEB9 + QEB13 + QEB15
SA =~ SA3 + SA5 + SA6 + SA12 + SA14
IEI =~ IEI1 + IEI7 + IEI8 + IEI10 + IEI11

QEB ~~ SA + IEI
SA ~~ IEI
"

fit.Model_2 <- cfa(Model_2, data = data)


fitMeasures(fit.Model_2)

#Model does not present an adequate fit to the data (RMSEA > .08); therefore 
#new specifications should be added to model (model re-specification).

modificationindices(fit.Model_2)


#Model re-specification

Model_2.1 <- " 
QEB =~ QEB2 + QEB4 + QEB9 + QEB13 + QEB15
SA =~ SA3 + SA5 + SA6 + SA12 + SA14
IEI =~ IEI1 + IEI7 + IEI8 + IEI10 + IEI11

QEB ~~ SA + IEI
SA ~~ IEI

SA5 ~~ SA6 
SA3 ~~ SA12
QEB4 ~~ QEB9
"

fit.Model_2.1 <- cfa(Model_2.1, data = data)
fitMeasures(fit.Model_2.1)
anova(fit.Model_2, fit.Model_2.1)
summary (fit.Model_2.1, standardized = TRUE, rsquare = TRUE)

######################################################################################################
#Full Structural Equation Model (SEM)

Model_3 <- "
#define independent latent variables (attachment to father)
QEB =~ QEB2 + QEB4 + QEB9 + QEB13 + QEB15

#define dependent latent variables (self-efficacy and optimism)
SE =~ SE1 + SE2 + SE3 + SE4 + SE5 + SE6 + SE7 + SE8 + SE9 + SE10
OPT =~ OPT1 + OPT2 + OPT3 + OPT4 + OPT5 + OPT7

#define the links that we intend to test
SE ~ QEB 
OPT ~ QEB
"

fit.Model_3 <- sem(Model_3, data = data)

fitMeasures(fit.Model_3)
fitMeasures(fit.Model_3, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr")) #if we 
#have interest in only a single or a few fit measures we can specified them by name and only 
#those are computed and returned 


summary(fit.Model_3, standardized = TRUE, rsquare = TRUE)


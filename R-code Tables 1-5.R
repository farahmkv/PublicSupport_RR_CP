########################################################################################
#               This scripts creates Tables 1-5 used in our paper 
#   "Public support for carbon pricing policies and revenue recycling options" (2024)
#             
#                               This version: June 2024
########################################################################################



# LOAD PACKAGES
library(readxl)
library(dplyr)
library(sandwich)
library(lmtest)
library(margins)
library(rcompanion)



# SPECIFY INPUT FILE
codebook <- read_excel("C:/Users/admin/Documents/MASTER_Coding.xlsx", sheet = "MAIN Tab")

#######################################################################

# DATA CLEANING AND CONTROLS

# delete first row with explanations   
codebook <- codebook[-1,]

# delete observations that are excluded
rows_to_keep <- which(codebook$incl_excl == "include")
codebook <- codebook[rows_to_keep,]

# delete empty rows (those without id number)
codebook <- codebook[complete.cases(codebook$id_nmbr),]

# split first column in four and create an survey_id, which identifies each survey uniquely
codebook <- codebook %>% 
  mutate(paper_id = substr(id_nmbr, 1, 3),
         model_id = substr(id_nmbr, 5, 7),
         regression_id = substr(id_nmbr, 9, 11))
codebook$survey_id <- paste(codebook$paper_id, codebook$id_survey, sep = "_")

# preparation of explanatory variables

# rroption_new (main explanatory variable of interest: different revenue recycling options)
codebook$rroption_new <- ifelse(codebook$rroption_main == "Tax cuts - general" | codebook$rroption_main == "Tax cuts - targeted", "Tax cuts general / targeted", codebook$rroption_main) 
codebook <- codebook %>% filter(rroption_new != "Other - general")

# policy_new (policy type) 
codebook$policy_new <- ifelse(codebook$pol_type == "Fossil Fuel Subsidy Reform", "Subsidy", "Tax")

# crbnprc_pol_group (if a carbon policy is already in place)
codebook$crbnprc_pol_group <- as.character(ifelse(codebook$crbnprc_pol_group == "No"
                                                  | codebook$crbnprc_pol_group == "None", 
                                                  "None", "Yes"))

# dechezlepretre (if observation is from Dechezleprêtre et al 2022)
codebook$dechezlepretre <- ifelse(grepl("078", codebook$id_nmbr), 1, 0)

# regression (if observation comes from a paper that also performs a regression analysis)
codebook <- codebook %>%
  mutate(best_desc2 = ifelse(is.na(codebook$best_desc), 0, 1),
         best_mdl2 = ifelse(is.na(codebook$best_mdl), 0, 1)) %>%
  group_by(survey_id) %>%
  mutate(regression = ifelse(any(best_desc2 == "1") & any(best_mdl2 == "1"), 1, 0)) %>%
  ungroup()

# region (create region assignments)
europe_westasia <- c("Norway", "Italy", "Turkey", "Switzerland", "United Kingdom", "Germany", "Spain", "Belgium",
                        "Denmark", "France", "Poland", "Ukraine", "Sweden")
latinamerica_caribbean <- c("Brazil", "Mexico", "Ecuador")
northamerica <- c("Canada", "USA")
middleeast_africa <- c("South Africa", "Egypt")
asiaandpacific <- c("India", "Australia", "Japan", "China", "South Korea", "Indonesia")

codebook$region <- ifelse(codebook$smplng_cntry %in% europe_westasia, "Europe and West Asia", 
                          ifelse(codebook$smplng_cntry %in% asiaandpacific, "Asia and Pacific",
                                 ifelse(codebook$smplng_cntry %in% latinamerica_caribbean, "Latin America and Caribbean",
                                        ifelse(codebook$smplng_cntry %in% middleeast_africa, "Africa",
                                               ifelse(codebook$smplng_cntry %in% northamerica, "North America", NA)))))

# create binary outcome variable
codebook$z <- ifelse(codebook$rr_option_effctsz_drctn == "increase", 1, ifelse(
  codebook$rr_option_effctsz_drctn == "decrease", 0, NA))
codebook <- codebook[!is.na(codebook$z),] #259 observations left

# additional cleaning of the data set

# exclude pooled observations
codebook <- codebook[codebook$smplng_cntry != "Pooled",]

# define data set that only contains observations with "unspecified" as baseline
codebook <- subset(codebook, rr_baseline_group %in% c("Unspecified"))

# use observations from descriptive stats only
codebook <- codebook[!is.na(codebook$best_desc),] 


#######################################################################

# REGRESSIONS

# set reference categories
codebook$region <- relevel(as.factor(codebook$region), ref = "North America")
codebook$rroption_new <- relevel(as.factor(codebook$rroption_new), ref = "General budget")
codebook$dechezlepretre <- relevel(as.factor(codebook$dechezlepretre), ref = "0")
codebook$regression <- relevel(as.factor(codebook$regression), ref = "0")

#### TABLE 1 ####
logit <- glm(z ~ rroption_new + srvy_dsgn_group +  policy_new + crbnprc_pol_group + region, family=binomial(link='logit'), data=codebook)
logit.cluster <- coeftest(logit, vcov. = vcovCL, cluster = codebook$survey_id) # cluster standard errors by survey ID
logit.cluster
nobs(logit)

# get marginal effects
logit.margins <- margins(logit)
summary(margins(logit))

nagelkerke(logit) 

#### TABLE 2 ####
check_collinearity(logit)

#### TABLE 3 ####
# OLS
ols <- lm(z ~ rroption_new + srvy_dsgn_group +  policy_new + crbnprc_pol_group + region, data=codebook)
ols.cluster <- coeftest(ols, vcov. = vcovCL, cluster = codebook$survey_id) # cluster standard errors by survey ID
ols.cluster
nobs(ols) #259 observations

#### TABLE 4 ####
# include "dechezlepretre" dummy
logit <- glm(z ~ rroption_new + srvy_dsgn_group +  policy_new + crbnprc_pol_group + region + dechezlepretre, family=binomial(link='logit'), data=codebook)
logit.cluster <- coeftest(logit, vcov. = vcovCL, cluster = codebook$survey_id) # cluster standard errors by survey ID
logit.cluster
nobs(logit) 
nagelkerke(logit) 

#### TABLE 5 ####
# include "regression" dummy
logit <- glm(z ~ rroption_new + srvy_dsgn_group +  policy_new + crbnprc_pol_group + region + regression, family=binomial(link='logit'), data=codebook)
logit.cluster <- coeftest(logit, vcov. = vcovCL, cluster = codebook$survey_id) # cluster standard errors by survey ID
logit.cluster
nobs(logit) 
nagelkerke(logit) 

#### TABLE 6 ####
# include weights  
codebook$weights <- as.numeric(codebook$n)
codebook$weightssqr <- sqrt(codebook$weights)
logit <- glm(z ~ rroption_new + srvy_dsgn_group +  policy_new + crbnprc_pol_group + region, weight = weightssqr, family=quasibinomial(link='logit'), data=codebook)
logit.cluster <- coeftest(logit, vcov. = vcovCL, cluster = codebook$survey_id) # cluster standard errors by survey ID
logit.cluster
nobs(logit)

logit.margins <- margins(logit)
summary(margins(logit))



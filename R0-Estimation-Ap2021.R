##### 17. ESTIMATE R0 #####

# Uses methods from https://bmcinfectdis.biomedcentral.com/articles/10.1186/1471-2334-9-187
# and https://www.medrxiv.org/content/10.1101/2020.05.15.20102657v1
# relative R0 given two contact matrices
# and then compare to calcat data https://calcat.covid19.ca.gov/cacovidmodels/
# Or this other source: https://ca-covid-r.info/

#setwd("C:\\Users\\Jennifer\\Documents\\Research\\COVID-19\\SurveyR0Estimation")
library(socialmixr)
library(dplyr)
library(reshape2)

# 1. PREPARE TWO COMPARISON MATRICES - POLYMOD AND SYNTHETIC COMMUNITY

# 1A. PULL DATA FROM POLYMOD - JUST GET COMMUNITY CONTACTS
polymod_nohh <- contact_matrix(polymod, countries = "United Kingdom", #this includes both household + community contacts 
                          age.limits = c(0, 5, 13, 18, 40, 65),
                          filter = list(cnt_home = 0),
                          symmetric = TRUE)
polymod_mat_nohh <- polymod_nohh$matrix

polymod_sch_only <- contact_matrix(polymod, countries = "United Kingdom", #this includes both household + community contacts 
                               age.limits = c(0, 5, 13, 18, 40, 65),
                               filter = list(cnt_school = 1),
                               symmetric = TRUE)
polymod_sch_mat <- polymod_sch_only$matrix

# 1B. USE CLASSROOM SIZES FROM DATA
polymod_mat_nohh[2,2] <- polymod_mat_nohh[2,2] - polymod_sch_mat[2,2] + 20*5/7
polymod_mat_nohh[3,3] <- polymod_mat_nohh[3,3] - polymod_sch_mat[3,3] + 20*5/7

# 1C. USE HOUSEHOLD DATA FROM THE SURVEYS

#Load full datasets
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/MayData_Share.rData")
max(MayData_Share$total_contacts)
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/AugData_Share.rData")
max(AugData_Share$total_contacts)
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/FebData_Share.rData")
max(FebData_Share$total_contacts) 

#extract May HH information
May_dat_extract <- MayData_Share %>% dplyr::select(c(age_cat2,
                                              hh_infant_count,
                                              hh_ychild_count,
                                              hh_teen_count,
                                              hh_yadult_count,
                                              hh_madult_count,
                                              hh_oadult_count))

#extract Aug HH information
Aug_dat_extract <- AugData_Share %>% dplyr::select(c(age_cat2,
                                              hh_infant_count,
                                              hh_ychild_count,
                                              hh_teen_count,
                                              hh_yadult_count,
                                              hh_madult_count,
                                              hh_oadult_count))


#extract Aug HH information
Feb_dat_extract <- FebData_Share %>% dplyr::select(c(age_cat2,
                                                     hh_infant_count,
                                                     hh_ychild_count,
                                                     hh_teen_count,
                                                     hh_yadult_count,
                                                     hh_madult_count,
                                                     hh_oadult_count))

#combine them - in theory, HH info shouldn't chnage from May to Aug
dat_extract <- rbind(May_dat_extract, Aug_dat_extract)

#Generate HH matrix of age-structured contacts
group <- c(1, 2, 3, 4, 5, 6)
contacts_hh <- matrix(NA, nrow = 6, ncol = 6) #6 age groups 

for (i in 1:6) {
  dta_filt <- dat_extract %>% #changed from long_data3
    filter(age_cat2 == group[i]) #change to age_cat
  
  contacts_hh[1,i] = mean(dta_filt$hh_infant_count, na.rm = T)
  contacts_hh[2,i] = mean(dta_filt$hh_ychild_count, na.rm = T)
  contacts_hh[3,i] = mean(dta_filt$hh_teen_count, na.rm = T)
  contacts_hh[4,i] = mean(dta_filt$hh_yadult_count, na.rm = T)
  contacts_hh[5,i] = mean(dta_filt$hh_madult_count, na.rm = T)
  contacts_hh[6,i] = mean(dta_filt$hh_oadult_count, na.rm = T)
  
}

#Make symmetric using information on age distribution in the study region

#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/age_pop.Rdata")
# N_age <- data.frame(age_pop) #pull age_pop from PUMS.R file 
# N_age <- N_age[c(1:6),2] #
N_age <- c( 82249 ,154397, 90813, 552789, 538811, 231434)

contacts_hh_sym <- matrix(NA, nrow = 6, ncol = 6) #6 age groups 

#See https://cran.r-project.org/web/packages/socialmixr/vignettes/introduction.html for formula
for(i in 1: length(N_age)){
  for (j in 1:length(N_age)){
    contacts_hh_sym[i,j] <- 1/(2*N_age[i])*(contacts_hh[i,j]*N_age[i] + contacts_hh[j,i]*N_age[j])
  }
}

#Generate new polymod matrix, using observed HH patterns
polymod_mat <- polymod_mat_nohh + contacts_hh_sym

# 2. PREPARE SURVEY MATRICES

## CREATE FUNCTION TO GENERATE MATRIX FROM A SINGLE BOOTSTRAPPED SUVEY SAMPLE

bootstrap_survey <- function(survey_data){
  
  #make the househouse id numerical for easier manipulation
  survey_data$responseID <- as.numeric(as.factor(survey_data$responseID))
  unique_hh <- unique(survey_data$responseID)
  Nhh <- length(unique_hh)
  
  include_oadult <- FALSE
  
  while(include_oadult == FALSE){
    #create which households to sample
    HH_sample <- sample(unique_hh, Nhh, replace = T)
    
    #initialize the dataframe with single inclusion
    bs_dat <- survey_data[which(survey_data$responseID %in% HH_sample),]
    
    #create table showing how many times to select the HH into the main dataframe
    HH_table <- table(HH_sample)
    HH_table <- HH_table[HH_table >=2]
  
    #Include the repeat instances of the households
    for (i in min(HH_table):max(HH_table)){
      HH_sample_i <- names(HH_table[HH_table >= i])
      bs_dat_i <- survey_data[which(survey_data$responseID %in% HH_sample_i),]
      
      bs_dat <- rbind(bs_dat, bs_dat_i)
  
    }
    
    #see if bootstrapped sample contained older adults
    include_oadult <- sum(bs_dat$age_cat2 == 6) > 0
  }
  
  #Calculate the total number of contacts
  data_bs <- bs_dat %>% 
    mutate(infant.total = infant.contact.total + hh_infant_count, 
           ychild.total = ychild.contact.total + hh_ychild_count, 
           teen.total = teen.contact.total + hh_teen_count, 
           yadult.total = yadult.contact.total + hh_yadult_count, 
           madult.total = madult.contact.total + hh_madult_count, 
           oadult.total = oadult.contact.total + hh_oadult_count)
  group <- c(1, 2, 3, 4, 5, 6)
  contacts_hh_comm <- matrix(NA, nrow = 6, ncol = 6) #6 age groups 
  
  #summarize into a matrix
  for (i in 1:6) {
    dta_filt <- data_bs %>% #changed from long_data3
      filter(age_cat2 == group[i]) #change to age_cat
    
    contacts_hh_comm[1,i] = mean(dta_filt$infant.total, na.rm = T)
    contacts_hh_comm[2,i] = mean(dta_filt$ychild.total, na.rm = T)
    contacts_hh_comm[3,i] = mean(dta_filt$teen.total, na.rm = T)
    contacts_hh_comm[4,i] = mean(dta_filt$yadult.total, na.rm = T)
    contacts_hh_comm[5,i] = mean(dta_filt$madult.total, na.rm = T)
    contacts_hh_comm[6,i] = mean(dta_filt$oadult.total, na.rm = T)
    
  }
  
  #Make matrix symmetric
  matrix_comm_sym <- matrix(NA, nrow = 6, ncol = 6) #6 age groups 
  
  for(i in 1: length(N_age)){
    for (j in 1:length(N_age)){
      
      matrix_comm_sym[i,j] <- 1/(2*N_age[i])*(contacts_hh_comm[i,j]*N_age[i] + contacts_hh_comm[j,i]*N_age[j])

    }
  }
  
  #return the symmetric matrix
  return(matrix_comm_sym)
}

# Try an example for May
bootstrap_survey(MayData_Share)

# Try an example for August
bootstrap_survey(AugData_Share)

# Try an example for Feb
bootstrap_survey(FebData_Share)

# 3. DEFINE FUNCTIONS FOR GETTING R0

# Function to estimate relative R0: 
getRelativeR0 <- function(survey_mat, comparison_mat) {
  
  #generate a single bootstrapped sample
  survey_mat_bs <- bootstrap_survey(survey_mat)
  
  #find the eigenvalue of the bootstrapped sample
  survey_eigen <- max(Re(eigen(survey_mat_bs)$values))
  
  #find the eigenvalue of the comparison matrix
  comparison_eigen <- max(Re(eigen(comparison_mat)$values))
  
  #find ratio of dominant eigenvalues from different waves of survey 
  ratio = survey_eigen / comparison_eigen
  return(ratio) 
}

# Function to estimate R0 assuming a baseline R0 with mean 2.5
estimateR0 <- function(baselineR0_mean = 2.5, baselineR0_sd = 0.54, relativeR0, N.sims) { #assumes prior to interventions R0 was 2.5
  baselineR0 <- rnorm(N.sims, mean = baselineR0_mean, sd = baselineR0_sd ) # assume baseline R0 is normally distributed
  R0 <- relativeR0*baselineR0 #multiplies values of R0 prior to intervention (baselineR0_mean) by relative R0 (from above fctn)
  return(R0) #this gives you R0 under physical distancing interventions 
}

# Functions to use in apply calls, comparison = polymod
applyMayFun_PM <- function(x){
  return(getRelativeR0(MayData_Share, polymod_mat))
}


applyAugFun_PM <- function(x){
  return(getRelativeR0(AugData_Share, polymod_mat))
}

applyFebFun_PM <- function(x){
  return(getRelativeR0(FebData_Share, polymod_mat))
}


# 4. ESTIMATE R0 IN MAY AND AUGUST

## 4A. MAY, Comparison = POLYMOD

N.sims <- 10000
set.seed(12)

relativeR0 <- sapply(X = 1:N.sims, FUN = function(X) applyMayFun_PM(X))

estR0 <- estimateR0(relativeR0 = relativeR0, N.sims = N.sims)

May_PM <- quantile(estR0, probs = c(0.025, 0.50, 0.975))
May_PM

## 4B. AUGUST, Comparison = POLYMOD

set.seed(12)

relativeR0 <- sapply(X = 1:N.sims, FUN = function(X) applyAugFun_PM(X))

estR0 <- estimateR0(relativeR0 = relativeR0, N.sims = N.sims)

Aug_PM <- quantile(estR0, probs = c(0.025, 0.50, 0.975))
Aug_PM

## 4C. Feb, Comparison = POLYMOD
set.seed(12)

relativeR0 <- sapply(X = 1:N.sims, FUN = function(X) applyFebFun_PM(X))

estR0 <- estimateR0(relativeR0 = relativeR0, N.sims = N.sims)

Feb_PM <- quantile(estR0, probs = c(0.025, 0.50, 0.975))
Feb_PM

### 4D. PRE-SIP
estR0 <- estimateR0(relativeR0 = 1, N.sims = N.sims)
baselineR0 <- quantile(estR0, probs = c(0.025, 0.50, 0.975))
baselineR0

save(May_PM, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/May_PM.rData")
save(Feb_PM, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/Feb_PM.rData")
save(Aug_PM, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/Aug_PM.rData")
save(baselineR0, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/baselineR0.rData")

# 5. COMPARE TO KNOWN DATA IN PLOTS

load(May_PM, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/May_PM.rData")
load(Feb_PM, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/Feb_PM.rData")
load(Aug_PM, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/Aug_PM.rData")
load(baselineR0, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/baselineR0.rData")

# source: https://ca-covid-r.info/
#Reff <- read.csv("ca_daily_cases_and_r.csv")
Reff <- read.csv("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/ca_daily_cases_and_r.csv")
library(lubridate)
library(ggplot2)
Reff$Date <- mdy(Reff$Date)
Bay_Counties <- c("Alameda", "Marin", "San Francisco", "Contra Costa", "Santa Clara", "San Mateo")

Reff_Bay <- Reff %>% subset(Region %in% Bay_Counties)

survey_plot_dat <-
  data.frame(cbind(
    "R" = c(baselineR0[2], May_PM[2], Aug_PM[2], Feb_PM[2]), #2.495, 1.182, 1.289
    "R_l" = c(baselineR0[1], May_PM[1], Aug_PM[1], Feb_PM[1]), #1.420, 0.680, 0.747
    "R_u" = c(baselineR0[3], May_PM[3], Aug_PM[3], Feb_PM[3]) #3.587, 1.735, 1.845
  ))
survey_plot_dat$Date <- as.Date( c("2020-02-01", "2020-05-15", "2020-08-15", "2021-03-20"))

#To change geom_point symbology: 
# geom_point(data = survey_plot_dat, mapping = aes(x = Date, y = R, shape = type))
# In data set, add a new column for "type" where type = 16 (circle) for the last three points 
# And type = 16 (triangle) for the R0


#Generate plot
ggplot(Reff_Bay) + 
  geom_line(aes(x = Date, y = Estimated.Effective.R, group = Region), color = "darkcyan") +
  geom_ribbon(aes(x = Date, ymin = X95..CI.Low, ymax = X95..CI.High, group = Region), fill = "darkcyan", alpha = 0.08) +
  geom_point(data = survey_plot_dat, mapping = aes(x = Date, y = R)) +
  geom_errorbar(data = survey_plot_dat, mapping = aes(x = Date, ymin = R_l, ymax = R_u), width = 0.1) +
  theme_bw() + geom_hline(aes(yintercept = 1)) + theme(text = element_text(size = 15)) +
  ylab("Estimated Effective R")

ggsave("final-code/EstimatedR0_fromSurveyData.jpg", dpi = 600, width = 7, height = 5)

# see individual values...
MayR <- Reff %>% subset(Region %in% Bay_Counties & Date >= "2020-05-01" & Date <= "2020-05-31") %>%
  summarize(meanR = mean(Estimated.Effective.R, na.rm = T))
MayR

AugR <- Reff %>% subset(Region %in% Bay_Counties & Date >= "2020-08-01" & Date <= "2020-08-31") %>%
  summarize(meanR = mean(Estimated.Effective.R, na.rm = T))
AugR

FebR <- Reff %>% subset(Region %in% Bay_Counties & Date >= "2021-02-01" & Date <= "2021-04-01") %>%
  summarize(meanR = mean(Estimated.Effective.R, na.rm = T))
FebR


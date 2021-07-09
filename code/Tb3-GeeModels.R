## Table 3: GEE
## NOTES: This also creates code for forest plot figures of effect estimates 
##         Note there are 2 types of models: one model which evaluates determinants of contacts across ALL waves 
##         (no interaction) and a second *set* of models that looks at interactions between predictors across study wave
##         This is an updated version of the tb3.R file, cleaned for the MIDAS and first submission
## This code also creates Figure 3
## Last updated: 04-16-21
## Kristin Andrejko

#### 0. Libraries
library(dplyr)
library(MASS)
library(geeM)
library(Amelia)
library(ggplot2)
library(cowplot)

#### 1. Load and Clean Data #####
# Load data_comb from tb3.R

load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_comb_040821.rData") #1328
length(unique(data_comb$responseID))#1967
length(which(is.na(data_comb$more_wfh)))
max(data_comb$total_contacts)

# Create Vaccine Variable
data_comb$vaccine_mod <- ifelse(data_comb$vx == "Yes", 1,
                                ifelse(data_comb$vx == "No" | data_comb$vx == "Not sure", 0, NA))

# Create complete case data set (used for non-multiply imputed model)
data_comb2 <- data_comb[!is.na(data_comb$more_wfh),]

#### 2. FULL MODEL: NO INTERACTIONS #####
# To get estimates of more WFH, we must multiply impute values of more WFH for missing ##

#Create numeric variables for imputation
data_comb$hispanicN <- ifelse(data_comb$hispanic == "Yes", 1, 0)
data_comb$waveN <- ifelse(data_comb$wave == "may", 1, 
                          ifelse(data_comb$wave == "august", 2, 3))
data_comb$countyN <- ifelse(data_comb$county == "Alameda", 1, 
                            ifelse(data_comb$county == "San Francisco", 2, 
                                   ifelse(data_comb$county == "Contra Costa", 3, 
                                          ifelse(data_comb$county == "Sonoma", 4, 
                                                 ifelse(data_comb$county == "Marin", 5, 
                                                        ifelse(data_comb$county == "Santa Clara", 6, 
                                                               ifelse(data_comb$county == "San Mateo", 7, 
                                                                      ifelse(data_comb$county == "Solano",8,9)))))))) 
data_comb$single_parentN <- ifelse(data_comb$single_parent == "No", 0, 1)
data_comb$race_modN <- ifelse(data_comb$race_mod == "White alone", 1, 
                              ifelse(data_comb$race_mod == "Asian alone", 2, 
                                     ifelse(data_comb$race_mod == "Black or African American alone", 3, 
                                            ifelse(data_comb$race_mod == "Some other race alone", 4, 5))))

data_comb_mod <- data_comb %>% dplyr::select(responseID, total_contacts, race_modN, income_binary, hispanicN, HH_count, 
                                             age_cat2, waveN, more_wfh, single_parentN, countyN, vaccine_mod)
str(data_comb_mod)

set.seed(1)
library(Amelia)
impMulti = amelia(x=data_comb_mod,m=5,p2s=2,parallel='multicore',ncpus=4,
                  noms = 'more_wfh', id = "responseID")
impMultiHighTol = impMulti$imputations

View(impMultiHighTol[[2]])
length(which(is.na(impMultiHighTol[[2]]$more_wfh)))
# table(data_comb_mod$race_modN)
# table(impMultiHighTol[[2]]$race_modN)

imp_merge <- list(impMultiHighTol[[1]], impMultiHighTol[[2]], impMultiHighTol[[3]], 
                  impMultiHighTol[[4]], impMultiHighTol[[5]])

library(geeM) 
# reg.fn <- function(i){
#   imp_merge[[i]]$race_modN <- as.factor(imp_merge[[i]]$race_modN)
#   imp_merge[[i]]$age_cat2 <- factor(imp_merge[[i]]$age_cat2, levels = c(4, 1, 2, 3, 5, 6))
#   imp_merge[[i]]$waveN <- as.factor(imp_merge[[i]]$waveN)
#   imp_merge[[i]]$countyN <- as.factor(imp_merge[[i]]$countyN)
#   
#   mod <-  geem(total_contacts ~ race_modN + hispanicN+ income_binary + HH_count + age_cat2 + waveN + more_wfh + single_parentN + countyN +
#                  + vaccine_mod, data=imp_merge[[i]], corstr="exchangeable", id = responseID, family =quasipoisson, 
#                sandwich = TRUE) 
#   coefs = coef(mod)
#   vcov = mod$var
#   out = list(coefs, vcov)
#   return(out)
# }
library(parallel)
set.seed(1)
num = 1:5
# modFinal = mclapply(num, function(num) reg.fn(num), mc.cores = 5)
# 
# modTest <-  geem(total_contacts ~ as.factor(race_modN) + hispanicN+ income_binary + HH_count + as.factor(age_cat2) 
#                  + as.factor(waveN) + more_wfh + single_parentN + as.factor(countyN) + vaccine_mod, data=data_comb2, corstr="exchangeable", id = responseID, family =quasipoisson, sandwich = TRUE) 
# length(coef(modTest)) #vaccine is the 26th coefficient 
# coef(modTest)
# est = array(NA,dim=c(26,5,1e4)) #without interactions 26 terms, w/ interaction 36
# est2 = array(NA,dim=c(26,5,1e4)) #without interactions 26 terms, w/ interaction 36
# 
# for (i in 1:26){
#   for (j in 1:5){
#     est[i,j,] = exp(rnorm(1e4,modFinal[[j]][[1]][i],sqrt(diag(modFinal[[j]][[2]])[i]))) 
#     est2[i,j,] = rnorm(1e4,modFinal[[j]][[1]][i],sqrt(diag(modFinal[[j]][[2]])[i]))
#   }
# }
# 
# for (j in 1:26){ #j is number of variables 
#   print(names(modFinal[[1]][[1]])[j])
#   print(quantile(est[j,,],c(0.5,0.025,0.975))) #row = variable, colmn = imputation, third = array 
# }

#### TO GET MODELS FOR FINAL TABLE 3 ####
## Model 1: Age, race, hispanic, household income, number of household members, single parent, wave
## Model 3: Age, race, hispanic, household income, number of household members, single parent, wave + vaccination 
## Model 2: Age, race, hispanic, household income, number of household members, single parent, wave + more adults WFH 
## Model 4: UNIVARIATE effect of each predictor  

# mod <-  geem(total_contacts ~ race_modN + hispanicN+ income_binary + HH_count + age_cat2 + waveN + more_wfh + single_parentN + countyN +
#                + vaccine_mod, data=imp_merge[[i]], corstr="exchangeable", id = responseID, family =quasipoisson)

reg.fn.final <- function(i){ #UPDATED without county 
  imp_merge[[i]]$race_modN <- as.factor(imp_merge[[i]]$race_modN)
  imp_merge[[i]]$age_cat2 <- factor(imp_merge[[i]]$age_cat2, levels = c(4, 1, 2, 3, 5, 6))
  imp_merge[[i]]$waveN <- as.factor(imp_merge[[i]]$waveN)
  
  # mod <-  geem(total_contacts ~ race_modN + hispanicN+ income_binary + HH_count + age_cat2 + waveN  + single_parentN,
  #                data=imp_merge[[i]], corstr="exchangeable", id = responseID, family =quasipoisson) #FOR MODEL 1
  mod <-  geem(total_contacts ~ race_modN + hispanicN+ income_binary + HH_count + age_cat2 + waveN  + single_parentN + vaccine_mod,
               data=imp_merge[[i]], corstr="exchangeable", id = responseID, family =quasipoisson) #FOR MODEL 3
  # mod <-  geem(total_contacts ~ race_modN + hispanicN+ income_binary + HH_count + age_cat2 + waveN  + single_parentN + more_wfh,
  #              data=imp_merge[[i]], corstr="exchangeable", id = responseID, family =quasipoisson) #FOR MODEL 2
  # mod <-  geem(total_contacts ~ more_wfh, data=imp_merge[[i]], corstr="exchangeable", id = responseID, family =quasipoisson, 
  #              sandwich = TRUE) # FOR MODEL 4
  coefs = coef(mod); vcov = mod$var; out = list(coefs, vcov); return(out)
}

modFinal = mclapply(num, function(num) reg.fn.final(num), mc.cores = 5)
coefs <- length(modFinal[[1]][[1]])

est = array(NA,dim=c(coefs,5,1e4)) 
est2 = array(NA,dim=c(coefs,5,1e4)) 

for (i in 1:coefs){
  for (j in 1:5){
    est[i,j,] = exp(rnorm(1e4,modFinal[[j]][[1]][i],sqrt(diag(modFinal[[j]][[2]])[i]))) 
    est2[i,j,] = rnorm(1e4,modFinal[[j]][[1]][i],sqrt(diag(modFinal[[j]][[2]])[i])) 
  }
}
name <- ee <- ci_lb <- ci_ub <- c()
for (j in 1:coefs){
  name[j] <- (names(modFinal[[1]][[1]])[j])
  ee[j] <- quantile(est[j,,], 0.5, na.rm= T)
  ci_lb[j] <- quantile(est[j,,], 0.025, na.rm = T)
  ci_ub[j] <- quantile(est[j,,], 0.975, na.rm = T)
}
aOR <- data.frame(name, as.numeric(ee), as.numeric(ci_lb), as.numeric(ci_ub))
names(aOR)[2] <- "ee"
names(aOR)[3] <- "ci_lb"
names(aOR)[4] <- "ci_ub"

round0 = function(x,n){
  if (round(x,n)==round(x,n-1)){
    out = paste(round(x,n),'.0',sep='')
  } else{
    out = round(x,n)
  }
  return(out)
}

aOR$'aOR (95% CI)' <- paste0(round0(aOR$ee,2), " (", round0(aOR$ci_lb, 2), ",",
                             round0(aOR$ci_ub, 2), ")")
aOR
write.csv(aOR, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/GEE-NoInteraction-050421.csv")


# DIFFERENCE IN NUMBER OF CONTACTS IN MODELS: exp(B0 + BX)

#Compare vaccinated households to unvaccinated households, adjusting for all else <- this is exp(1) + exp(2)
# vxHH <- (quantile(est[1,,],c(0.5,0.025,0.975))) + (quantile(est[26,,],c(0.5,0.025,0.975)))
# novxHH <- (quantile(est[1,,],c(0.5,0.025,0.975)))
# vxHH - novxHH

#This is more correct <- exp(1 + 2)
# vxHH <- exp(quantile(est2[1,,],c(0.5,0.025,0.975)) + (quantile(est2[26,,],c(0.5,0.025,0.975))))
# novxHH <-  exp(quantile(est2[1,,],c(0.5,0.025,0.975)))
# vxHH - novxHH

vxHH <- (quantile(est[1,,],c(0.5,0.025,0.975)) + (quantile(est[17,,],c(0.5,0.025,0.975))))
novxHH <-  (quantile(est[1,,],c(0.5,0.025,0.975)))
vxHH - novxHH

vxHH <- exp(quantile(est2[1,,],c(0.5,0.025,0.975)) + (quantile(est2[17,,],c(0.5,0.025,0.975))))
novxHH <-  exp(quantile(est2[1,,],c(0.5,0.025,0.975)))
vxHH - novxHH

moreWFH <- exp(quantile(est2[1,,],c(0.5,0.025,0.975)) + (quantile(est2[16,,],c(0.5,0.025,0.975))))
lessWFH <-  exp(quantile(est2[1,,],c(0.5,0.025,0.975)))
moreWFH - lessWFH

hispanic <- exp(quantile(est2[1,,],c(0.5,0.025,0.975)) + (quantile(est2[6,,],c(0.5,0.025,0.975))))
Nothispanic <- exp(quantile(est2[1,,],c(0.5,0.025,0.975))) 
hispanic - Nothispanic #ALL THREE WAVES

singleParent <- exp(quantile(est2[1,,],c(0.5,0.025,0.975)) + (quantile(est2[17,,],c(0.5,0.025,0.975))))
notSingleParent <- exp(quantile(est2[1,,],c(0.5,0.025,0.975))) 
singleParent - notSingleParent #ALL THREE WAVES

highIncome <- exp(quantile(est2[1,,],c(0.5,0.025,0.975)) + (quantile(est2[7,,],c(0.5,0.025,0.975))))
lowIncome <- exp(quantile(est2[1,,],c(0.5,0.025,0.975))) 
highIncome - lowIncome #ALL THREE WAVES

ci_ub <- ci_lb <- ee <- name <- c()

for (j in 1:26){
  name[j] <- (names(modFinal[[1]][[1]])[j])
  ee[j] <- quantile(est[j,,], 0.5)
  ci_lb[j] <- quantile(est[j,,], 0.025)
  ci_ub[j] <- quantile(est[j,,], 0.975)
}

aOR <- data.frame(name, as.numeric(ee), as.numeric(ci_lb), as.numeric(ci_ub))
names(aOR)[2] <- "ee"
names(aOR)[3] <- "ci_lb"
names(aOR)[4] <- "ci_ub"

aOR$'aOR (95% CI)' <- paste0(round(aOR$ee,2), " (", round(aOR$ci_lb, 2), ",",
                             round(aOR$ci_ub, 2), ")")
aOR <- aOR %>% dplyr::select(name, 'aOR (95% CI)')

write.csv(aOR, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/GEE-Interaction-040921.csv")

#### 3. Interaction Models #### 
# Clear WD and re-load data 

load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_comb_040821.rData") #1328
length(unique(data_comb$responseID))
data_comb$vaccine_mod <- ifelse(data_comb$vx == "Yes", 1,
                                ifelse(data_comb$vx == "No" | data_comb$vx == "Not sure", 0, NA))

## 3.1 Recode variable for interaction models 

  #Race
  unique(data_comb$race_mod)
  data_comb$race_mod <- factor(data_comb$race_mod) 
  data_comb$race_mod  <- relevel(data_comb$race_mod, "White alone")
  
  #income (binary, <150K set as reference = 0)
  unique(data_comb$income_binary)
  data_comb$high_income <- factor(data_comb$income_binary) #when high_income = 1, >=150K
  table(data_comb$high_income, data_comb$income_binary)
  
  #Hispanic
  data_comb$hispanic <- factor(data_comb$hispanic) 
  data_comb$hispanic  <- relevel(data_comb$hispanic, "No")
  levels(factor(data_comb$hispanic))
  
  #HH count (categorical with 3 as ref)
  data_comb$HH_count.f <- factor(data_comb$HH_count)
  data_comb$HH_count.f  <- relevel(data_comb$HH_count.f, "3")
  
  #Wave
  data_comb$wave <- factor(data_comb$wave) 
  data_comb$wave  <- relevel(data_comb$wave, "may")
  levels(factor(data_comb$wave))
  
  #age as a categorical variable 
  data_comb$age_cat2 <- factor(data_comb$age_cat2) 
  data_comb$age_cat2  <- relevel(data_comb$age_cat2, "4")
  levels(factor(data_comb$age_cat2))
  
  #more_wfh
  table( data_comb$more_wfh, data_comb$delta_work)
  
  #single_parent
  data_comb$single_parent <- factor(data_comb$single_parent) 
  data_comb$single_parent  <- relevel(data_comb$single_parent, "No")
  levels(factor(data_comb$single_parent))
  
  #county
  data_comb$county <- factor(data_comb$county) 
  data_comb$county  <- relevel(data_comb$county, "Alameda")
  levels(factor(data_comb$county))

  q95fn = function(x){return(exp(quantile(x,c(0.5,0.025,0.975),na.rm=T)))}
  set.seed(252)
##### 3.2 HISPANICx WAVE:  ADJUSTED MODELS ##### 
mod_hisp <- geem(total_contacts ~ hispanic*wave + age_cat2 + HH_count.f + race_mod + income_binary, data=data_comb, 
                 corstr="exchangeable", id = responseID, family =quasipoisson, sandwich = TRUE) 
parsModGee = mvrnorm(1e4,coef(mod_hisp), mod_hisp$var) #var is the robust SE 
nonHispMay = parsModGee[,1]
nonHispAug = parsModGee[,1] +  parsModGee[,3]
nonHispFeb = parsModGee[,1] +  parsModGee[,4]
HispMay = parsModGee[,1] + parsModGee[,2]
HispAug = parsModGee[,1] + parsModGee[,2] + parsModGee[,3] + parsModGee[,20]
HispFeb = parsModGee[,1] +  parsModGee[,2] + parsModGee[,4] + parsModGee[,21]
coef(mod_hisp)[20]

#Difference in contacts
q95fn(HispFeb - nonHispFeb)
q95fn(HispAug - nonHispAug)
q95fn(HispMay - nonHispMay) #daily contact rates were higher among children from Hispanic or Latinx families (1.52 more contacts, 95% CI: 1.14-2.04), 
exp(quantile(HispMay,c(0.5,0.025,0.975)) - (quantile(nonHispMay,c(0.5,0.025,0.975))))

nonHispMay <- q95fn(nonHispMay)
nonHispAug <- q95fn(nonHispAug)
nonHispFeb <- q95fn(nonHispFeb)
HispMay <- q95fn(HispMay)
HispAug <- q95fn(HispAug)
HispFeb <- q95fn(HispFeb)

hisp <- data.frame(rbind(nonHispMay, nonHispAug, nonHispFeb, 
                         HispMay, HispAug, HispFeb))
hisp$month <- rep(c("Wave 1", "Wave 2", "Wave 3"), 2)
hisp$label <- c(rep("Non-hispanic",3), rep("Hispanic", 3))
names(hisp)[1] <- "ee"
names(hisp)[2] <- "ci_lb"
names(hisp)[3] <- "ci_ub"
hisp$id <- c(0, 0.3, 0.6, 2, 2.3, 2.6)

estsPrint <- hisp %>% mutate('ee 95% CI' = paste0(round(ee, 2), " (", 
                                                  round(ci_lb, 2), ", ", 
                                                  round(ci_ub, 2), ")"))
estsPrint

pHisp <- ggplot(hisp) +
  geom_point(aes(x = id, y = ee, color = month), position = position_dodge(width= .85), size = 2) +
  geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub, x = id, color = month), 
                position = position_dodge(width= .85), size = 0.5, width =0.2) + 
  scale_x_continuous(breaks=c(0.3, 2.3), 
                     labels = c("Non-hispanic", "Hispanic")) + 
  theme(#axis.text.x=element_text(angle=90,hjust=1, size = 12), 
    axis.ticks = element_blank(),
    axis.title = element_text(size = 8)) + 
  labs(y = "Expected Count of \n non-household contacts", x = "", color = "Survey Wave") + 
  scale_color_brewer(palette = "Dark2")
pHisp

p1 + theme(legend.position="none")

##### 3.3 AGE X WAVE:  ADJUSTED MODELS ##### 
mod_geeAge2 <- geem(total_contacts ~ age_cat2*wave + hispanic + HH_count.f + race_mod + income_binary, data=data_comb, corstr="exchangeable", id = responseID, family =quasipoisson, 
                    sandwich = TRUE) 
parsModGeeAge = mvrnorm(1e4,coef(mod_geeAge2), mod_geeAge2$var) #var is the robust SE 

age1839May = parsModGeeAge[,1]
age04May = parsModGeeAge[,1] + parsModGeeAge[,2]
age512May = parsModGeeAge[,1] + parsModGeeAge[,3]
age1317May = parsModGeeAge[,1] + parsModGeeAge[,4]
age4064May = parsModGeeAge[,1] + parsModGeeAge[,5]
age65May = parsModGeeAge[,1] + parsModGeeAge[,6]

age1839Aug = parsModGeeAge[,1]  + parsModGeeAge[,7]
age04Aug = parsModGeeAge[,1]    + parsModGeeAge[,7] +parsModGeeAge[,2] + parsModGeeAge[,20]
age512Aug = parsModGeeAge[,1]   + parsModGeeAge[,7] +parsModGeeAge[,3] + parsModGeeAge[,21]
age1317Aug = parsModGeeAge[,1]  + parsModGeeAge[,7] +parsModGeeAge[,4] + parsModGeeAge[,22]
age4064Aug = parsModGeeAge[,1]  + parsModGeeAge[,7] +parsModGeeAge[,5] + parsModGeeAge[,23]
age65Aug = parsModGeeAge[,1]    + parsModGeeAge[,7] +parsModGeeAge[,6] + parsModGeeAge[,24]

age1839Feb = parsModGeeAge[,1]  + parsModGeeAge[,8]
age04Feb = parsModGeeAge[,1]    + parsModGeeAge[,8] +parsModGeeAge[,2]+ parsModGeeAge[,25]
age512Feb = parsModGeeAge[,1]   + parsModGeeAge[,8] +parsModGeeAge[,3]+ parsModGeeAge[,26]
age1317Feb = parsModGeeAge[,1]  + parsModGeeAge[,8] +parsModGeeAge[,4]+ parsModGeeAge[,27]
age4064Feb = parsModGeeAge[,1]  + parsModGeeAge[,8] +parsModGeeAge[,5]+ parsModGeeAge[,28]
age65Feb = parsModGeeAge[,1]    + parsModGeeAge[,8] +parsModGeeAge[,6]+ parsModGeeAge[,29]

coef(mod_geeAge2)[21]

## Absolute differences in contacts by age 
q95fn(age512Aug - age512May) #2.67 (1.82, 3.94)
q95fn(age1317Aug - age1317May) #3.67 (95% CI: 2.01, 6.75)

q95fn(age1839Aug - age1839May) #1.05 (0.82, 1.35)
q95fn(age4064Aug - age4064May) #1.05 (0.82, 1.35)

q95fn(age512Feb - age512Aug)
q95fn(age1317Feb - age1317Aug) 
q95fn(age65Feb - age65Aug)
q95fn(age1317Feb - age1317Aug) 

ageDF <- data.frame(cbind(age1839May, age04May, age512May, age1317May, age4064May, age65May, 
                          age1839Aug, age04Aug, age512Aug, age1317Aug, age4064Aug, age65Aug, 
                          age1839Feb, age04Feb, age512Feb, age1317Feb, age4064Feb, age65Feb))

ests <- (data.frame(t(apply(ageDF, 2, q95fn))))
ests
ests$month <- c(rep("Wave 1", 6), rep("Wave 2", 6), rep("Wave 3", 6))
ests$age <- rep(c("18-39", "0-4", "5-12", "13-17", "40-64", "65+"), 3)
ests$names <- rownames(ests)
rownames(ests) <- seq(1, nrow(ests), 1)
ests$id <- c(6, 0, 2, 4, 8, 10, 
             6.3,0.3, 2.3, 4.3, 8.3, 10.3,
             6.6,0.6, 2.6, 4.6, 8.6, 10.6)
names(ests)[1] <- "ee"
names(ests)[2] <- "ci_lb"
names(ests)[3] <- "ci_ub"

estsPrint <- ests %>% mutate('ee 95% CI' = paste0(round(ee, 2), " (", 
                                                  round(ci_lb, 2), ", ", 
                                                  round(ci_ub, 2), ")"))
estsPrint

pAge <- ggplot(ests) +
  geom_point(aes(x = id, y = ee, color = month), position = position_dodge(width= .85), size = 2) +
  geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub, x = id, color = month), 
                position = position_dodge(width= .85), size = 0.5, width =0.2) + 
  scale_x_continuous(breaks=c(0.5, 2.5, 4.5, 6.5, 8.5, 10.5), 
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) + 
  theme(#axis.text.x=element_text(angle=90,hjust=1, size = 12), 
    axis.ticks = element_blank(), 
    axis.title = element_text(size = 8)) + 
  # axis.text.y = element_blank(), 
  #legend.position = "none", 
  #legend.title = element_blank(), 
  #strip.text.x = element_text(size = 12), 
  #panel.background = element_blank(), 
  #panel.grid.minor = element_line(color = "grey")) + 
  labs(y = "Expected Count of \n non-household contacts", x = "", color = "Survey Wave") + 
  scale_color_brewer(palette = "Dark2")
pAge


##### 3.4 RACE X WAVE:  ADJUSTED MODELS ##### 
mod_geeRace2 <- geem(total_contacts ~ race_mod*wave + hispanic + HH_count.f + age_cat2 + income_binary, data=data_comb, corstr="exchangeable", id = responseID, family =quasipoisson, 
                     sandwich = TRUE) 
parsModGeeRace = mvrnorm(1e4,coef(mod_geeRace2), mod_geeRace2$var) #var is the robust SE 
whiteMay = parsModGeeRace[,1] 
asianMay = parsModGeeRace[,1] + parsModGeeRace[,2]
blackMay = parsModGeeRace[,1] + parsModGeeRace[,3]
otherMay = parsModGeeRace[,1] + parsModGeeRace[,4]
twoMay =   parsModGeeRace[,1] + parsModGeeRace[,5]

whiteAug = parsModGeeRace[,1] + parsModGeeRace[,6]
asianAug = parsModGeeRace[,1] + parsModGeeRace[,6] + parsModGeeRace[,2] + parsModGeeRace[,20]
blackAug = parsModGeeRace[,1] + parsModGeeRace[,6] + parsModGeeRace[,3] + parsModGeeRace[,21]
otherAug = parsModGeeRace[,1] + parsModGeeRace[,6] + parsModGeeRace[,4] + parsModGeeRace[,22]
twoAug =   parsModGeeRace[,1] + parsModGeeRace[,6] + parsModGeeRace[,5] + parsModGeeRace[,23]

whiteFeb = parsModGeeRace[,1] + parsModGeeRace[,7]
asianFeb = parsModGeeRace[,1] + parsModGeeRace[,7] + parsModGeeRace[,2] + parsModGeeRace[,24]
blackFeb = parsModGeeRace[,1] + parsModGeeRace[,7] + parsModGeeRace[,3] + parsModGeeRace[,25]
otherFeb = parsModGeeRace[,1] + parsModGeeRace[,7] + parsModGeeRace[,4] + parsModGeeRace[,26]
twoFeb =   parsModGeeRace[,1] + parsModGeeRace[,7] + parsModGeeRace[,5] + parsModGeeRace[,27]

raceDF <- data.frame(cbind(whiteMay, asianMay, blackMay, otherMay, twoMay, 
                           whiteAug, asianAug, blackAug, otherAug, twoAug,   
                           whiteFeb, asianFeb, blackFeb, otherFeb, twoFeb))
estsRace <- (data.frame(t(apply(raceDF, 2, q95fn))))
estsRace$month <- c(rep("Wave 1", 5), rep("Wave 2", 5), rep("Wave 3", 5))
estsRace$race <- rep(c("White", "Asian", "Black", "Some other race", "Two or more races"), 3)
estsRace$names <- rownames(estsRace)
rownames(estsRace) <- seq(1, nrow(estsRace), 1)
estsRace$id <- c(0, 2, 4, 6, 8, 
                 0.3, 2.3, 4.3, 6.3, 8.3, 
                 0.6, 2.6, 4.6, 6.6, 8.6)
names(estsRace)[1] <- "ee"
names(estsRace)[2] <- "ci_lb"
names(estsRace)[3] <- "ci_ub"

estsRacePrint <- estsRace %>% mutate('ee 95% CI' = paste0(round(ee, 2), " (", 
                                                          round(ci_lb, 2), ", ", 
                                                          round(ci_ub, 2), ")"))
estsRacePrint

pRace <- ggplot(estsRace) +
  geom_point(aes(x = id, y = ee, color = month), position = position_dodge(width= .85), size = 2) +
  geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub, x = id, color = month), 
                position = position_dodge(width= .85), size = 0.5, width =0.2) + 
  scale_x_continuous(breaks=c(0.5, 2.5, 4.5, 6.5, 8.5), 
                     labels = c("White", "Asian", "Black", "Some other \n race", "Two or \n more races")) + 
  theme(#axis.text.x=element_text(angle=90,hjust=1, size = 12), 
    axis.ticks = element_blank(), 
    axis.text.y = element_text(size = 8)) + 
  #axis.text.y = element_blank()
  #legend.position = "none", 
  #legend.title = element_blank(), 
  #strip.text.x = element_text(size = 12), 
  #panel.background = element_blank(), 
  #panel.grid.minor = element_line(color = "grey")) + 
  labs(y = "", x = "", color = "Survey Time") + 
  scale_color_brewer(palette = "Dark2")
pRace

##### 3.5 WFH X WAVE:  ADJUSTED MODELS ##### 
data_comb2 <- data_comb[!is.na(data_comb$more_wfh),]

mod_wfh <- geem(total_contacts ~ more_wfh*wave + age_cat2 + HH_count.f + race_mod + hispanic+ income_binary, data=data_comb2, 
                corstr="exchangeable", id = responseID, family =quasipoisson, sandwich = TRUE) 
parsModWfh = mvrnorm(1e4,coef(mod_wfh), mod_wfh$var) #var is the robust SE 
WFHMay = parsModWfh[,1] + parsModWfh[,2]
WFHAugust = parsModWfh[,1]  + parsModWfh[,2] + parsModWfh[,3] + parsModWfh[,21]
WFHFeb = parsModWfh[,1] + parsModWfh[,2]+ parsModWfh[,4] + parsModWfh[,22]
coef(mod_wfh)[4]

q95fn(WFHMay)
q95fn(WFHAugust)
q95fn(WFHFeb)

LessWFHMay = parsModWfh[,1] 
LessWFHAugust = parsModWfh[,1] + parsModWfh[,3] 
LessWFHFeb = parsModWfh[,1] + parsModWfh[,4] 
q95fn(LessWFHMay)
q95fn(LessWFHAugust)
q95fn(LessWFHFeb)

#Difference between waves
q95fn(LessWFHMay - WFHMay) #households whose parents are unable to work from home (1.82 more contacts, 95% CI: 1.40-2.40), 
q95fn(LessWFHAugust - WFHAugust)
q95fn(LessWFHFeb - WFHFeb)

WFHDF <- data.frame(cbind(WFHMay, WFHAugust, WFHFeb, 
                          LessWFHMay, LessWFHAugust, LessWFHFeb))
estsWFH <- (data.frame(t(apply(WFHDF, 2, q95fn))))
estsWFH$month <- rep(c("Wave 1", "Wave 2", "Wave 3"), 2)
estsWFH$wfh <- c(rep("More adults WFH", 3),rep("Less/same adults WFH", 3))
estsWFH$names <- rownames(estsWFH)
rownames(estsWFH) <- seq(1, nrow(estsWFH), 1)
estsWFH$id <- c(0, 0.3, 0.6,
                2, 2.3, 2.6)
names(estsWFH)[1] <- "ee"
names(estsWFH)[2] <- "ci_lb"
names(estsWFH)[3] <- "ci_ub"

estsWFHPrint <- estsWFH %>% mutate('ee 95% CI' = paste0(round(ee, 2), " (", 
                                                              round(ci_lb, 2), ", ", 
                                                              round(ci_ub, 2), ")"))
estsWFHPrint


pWFH <- ggplot(estsWFH) +
  geom_point(aes(x = id, y = ee, color = month), position = position_dodge(width= .85), size = 2) +
  geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub, x = id, color = month), 
                position = position_dodge(width= .85), size = 0.5, width =0.2) + 
  scale_x_continuous(breaks=c(0.5, 2.5), 
                     labels = c("More adults \n working at home", "Same or less no. of \n adults working at home")) + 
  theme(axis.ticks = element_blank()) + 
  labs(y = "", x = "", color = "Survey Wave") + 
  scale_color_brewer(palette = "Dark2")
pWFH

##### 3.6 INCOME X WAVE:  ADJUSTED MODELS ##### 
# note: low income is reference, when income_binary == 1 that means high income folks 
mod_Income<- geem(total_contacts ~ income_binary*wave + hispanic + HH_count.f + age_cat2 + race_mod, data=data_comb, corstr="exchangeable", id = responseID, family =quasipoisson, 
                  sandwich = TRUE) 
parsModIncome = mvrnorm(1e4,coef(mod_Income), mod_Income$var) #var is the robust SE 
LowIncomeMay = parsModIncome[,1] 
LowIncomeAug = parsModIncome[,1]  + parsModIncome[,3] 
LowIncomeFeb = parsModIncome[,1] + parsModIncome[,4]
HighIncomeMay = parsModIncome[,1] + parsModIncome[,2] 
HighIncomeAug = parsModIncome[,1]  + parsModIncome[,2] + parsModIncome[,3]  + parsModIncome[,20]  
HighIncomeFeb = parsModIncome[,1] + parsModIncome[,2] + parsModIncome[,4]+ parsModIncome[,21]  
q95fn(HighIncomeMay)

coef(mod_Income)[2]

#Difference between waves
q95fn(LowIncomeMay - HighIncomeMay)
q95fn(LowIncomeAug - HighIncomeAug)
q95fn(LowIncomeFeb - HighIncomeFeb)

incomeDF <- data.frame(cbind(LowIncomeMay, LowIncomeAug, LowIncomeFeb, 
                             HighIncomeMay, HighIncomeAug, HighIncomeFeb))
estsIncome <- (data.frame(t(apply(incomeDF, 2, q95fn))))
estsIncome$month <- rep(c("Wave 1", "Wave 2", "Wave 3"), 2)
estsIncome$wfh <- c(rep("<$150K", 3),rep(">130K", 3))
estsIncome$names <- rownames(estsIncome)
rownames(estsIncome) <- seq(1, nrow(estsIncome), 1)
estsIncome$id <- c(0, 0.3, 0.6,
                   2, 2.3, 2.6)
names(estsIncome)[1] <- "ee"
names(estsIncome)[2] <- "ci_lb"
names(estsIncome)[3] <- "ci_ub"

estsIncomePrint <- estsIncome %>% mutate('ee 95% CI' = paste0(round(ee, 2), " (", 
                                                          round(ci_lb, 2), ", ", 
                                                          round(ci_ub, 2), ")"))
estsIncomePrint


pIncome <- ggplot(estsIncome) +
  geom_point(aes(x = id, y = ee, color = month), position = position_dodge(width= .85), size = 2) +
  geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub, x = id, color = month), 
                position = position_dodge(width= .85), size = 0.5, width =0.2) + 
  scale_x_continuous(breaks=c(0.5, 2.5), 
                     labels = c("Household income \n under $150,000", "Household income \n over $150,000")) + 
  theme(axis.ticks = element_blank(), 
        axis.title = element_text(size = 8)) + 
  labs(y = "Expected Counts of \n non-household contacts", x = "", color = "Survey Wave") + 
  scale_color_brewer(palette = "Dark2")
pIncome

# Create combined plot

pIncome2 <- pIncome + theme(legend.position="none")
pWFH2 <- pWFH + theme(legend.position="none")
pHisp2 <- pHisp+ theme(legend.position="none")
pAge2 <-  pAge+ theme(legend.position="none")
pRace2 <- pRace + theme(legend.position="none")

legend_b <- get_legend(
  pIncome +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

bottom_row <- plot_grid(pHisp2, pRace2, pIncome2, pWFH2, labels = c('B', 'C', 'D', 'E'), rel_heights = c(1, 1, 1, 1))

regEstPlot <- plot_grid(pAge2, bottom_row, legend_b, ncol = 1, labels = c('A'), rel_heights = c(1, 2, .1))
ggsave("final-code/Fig3.jpg", dpi = 600, width = 7, height = 6)

##### 3.X Vaccination x Age among third wave ony ##### 
# This estimates the # of contacts higher for child from vx vs not vx'd house

data_comb3 <- data_comb_mod %>% filter(waveN== 3)
set.seed(252)
mod_Vx_adj <- geem(total_contacts ~ vaccine_mod*factor(age_cat2) + hispanicN + HH_count + race_modN, 
                   data=data_comb3, corstr="exchangeable", id = responseID, family =quasipoisson, 
                  sandwich = TRUE) 
parsModVx = mvrnorm(1e4,coef(mod_Vx_adj), mod_Vx_adj$var) 
q95fn = function(x){return(exp(quantile(x,c(0.5,0.025,0.975),na.rm=T)))}

vx5to12 = parsModVx[,1]  + parsModVx[,2]+ parsModVx[,3]  +  parsModVx[,11] 
vx13to17 = parsModVx[,1]  + parsModVx[,2] + parsModVx[,4] +  parsModVx[,12] 

Novx5to12 = parsModVx[,1] + parsModVx[,3]  
Novx13to17 = parsModVx[,1] + parsModVx[,4] 

q95fn(vx5to12 - Novx5to12)
q95fn(vx13to17 - Novx13to17)

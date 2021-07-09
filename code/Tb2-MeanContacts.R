## Table 2: Average value of recorded contacts per person per day stratified by baseline covariates, 
##          accounting for clustering at the household level using random intercept models. 
## Last updated: 11-02-20
## NOTE: to run this file, load each data set, rename long_data4 to data_aug and run function to avoid having to change names 
## Kristin Andrejko

library(dplyr)
library(lme4)

#### 0. Load Function ####
funct_tb2 <- function(var){
  school_group <- unique(var)
  matrix_school <- matrix(NA, nrow = length(unique(var)), ncol = 7)
  
  for (i in 1:length(school_group)){ 
    dta_filt <- data_aug %>% 
      filter(var == school_group[i]) 
    
    if(length(unique(dta_filt$responseID)) < nrow(dta_filt)){ #If you can run the model, run MODEL 
     #if(length(unique(dta_filt$responseID)) + nrow(dta_filt) >= 9){ #If you can run the model, run MODEL 
      #if(length(unique(dta_filt$responseID >= 2 )) && nrow(dta_filt) > 2){ #This wasn't handling the case of >65 w/ limited data
      mod <- lmer(total_contacts ~ (1|responseID), data = dta_filt)
      matrix_school[i,1] = as.numeric(fixef(mod))
      matrix_school[i,2] = median(dta_filt$total_contacts, na.rm = T) 
      matrix_school[i,3] = quantile(dta_filt$total_contacts, 0.25) 
      matrix_school[i,4] = quantile(dta_filt$total_contacts, 0.75) 
      #matrix_school[i,5] = length(unique(dta_filt$responseID, na.rm = T)) 
      matrix_school[i,5] = nrow(dta_filt)
      matrix_school[i,6] = min(dta_filt$total_contacts) 
      matrix_school[i,7] = max(dta_filt$total_contacts) 
    }
    
    else if (nrow(dta_filt) >0) { #When you can't run the model, you just run this 
      matrix_school[i,1] = as.numeric(mean(dta_filt$total_contacts, na.rm = T)) #calculate mean number of contacts x var
      matrix_school[i,2] = median(dta_filt$total_contacts, na.rm = T) 
      matrix_school[i,3] = quantile(dta_filt$total_contacts, 0.25) 
      matrix_school[i,4] = quantile(dta_filt$total_contacts, 0.75) 
      #matrix_school[i,5] = length(unique(dta_filt$responseID, na.rm = T))
      matrix_school[i,5] = nrow(dta_filt)
      matrix_school[i,6] = min(dta_filt$total_contacts) 
      matrix_school[i,7] = max(dta_filt$total_contacts)  
    }
    
    else if (nrow(dta_filt) == 0){
      matrix_school[i,1] = NA
      matrix_school[i,2] = NA
      matrix_school[i,3] = NA
      matrix_school[i,4] = NA
      matrix_school[i,5] = NA
      matrix_school[i,6] = NA
      matrix_school[i,7] = NA
    }
  }
  
  matrix_school <- data.frame(matrix_school)
  matrix_school$label <- unique(var); 
  names(matrix_school)[1] <- "Mean"; 
  names(matrix_school)[2] <- "Median"; 
  names(matrix_school)[3] <- "q_25"
  names(matrix_school)[4] <- "q_75"
  names(matrix_school)[5] <- "Respondents"
  names(matrix_school)[6] <- "Min"
  names(matrix_school)[7] <- "Max"
  
  matrix_school <- matrix_school %>%
    mutate("Mean (IQR)" = paste0(
      (round(Mean, 2)),
      " (",
      (round(q_25, 2)),
      ", ",
      (round(q_75, 2)),
      ")")) %>%
   arrange(-Mean)
  
  matrix_school <- matrix_school[, c(8, 9, 5, 1, 2, 3, 4, 6, 7) ] #change order of columns 
  
  matrix_school <- matrix_school[!is.na(matrix_school$label), ]
  return(matrix_school)
}

mod <- lmer(total_contacts ~ (1|responseID), data = dta_filt)


#### 1. Create table using long_data4 ####
#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/long_data4.rda")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_may.rda") #new may data file that corrects issue in child ID
data_aug <- long_data4 #temp rename of long_data4 to prevent renaming below
data_aug$total_contacts <- ifelse(data_aug$total_contacts >29, 29, data_aug$total_contacts) #Top code contacts at 29

# parent <- data_aug %>% filter(child == "child0")
# child <- data_aug %>% filter(child != "child0")
# nrow(child) + nrow(parent)
# 
# data <- long_data4 %>% 
#   dplyr::select(child, responseID, age, HH_count, age_cat2, child_id) %>% 
#   group_by(responseID) %>% 
#   dplyr::summarise(count = n_distinct(child)) %>% 
#   filter(count == 1)
# View(data) 
# 
# ### Look up specific cases where there are issues with data (either child data with no parent, or parent data with no children)
# cases <- long_data4 %>% 
#   filter(responseID %in% data$responseID) %>% 
#   dplyr::select(responseID, child, age, HH_count, child_count, child_id, age_cat2, HH_age1, HH_age2, HH_age3, HH_age4, HH_age5)
# View(cases)
# 
# length(unique(cases$responseID)); nrow(cases)
# 
# cases_u18 <- cases %>% filter(age <= 18) #23 (child only)
# cases_o18 <- cases %>% filter(age >= 19) #62 (parent only)
# 
# cases_u18_df <- long_data4 %>% 
#   filter(responseID %in% cases_u18$responseID) %>% 
#   dplyr::select(responseID, child, age, HH_count, child_count, child_id, age_cat2, HH_age1, HH_age2, HH_age3, HH_age4, HH_age5)
# View(cases_u18_df)
# 
# max(data_aug$child_id, na.rm = T) #issue with child_id where nrow(child) is not the same as child_id
# length(unique(data_aug$responseID))
# nrow(data_aug)

#### 2. Create table using august data- CLEAR WD BEFORE YOU START THIS  ####
#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/data/data_aug.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_aug.Rdata") 
data_aug$total_contacts <- ifelse(data_aug$total_contacts >29, 29, data_aug$total_contacts) #Top code contacts at 29

max(data_aug$child_id, na.rm = T) #982 
length(unique(data_aug$responseID)) #716

load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_feb.Rdata") 
data_aug <- data_feb
data_aug$total_contacts <- ifelse(data_aug$total_contacts >29, 29, data_aug$total_contacts) #Top code contacts at 29
max(data_aug$child_id, na.rm = T) #865 
length(unique(data_aug$responseID)) #639 households, 1446 total observations 

#### 3. Run tb2 functin on each data set 

# Vaccination- ONLY FOR THIRD WAVE DATA 
data_aug$vx_cl  <- ifelse(data_aug$vx == "Yes", "Yes", "No") 

matrix_vx <- funct_tb2(data_aug$vx_cl); matrix_vx

#12.0 Age Category
#temporarily exclude 65+ for long_data4
matrix_age <- funct_tb2(data_aug$age_cat2); matrix_age
matrix_age$label <- ifelse(matrix_age$label == 1, "0-4", 
                           ifelse(matrix_age$label == 2, "5-12", 
                                  ifelse(matrix_age$label == 3, "13-17", 
                                         ifelse(matrix_age$label == 4, "18-39", 
                                                ifelse(matrix_age$label == 5, "40-64", "65+")))))
matrix_age
sum(matrix_age$Respondents) 

# #This confirms that the lmer() code is working
dta_filt <- data_aug %>%
  filter(age_cat2 == 4)
mod2 <- lmer(total_contacts ~ (1|responseID), data = dta_filt)
as.numeric(fixef(mod2)) #mean taking into account clustering at household
mean(dta_filt$age_cat2) #shoudl not be the same as above


#12.1 RACE
matrix_race <- funct_tb2(data_aug$race_clean); matrix_race
sum(matrix_race$Respondents) 

# dta_filt <- data_aug %>%
#   filter(race_clean == "Two or More Races")
# mod2 <- lmer(total_contacts ~ (1|responseID), data = dta_filt)
# as.numeric(fixef(mod2)) #mean taking into account clustering at household

#12.2 HISPANIC
matrix_hispanic <- funct_tb2(data_aug$hispanic); matrix_hispanic
sum(matrix_hispanic$Respondents) 

#12.3 DO SCHOOL CLOSURES HELP
matrix_help <- funct_tb2(data_aug$closure_help); matrix_help
#matrix_help <- matrix_help[-3,]; matrix_help #remove NA row, now integrated into loop 
sum(matrix_help$Respondents) 

#12.4 ARE SCHOOL CLOSURES NECESSARY: not a good variable for round 1 bc skip logic
matrix_necessary <- funct_tb2(data_aug$closure_necessary); matrix_necessary
sum(matrix_necessary$Respondents) 

#12.5 CHANGE IN ADULTS WORKING FROM HOME (1 = same or less adults working from home)
matrix_deltawork <- funct_tb2(data_aug$delta_work); matrix_deltawork
matrix_deltawork$label <- ifelse(matrix_deltawork$label == 0, "More WFH", "Same/less WFH"); matrix_deltawork
#1 means same or less adults work form home 
sum(matrix_deltawork$Respondents) 

#12.6 SCHOOL
data_aug$school2 <- ifelse(data_aug$school == "My child does not attend school/daycare", "Other", data_aug$school)
matrix_school <- funct_tb2(data_aug$school2); matrix_school
sum(matrix_school$Respondents) 

#12.7 PUBLIC VS. NON-PUBLIC SCHOOL
matrix_schoolbinary <- funct_tb2(data_aug$school_binary); matrix_schoolbinary
matrix_schoolbinary$label <- ifelse(matrix_schoolbinary$label == 1, "Public", "Not Public"); matrix_schoolbinary

#12.8 INCOME CAT
matrix_income <- funct_tb2(data_aug$income); matrix_income
sum(matrix_income$Respondents) 

#12.9 INCOME BINARY (WHERE 0 = <$150K)
matrix_incomeb <- funct_tb2(data_aug$income_binary); matrix_incomeb 
matrix_incomeb$label <- ifelse(matrix_incomeb$label == 1, ">= $150K", "<$150K"); matrix_incomeb #\u2265 is greater than or equal =>
sum(matrix_incomeb$Respondents) 

#12.10 SINGLE PARENT
matrix_sparent <- funct_tb2(data_aug$single_parent); matrix_sparent
sum(matrix_sparent$Respondents) 

#12.11 COUNTY
matrix_county <- funct_tb2(data_aug$county); matrix_county
sum(matrix_county$Respondents) 

#12.12 COVID SYMPTOMS
matrix_symptoms <- funct_tb2(data_aug$covid_symptoms); matrix_symptoms

#12.13 WEEKEND
matrix_weekend <- funct_tb2(data_aug$weekend); matrix_weekend
matrix_weekend$label <- ifelse(matrix_weekend$label == 1, "Weekend", "Weekday"); matrix_weekend

#12.14 DATE OF SURVEY
matrix_date <- funct_tb2(data_aug$survey_date); matrix_date

#12.15 HH size
data_aug$HH_count2 <- ifelse(data_aug$HH_count >=5, 5, data_aug$HH_count)
matrix_size <- funct_tb2(data_aug$HH_count2); matrix_size
matrix_size$label <- ifelse(matrix_size$label == 5, "More than 5", matrix_size$label); matrix_size 

#12.16 MAJORITY location 
#Temporarily create new variable for location majority 
data_aug$location.majority2 <- ifelse(data_aug$location.majority == "At my home", "At my home", "Other")

matrix_locmaj <- funct_tb2(data_aug$location.majority2); matrix_locmaj

## NEW FOR AUGUST WAVE ## 

#12.17 HOW CONCERNED ARE YOU ABOUT COVID
matrix_concerned <- funct_tb2(data_aug$concerned); matrix_concerned

#12.18 CHANGING FACE-TO-FACE INTERACTION
matrix_deltainteraction <- funct_tb2(data_aug$delta_interaction); matrix_deltainteraction

#12.19 SATISIFEIED WITH ABILITY TO SIP 
matrix_distancing <- funct_tb2(data_aug$sd_ability); matrix_distancing

#12.21 TRANSPORT TO SCHOOL
matrix_transport <- funct_tb2(data_aug$school.transport); matrix_transport

#12.22 EMPLOYMENT
matrix_employment <- funct_tb2(data_aug$employment); matrix_employment

#12.23 WORK LOCATION
matrix_work <- funct_tb2(data_aug$work_location); matrix_work

#12.24 ATTEND SCHOOL IN PERSON OR VIRTUALLY 
matrix_attendschool <- funct_tb2(data_aug$attend.school); matrix_attendschool

#12.25 SCHOOL INSTRUCTION PLANS
matrix_schoolplans <- funct_tb2(data_aug$school.instruc); matrix_schoolplans

### 12.26 NEW FOR THIRD WAVE- COVID VACCINES 
matrix_vaccine <- funct_tb2(data_aug$vx); matrix_vaccine

#New vx hesistancy variable
data_aug$vx_hes2 <- ifelse(data_aug$vx == "Yes", NA, data_aug$vx_notsure)
matrix_vxHes <- funct_tb2(data_aug$vx_hes2); matrix_vxHes #Everyone was asked this q, so limit to those w/o family member vx'd 

table(data_aug$vx, data_aug$age_cat2)

### 3. Stack data frame
table_meancontacts_may_new <- rbind(matrix_age, matrix_race,matrix_hispanic,matrix_size, matrix_income, matrix_incomeb, 
                                    matrix_deltawork, matrix_sparent,matrix_school, matrix_schoolbinary, matrix_help, 
                                    matrix_date, matrix_weekend, matrix_symptoms, matrix_county, matrix_locmaj)

View(table_meancontacts_may_new)


table_meancontacts_august_new <- rbind(matrix_age, matrix_race,matrix_hispanic,matrix_size, matrix_income, matrix_incomeb, 
                                       matrix_deltawork, matrix_sparent ,matrix_school, matrix_schoolbinary, matrix_help,
                                       matrix_date, matrix_weekend, matrix_symptoms, matrix_county, matrix_locmaj,
                                       matrix_necessary, matrix_concerned, matrix_deltainteraction, matrix_distancing,
                                       matrix_employment, matrix_attendschool)
View(table_meancontacts_august_new)

table_meancontacts_feb_new <- rbind(matrix_age,matrix_hispanic,matrix_size, matrix_income, matrix_incomeb, 
                                       matrix_deltawork, matrix_sparent ,matrix_school, matrix_schoolbinary, matrix_help,
                                       matrix_date, matrix_weekend, matrix_symptoms, matrix_county, matrix_locmaj,
                                       matrix_necessary, matrix_concerned, matrix_deltainteraction, matrix_distancing,
                                       matrix_employment, matrix_attendschool, matrix_vaccine, matrix_vxHes)
View(table_meancontacts_feb_new)

#write.csv(table_meancontacts_may_new, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/table_meancontacts_may_110220.csv") #OLD MAY FILE
#write.csv(table_meancontacts_august_new, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/table_meancontacts_august_new_102820.csv")
write.csv(table_meancontacts_feb_new, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/table_meancontacts_feb_new_040821.csv")
write.csv(table_meancontacts_may_new, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/table_meancontacts_may_new_040821.csv")

#### 3. Load files and create merged table ####

#tb2.may <- read.csv("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/table_meancontacts_may_102820.csv")
tb2.may <- read.csv("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/table_meancontacts_may_new_040821.csv")
tb2.august <- read.csv("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/table_meancontacts_august_new_102820.csv")

na.rows <- nrow(tb2.august) - nrow(tb2.may)

#Rbind na rows to tb2.may 
na.df <- data.frame(matrix(NA, ncol = ncol(tb2.may), nrow = na.rows))
names(na.df) <- names(tb2.may)
tb2.may2 <- rbind(tb2.may, na.df)
#dim(tb2.may2); dim(tb2.august)

#tb2.may2[nrow(tb2.may2)+1,] <- NA #Still need to add a row of NA

tb2 <- cbind(tb2.may2, tb2.august)

tb2 <- tb2[, c(2:4, 12:14)]
names(tb2)[1] <- ""
names(tb2)[2] <- "Mean (IQR)"
names(tb2)[3] <- "Respondents (No.)"
names(tb2)[4] <- ""
names(tb2)[5] <- "Mean (IQR)"
names(tb2)[6] <- "Respondents (No.)"

View(tb2)
#write.csv(tb2, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/MS/tb2.csv") #old tb2 file
write.csv(tb2, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/MS/tb2_new.csv") #new tb2 file

## NEW Add in Feb data to the table 
tb2.feb <- read.csv("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/table_meancontacts_feb_new_040821.csv")
tb2 <- tb2.feb[, c(2:4)]
View(tb2)

#Add hard coded estimates for race
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_feb.Rdata") 
data_aug <- data_feb
data_aug$total_contacts <- ifelse(data_aug$total_contacts >29, 29, data_aug$total_contacts) #Top code contacts at 29
data_aug <- data_aug %>% filter(race_clean != "Native Hawaiian or Pacific Islander alone")
matrix_race <- funct_tb2(data_aug$race_clean); matrix_race
#Add row for Native Hawaiian - reload the data_aug data set 
mean(data_aug$total_contacts[data_aug$race_clean == "Native Hawaiian or Pacific Islander alone"]) #mean = 1
quantile(data_aug$total_contacts[data_aug$race_clean == "Native Hawaiian or Pacific Islander alone"], 0.25) #1
quantile(data_aug$total_contacts[data_aug$race_clean == "Native Hawaiian or Pacific Islander alone"], 0.75) #1
length(unique(data_aug$responseID[data_aug$race_clean == "Native Hawaiian or Pacific Islander alone"])) #1 household,2 rows ofdata

matrix_race[nrow(matrix_race) + 1,] = c("Native Hawaiian or Pacific Islander alone","1 (1,1)", "2", "--", "--", "1", "1", "--", "--")
matrix_race2 <- matrix_race[,c(1:3)]
names(matrix_race2)[2] <- "Mean..IQR."
tb2 <- rbind(tb2, matrix_race2)
#reorder tb2
tb2.FebAddition <- tb2[c(1:6, 88:94, 7:87),]
tb2.FebAddition
write.csv(tb2.FebAddition, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/MS/tb2.FebAddition.csv")

#combine tb2 with tb2.FebAddition for final table 
tb2.FebAddition <- read.csv("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/MS/tb2.FebAddition.csv")
tb2 <- read.csv("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/MS/tb2_new.csv")

na.rows <- nrow(tb2.FebAddition) - nrow(tb2)
na.df <- data.frame(matrix(NA, ncol = ncol(tb2), nrow = na.rows))
names(na.df) <- names(tb2)
tb2_plusNA <- rbind(tb2, na.df)
tb2Final <- cbind(tb2_plusNA, tb2.FebAddition)
View(tb2Final)
write.csv(tb2Final, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/MS/tb2_PlusFeb.csv")

#Now organize by the same order (create three separate tables) 
may <- tb2Final[,c(1:3)]
aug <- tb2Final[,c(4:6)]
feb <- tb2Final[,c(7:9)]



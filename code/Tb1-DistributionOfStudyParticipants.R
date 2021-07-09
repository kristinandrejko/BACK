## Table 1: Distribution of study participants 
## Last updated: 10-28-20
## Kristin Andrejko

#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/long_data4.rda")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_may.rda") #new may data file that corrects issue in child ID
#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/data/data_aug.Rdata") 
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_aug.Rdata") 

load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_feb.Rdata") 

#Create Table 1

#4.1 Only keep first value of responseID for table 1 package 
unique_aug <- data_aug %>% 
  distinct(responseID, .keep_all = TRUE)
#length(unique(data_aug$responseID)); length(unique(unique_aug$responseID)) #CHECK that these are the same

unique_may <- long_data4 %>% 
  distinct(responseID, .keep_all = TRUE)
#length(unique(long_data4$responseID)); length(unique(unique_may$responseID)) #CHECK that these are the same

unique_feb <- data_feb %>% 
  distinct(responseID, .keep_all = TRUE)
#length(unique(data_feb$responseID)); length(unique(unique_feb$responseID)) #CHECK that these are the same

#Merge together the relevant cells for the May + August 
unique_aug2 <- unique_aug %>% 
  dplyr::mutate(survey_type = "August 2020") %>% 
  dplyr::select(zipcode, survey_type, income_code, race_clean_code, county, HH_count, hispanic, single_parent, survey_date)

unique_may2 <- unique_may %>% 
  mutate(survey_type = "May 2020") %>% 
  dplyr::select(zipcode, survey_type, income_code, race_clean_code, county, HH_count, hispanic, single_parent, survey_date)

unique_feb2 <- unique_feb %>% 
  mutate(survey_type = "February 2021") %>% 
  dplyr::select(zipcode, survey_type, income_code, race_clean_code, county, HH_count, hispanic, single_parent, survey_date)

unique_total <- rbind(unique_feb2, unique_aug2, unique_may2)
#nrow(unique_aug2) + nrow(unique_may2); nrow(unique_total)
#Make table1 prettier #

#Reorder the levels of variables
unique_total$income_code <- factor(unique_total$income_code, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("Less than $19,999", "$20,000 to $39,999", "$40,000 to $59,999", 
                                                                                                         "$60,000 to $79,999", "$80,000 to $99,999", "$100,000 to $149,999", 
                                                                                                         "$150,000 or more"))

unique_total$race_clean_code <- factor(unique_total$race_clean_code, levels = c(1, 2, 5, 6, 7, 8, 9), 
                                       labels = c("White alone", "Black or African American alone", "American Indian or Alaska Native", 
                                                  "Asian alone", "Native Hawaiian or Pacific Islander alone", 
                                                  "Some other race alone", "Two or More Races"))

# Deal with survey_dates being coded as "Mon/ Tue" in April survey
unique_total$survey_date <- ifelse(unique_total$survey_date == "Mon", "Monday", 
                                   ifelse(unique_total$survey_date == "Tue", "Tuesday", 
                                          ifelse(unique_total$survey_date == "Wed", "Wednesday", 
                                                 ifelse(unique_total$survey_date == "Thr", "Thursday", 
                                                        ifelse(unique_total$survey_date == "Fri", "Friday", 
                                                               ifelse(unique_total$survey_date == "Sat", "Saturday",
                                                                      ifelse(unique_total$survey_date == "Sun", "Sunday", unique_total$survey_date)))))))

unique_total$survey_date <- factor(unique_total$survey_date, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
                                                                        "Sunday"), 
                                   labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
                                              "Sunday"))
#str(unique_total$survey_date)


label(unique_total$county) <- "County"
label(unique_total$HH_count) <- "Number of Household Members"
label(unique_total$race_clean_code) <- "Race"
label(unique_total$hispanic) <- "Hispanic"
label(unique_total$income_code) <- "Income"
label(unique_total$single_parent) <- "Single Parent Household"
label(unique_total$survey_date) <- "Weekday of Reported Contacts"

#Create table
#Reorder survey_type
unique_total$survey_type <- factor(unique_total$survey_type, levels = c("May 2020", "August 2020", "February 2021"))

tab1 <- table1( ~ HH_count + county + race_clean_code + hispanic + income_code + single_parent  + survey_date | survey_type, data=unique_total)
tab1 #this outputs in html but not pdf 

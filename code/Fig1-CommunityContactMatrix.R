## Figure 1. Age Structured Contact Matrices
## Last updated: 04-12-21
## This code generates: 
  # Figure 1: Community contact matrices
  # Figure S4: age structured community contact matrices stratified by hispanic, income, adults working from home)
  # Figure S5: age structured community contact matrices stratified by vaccination status 
  # Figure S6: bootstrapped estimates fo # of contacts
## Kristin Andrejko

library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(cowplot)

library(colourvalues)
number_vector <- c(0.772, 1.235, 5.78, 8.890, 10.543, 14.702)
colourvalues::colour_values(number_vector)

myPalette <- colorRampPalette(colourvalues::colour_values(number_vector))
magma <- c("#f9a242ff", "#f9b641ff", "#f7cb44ff", "#efe350ff", "#e8fa5bff", 
           "#b8627dff", "#cc6a70ff", "#de7065ff", "#eb8055ff", "#f68f46ff", 
           "#593d9cff", "#6b4596ff", "#7e4e90ff", "#90548bff", "#a65c85ff", 
           "#042333ff", "#0c2a50ff", "#13306dff", "#253582ff", "#403891ff")
#myPalette <- colorRampPalette(magma)

#### 1. Load Data ####
#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/long_data4.rda")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_may.rda") #new may data file that corrects issue in child ID
data_may <- long_data4 
data_may$total_contacts <- ifelse(data_may$total_contacts >29, 29, data_may$total_contacts) #Top code contacts at 29
max(data_may$child_id, na.rm = T)
length(unique(data_may$responseID))

#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/data/data_aug.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_aug.Rdata")
data_aug$total_contacts <- ifelse(data_aug$total_contacts >29, 29, data_aug$total_contacts) #Top code contacts at 29
length(unique(data_aug$responseID))
max(data_aug$child_id, na.rm = T)

load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_feb.Rdata")
data_feb$total_contacts <- ifelse(data_feb$total_contacts >29, 29, data_feb$total_contacts) #Top code contacts at 29
length(unique(data_feb$responseID))
max(data_feb$child_id, na.rm = T)

# nrow(long_data4)
# 
# child <- data_aug %>% 
#   filter(child != "child0")
# max(data_aug$child_id, na.rm = T)



#1. Create function to calculate mean # of contacts between each group #####

function_cmatrix <- function(data){
  group <- c(1, 2, 3, 4, 5, 6)
  group_des <- c("infant", "ychild", "teen", "yadult", "madult", "oadult")
  mean_comm_contacts <- matrix(NA, nrow = 6, ncol = 6) #6 age groups 
  
  for (i in 1:6) {
    dta_filt <- data %>% #change to data_aug!! 
      filter(age_cat2 == group[i]) #NOTE that age1= respondent and age2= contact
    
    # mean_comm_contacts[1,i] = mean(dta_filt$infant.contact.total, na.rm = T)
    # mean_comm_contacts[2,i] = mean(dta_filt$ychild.contact.total, na.rm = T)
    # mean_comm_contacts[3,i] = mean(dta_filt$teen.contact.total, na.rm = T)
    # mean_comm_contacts[4,i] = mean(dta_filt$yadult.contact.total, na.rm = T)
    # mean_comm_contacts[5,i] = mean(dta_filt$madult.contact.total, na.rm = T)
    # mean_comm_contacts[6,i] = mean(dta_filt$oadult.contact.total, na.rm = T)
    
    mean_comm_contacts[1,i] = weighted.mean(dta_filt$infant.contact.total, dta_filt$weight, na.rm = T) 
    mean_comm_contacts[2,i] = weighted.mean(dta_filt$ychild.contact.total, dta_filt$weight, na.rm = T)
    mean_comm_contacts[3,i] = weighted.mean(dta_filt$teen.contact.total  , dta_filt$weight, na.rm = T)
    mean_comm_contacts[4,i] = weighted.mean(dta_filt$yadult.contact.total, dta_filt$weight, na.rm = T) 
    mean_comm_contacts[5,i] = weighted.mean(dta_filt$madult.contact.total, dta_filt$weight, na.rm = T)
    mean_comm_contacts[6,i] = weighted.mean(dta_filt$oadult.contact.total, dta_filt$weight, na.rm = T)
    
  }
  
  #Impose reciprocity 
  # load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/age_pop.Rdata")
  # N_age <- data.frame(age_pop) #pull age_pop from PUMS.R file 
  # N_age <- N_age[c(1:6),2] #N_age <- c( "82249" ,"154397",  "90813", "552789", "538811", "231434")
  # 
  # mean_comm_contacts_sym <- matrix(NA, nrow = 6, ncol = 6) #6 age groups 
  # 
  # #USE Equation 6 from https://www.nature.com/articles/s41467-021-20990-2#MOESM1
  # for(j in 1: length(N_age)){ #row 
  #   for (i in 1:length(N_age)){ #column
  #     mean_comm_contacts_sym[i,j] <- ((mean_comm_contacts[i,j]*N_age[j]) + (mean_comm_contacts[j,i]*N_age[i])) / (2*N_age[j])
  #   }
  # }
  
  mean_comm_contacts <- data.frame(mean_comm_contacts)
  rownames(mean_comm_contacts) <- group
  colnames(mean_comm_contacts) <- c("infant", "ychild", "teen", "yadult", "madult" ,"oadult")
  
  #save(mean_comm_contacts, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/community_matrix.Rdata")
  
  #2. Convert from wide -> long
  data_long <- melt(mean_comm_contacts)
  data_long$contact <- rep(group_des, 6) #age2= contact, age1 = respondent
  names(data_long)[1] <- "respondent"
  #names(data_long)[2] <- "contacts"
  #data_long$age1 <- c(rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6), rep(5, 6), rep(6, 6))
  #data_long[31:36, 2] <- 0 #FOR WEEKEND 
  return(data_long)
}

#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/aug_community_longdata.Rdata")
#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/may_community_longdata.Rdata")

#### 2. Create function to plot data ####
function_cplot <- function(data){
  data$respondent_code <- ifelse(data$respondent == "infant", 1, 
                                          ifelse(data$respondent == "ychild", 2, 
                                                 ifelse(data$respondent == "teen", 3,
                                                        ifelse(data$respondent == "yadult", 4,
                                                               ifelse(data$respondent == "madult", 5,6)))))
  
  data$contact_code <- ifelse(data$contact == "infant", 1, 
                                          ifelse(data$contact == "ychild", 2, 
                                                 ifelse(data$contact == "teen", 3,
                                                        ifelse(data$contact == "yadult", 4,
                                                               ifelse(data$contact == "madult", 5,6)))))
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  
  #my_breaks = c(0, 0.1, 0.5, 0.7, 1, 1.5, 2, 3, 4, 5, 10)
  
  plot <- ggplot(data, aes(x = respondent_code, y = contact_code, fill = value)) +
    theme(legend.position = "bottom") + 
    geom_tile() + 
    scale_fill_gradientn(colours = myPalette(7), name = "Contacts \n per Person \n per day",
                                       limits = c(0,3.5)) +
    # scale_fill_gradient(name = "count", trans = "log",
    #                     breaks = my_breaks, labels = my_breaks, guide = "legend") + 
    theme_bw() +
    scale_x_continuous("Age of individual", breaks = c(1,2,3,4,5,6),
                       labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +
    scale_y_continuous("Age of contact", breaks = c(1,2,3,4,5,6),
                       labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +  
    theme(axis.text.x = element_text(angle = 60, hjust = 1), 
          text = element_text(size = 10))
  return(plot)
}

plot_com_feb <- function_cplot(longFeb)
plot_com_feb

## Run functions to create two plots: 
aug_comm_long <- function_cmatrix(data_aug)
plot_com_aug <- function_cplot(aug_comm_long)

may_comm_long <- function_cmatrix(data_may)
plot_com_may <- function_cplot(may_comm_long)

feb_comm_long <- function_cmatrix(data_feb)
plot_com_feb <- function_cplot(feb_comm_long)


### SPECIAL PLOTS ###
  #Run plots for vaccinated in feb
  data_feb_vx <- data_feb %>% filter(vx == "Yes")
  data_feb_Novx <- data_feb %>% filter(vx != "Yes")
  
  feb_comm_long_vx <- function_cmatrix(data_feb_vx)
  plot_com_feb_vx <- function_cplot(feb_comm_long_vx) #change function to go up to 4
  
  feb_comm_long_Novx <- function_cmatrix(data_feb_Novx)
  plot_com_feb_Novx <- function_cplot(feb_comm_long_Novx)

  #Run plots for kids out of school - need to change limits to go up to 25
  data_feb_school <- data_feb %>% filter(data_feb$attend.school == "Yes")
  data_feb_Noschool <- data_feb %>% filter(data_feb$attend.school == "No")
  
  feb_comm_long_school <- function_cmatrix(data_feb_school)
  plot_com_feb_school <- function_cplot(feb_comm_long_school) #change function to go up to 4
  
  feb_comm_long_Noschool <- function_cmatrix(data_feb_Noschool)
  plot_com_feb_Noschool <- function_cplot(feb_comm_long_Noschool)

#Remove legend from may
plot_com_may2 <- plot_com_may + theme(legend.position="none") +  coord_equal()
plot_com_aug <- plot_com_aug + coord_equal()

feb_comm_long_Novx2 <- plot_com_feb_Novx + theme(legend.position="none") +  coord_equal()
plot_com_feb_vx <- plot_com_feb_vx + coord_equal()

# Code to draw a plot
fig1 <- ggdraw() +
  draw_plot(plot_com_may2, x = 0.0, y = 0, width = 0.42, height = .6) +#x y width height
  draw_plot(plot_com_aug, x = 0.42, y = 0, width = 0.59, height = .6) +
  draw_plot_label(c("May", "August"), c(0, 0.41), c(0.7, 0.7), size = 15)
fig1

figVx <- ggdraw() +
  draw_plot(feb_comm_long_Novx2, x = 0.0, y = 0, width = 0.42, height = .6) +#x y width height
  draw_plot(plot_com_feb_vx, x = 0.42, y = 0, width = 0.59, height = .6) +
  draw_plot_label(c("Not vaccinated", "Vaccinated"), c(0, 0.41), c(0.7, 0.7), size = 15)
figVx

# NEW: ADD IN FEB #
#Remove legend from may
plot_com_may2 <- plot_com_may + theme(legend.position="none") +  coord_equal()
plot_com_aug2 <- plot_com_aug + theme(legend.position="none") +  coord_equal()
plot_com_feb2 <- plot_com_feb +  coord_equal()

# Code to draw a plot
fig1NEW <- ggdraw() +
  draw_plot(plot_com_may2, x = 0.0, y = 0, width = 0.30, height = .6) +#x y width height
  draw_plot(plot_com_aug2, x = 0.30, y = 0, width = 0.30, height = .6) +
  draw_plot(plot_com_feb2, x = 0.60, y = 0, width = 0.40, height = .6) +
  draw_plot_label(c("May", "August", "February"), c(0, 0.3, 0.6), c(0.7, 0.7, 0.7), size = 15)
fig1NEW

##### 3. Calculate percent change ####

#Bind together may + august long data 
names(may_comm_long)[2] <- "contacts_may"
names(aug_comm_long)[2] <- "contacts_aug"
names(feb_comm_long)[2] <- "contacts_feb"

perc_change <- merge(may_comm_long, aug_comm_long, by = c("respondent", "contact"))
perc_change <- merge(perc_change, feb_comm_long, by = c("respondent", "contact"))

#Calculate change in community contacts- this just does the difference by means. 
perc_change2 <- perc_change %>% 
  mutate(diffAug = (contacts_aug - contacts_may)) %>% 
  mutate(diffFeb = (contacts_feb - contacts_may))
# mutate(diff = ((contacts_aug - contacts_may)))

perc_change2$respondent <- ifelse(perc_change2$respondent == "infant", 1, 
                                  ifelse(perc_change2$respondent == "ychild", 2, 
                                         ifelse(perc_change2$respondent == "teen", 3,
                                                ifelse(perc_change2$respondent == "yadult", 4,
                                                       ifelse(perc_change2$respondent == "madult", 5, 6)))))
perc_change2$contact <- ifelse(perc_change2$contact == "infant", 1, 
                                  ifelse(perc_change2$contact == "ychild", 2, 
                                         ifelse(perc_change2$contact == "teen", 3,
                                                ifelse(perc_change2$contact == "yadult", 4,
                                                       ifelse(perc_change2$contact == "madult", 5, 6)))))

# August contacts - May contact / May contacts

plot_com_diffAM <- ggplot(perc_change2, aes(x = respondent, y = contact, fill = diffAug)) + 
  theme(legend.position = "bottom") + geom_tile() + 
  scale_fill_gradientn(colours = myPalette(6), 
                       name = "Difference \n in Contacts \n per Person \n per day", limits = c(-1, 2)) +
  theme_bw() +
  scale_x_continuous("Age of individual", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +
  scale_y_continuous("Age of contact", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +  
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        text = element_text(size = 10)) 
 # ggtitle("Difference in mean contacts (August to May)") 
plot_com_diffAM

library(colourvalues)
number_vector <- c(0.772, 1.235, 5.78, 8.890, 10.543, 14.702)
colourvalues::colour_values(number_vector)

myPalette <- colorRampPalette(colourvalues::colour_values(number_vector))
magma <- c("#f9a242ff", "#f9b641ff", "#f7cb44ff", "#efe350ff", "#e8fa5bff", 
           "#b8627dff", "#cc6a70ff", "#de7065ff", "#eb8055ff", "#f68f46ff", 
           "#593d9cff", "#6b4596ff", "#7e4e90ff", "#90548bff", "#a65c85ff", 
           "#042333ff", "#0c2a50ff", "#13306dff", "#253582ff", "#403891ff")

myPalette <- colorRampPalette(magma)

           
plot_com_diffFM <- ggplot(perc_change2, aes(x = respondent, y = contact, fill = diffFeb)) + 
  theme(legend.position = "bottom") + geom_tile() + 
  scale_fill_gradientn(colours = myPalette(4), 
                       name = "Difference \n in contacts \n per person \n per day", limits = c(-1,2)) +
  theme_bw() +
  scale_x_continuous("Age of individual", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +
  scale_y_continuous("Age of contact", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +  
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        text = element_text(size = 10)) 
 # ggtitle("Difference in mean contacts (February to May)") 
plot_com_diffFM


plot_com_diffAM <- plot_com_diffAM + theme(legend.position="none") +  coord_equal()
plot_com_diffFM <- plot_com_diffFM + coord_equal()

# Code to draw a plot
fig1Change <- ggdraw() +
  draw_plot(plot_com_diffAM, x = 0.0, y = 0, width = 0.42, height = .6) +#x y width height
  draw_plot(plot_com_diffFM, x = 0.42, y = 0, width = 0.59, height = .6) +
  draw_plot_label(c("August 2020 to May 2020", "February 2021 to May 2020"), c(0, 0.41), c(0.7, 0.7), size = 15)
fig1Change


##################################
# CREATE PLOTS FOR HISPANIC x WAVE
##################################

data_may_hisp <- data_may %>% filter(hispanic == "Yes")
data_may_nothisp <- data_may %>% filter(hispanic != "Yes")
data_aug_hisp <- data_aug %>% filter(hispanic == "Yes")
data_aug_nothisp <- data_aug %>% filter(hispanic != "Yes")
data_feb_hisp <- data_feb %>% filter(hispanic == "Yes")
data_feb_nothisp <- data_feb %>% filter(hispanic != "Yes")

data_may_hisp_long <- function_cmatrix(data_may_hisp)
data_aug_hisp_long <- function_cmatrix(data_aug_hisp)
data_feb_hisp_long <- function_cmatrix(data_feb_hisp)
data_may_nothisp_long <- function_cmatrix(data_may_nothisp)
data_aug_nothisp_long <- function_cmatrix(data_aug_nothisp)
data_feb_nothisp_long <- function_cmatrix(data_feb_nothisp)

plot_may_hisp <- function_cplot(data_may_hisp_long) + theme(legend.position="none") +  coord_equal()
plot_aug_hisp <- function_cplot(data_aug_hisp_long) + theme(legend.position="none") +  coord_equal()
plot_feb_hisp <- function_cplot(data_feb_hisp_long) + theme(legend.position="none") +  coord_equal()
plot_may_nothisp <- function_cplot(data_may_nothisp_long) + theme(legend.position="none") +  coord_equal()
plot_aug_nothisp <- function_cplot(data_aug_nothisp_long) + theme(legend.position="none") +  coord_equal()
plot_feb_nothisp <- function_cplot(data_feb_nothisp_long) + coord_equal()

figHispanic <- ggdraw() +
  draw_plot(plot_may_hisp, x = 0.15, y = 0, width = 0.30, height = .4) +#x y width height
  draw_plot(plot_aug_hisp, x = 0.35, y = 0, width = 0.30, height = .4) +
  draw_plot(plot_feb_hisp, x = 0.55, y = 0, width = 0.30, height = .4) +
  draw_plot(plot_may_nothisp, x = 0.15, y = 0.5, width = 0.30, height = .4)  +#x y width height
  draw_plot(plot_aug_nothisp, x = 0.35, y = 0.5, width = 0.30, height = .4) +
  draw_plot(plot_feb_nothisp, x = 0.55, y = 0.5, width = 0.40, height = .4) +
  draw_plot_label(c("Wave 1", "Wave 2", "Wave 3"), c(0.25, 0.42, 0.62), c(1, 1, 1), size = 15) +
  draw_plot_label(c("Hispanic", "Not Hispanic"), c(0.03, 0), c(0.3, 0.8), size = 15)
figHispanic

legend_b <- get_legend(
  plot_feb_nothisp +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

prow <- plot_grid(
  plot_may_hisp + theme(legend.position="none"),
  plot_aug_hisp + theme(legend.position="none"),
  plot_feb_hisp + theme(legend.position="none"),
  plot_may_nothisp + theme(legend.position="none"),
  plot_aug_nothisp + theme(legend.position="none"),
  plot_feb_nothisp + theme(legend.position="none"),
  align = 'vh',
  #hjust = -1,
  nrow = 2, 
  rel_widths = c(1, 1), 
  labels = c("Hispanic", "", "", "Not Hispanic", "", ""), label_size = 12)
prow

figHispanic <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))
figHispanic
ggsave("final-code/figHispanic.jpg", dpi = 600, width = 7, height = 6)

##################################
# CREATE PLOTS FOR INCOME x WAVE
##################################

data_may_hi <- data_may %>% filter(income_binary == 1)
data_may_nothi <- data_may %>% filter(income_binary != 1)
data_aug_hi <- data_aug %>% filter(income_binary == 1)
data_aug_nothi <- data_aug %>% filter(income_binary != 1)
data_feb_hi <- data_feb %>% filter(income_binary == 1)
data_feb_nothi <- data_feb %>% filter(income_binary != 1)

data_may_hi_long <- function_cmatrix(data_may_hi)
data_aug_hi_long <- function_cmatrix(data_aug_hi)
data_feb_hi_long <- function_cmatrix(data_feb_hi)
data_may_nothi_long <- function_cmatrix(data_may_nothi)
data_aug_nothi_long <- function_cmatrix(data_aug_nothi)
data_feb_nothi_long <- function_cmatrix(data_feb_nothi)

plot_may_hi <- function_cplot(data_may_hi_long) + theme(legend.position="none") +  coord_equal()
plot_aug_hi <- function_cplot(data_aug_hi_long) + theme(legend.position="none") +  coord_equal()
plot_feb_hi <- function_cplot(data_feb_hi_long) + theme(legend.position="none") +  coord_equal()
plot_may_nothi <- function_cplot(data_may_nothi_long) + theme(legend.position="none") +  coord_equal()
plot_aug_nothi <- function_cplot(data_aug_nothi_long) + theme(legend.position="none") +  coord_equal()
plot_feb_nothi <- function_cplot(data_feb_nothi_long) + coord_equal()

figIncome <- ggdraw() +
  draw_plot(plot_may_hi, x = 0.15, y = 0, width = 0.30, height = .4) +#x y width height
  draw_plot(plot_aug_hi, x = 0.35, y = 0, width = 0.30, height = .4) +
  draw_plot(plot_feb_hi, x = 0.55, y = 0, width = 0.30, height = .4) +
  draw_plot(plot_may_nothi, x = 0.15, y = 0.5, width = 0.30, height = .4)  +#x y width height
  draw_plot(plot_aug_nothi, x = 0.35, y = 0.5, width = 0.30, height = .4) +
  draw_plot(plot_feb_nothi, x = 0.55, y = 0.5, width = 0.40, height = .4) +
  draw_plot_label(c("Wave 1", "Wave 2", "Wave 3"), c(0.25, 0.42, 0.62), c(1, 1, 1), size = 15) +
  draw_plot_label(c(">$150K", "<$150K"), c(0, 0), c(0.3, 0.8), size = 15)
figIncome

legend_b <- get_legend(
  plot_feb_nothi +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

prow <- plot_grid(
  plot_may_hi + theme(legend.position="none"),
  plot_aug_hi + theme(legend.position="none"),
  plot_feb_hi + theme(legend.position="none"),
  plot_may_nothi + theme(legend.position="none"),
  plot_aug_nothi + theme(legend.position="none"),
  plot_feb_nothi + theme(legend.position="none"),
  align = 'vh',
  #hjust = -1,
  nrow = 2, 
  rel_widths = c(1, 1), 
  labels = c("Over $150,000", "", "", "Under $150,000", "", ""), label_size = 12)
prow

figIncome <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))
figIncome
ggsave("final-code/figIncome.jpg", dpi = 600, width = 7, height = 6)

##################################
# CREATE PLOTS FOR MORE WFH x WAVE
##################################

data_may_morewfh <- data_may %>% filter(more_wfh == 1)
data_may_lesswfh <- data_may %>% filter(more_wfh != 1)
data_aug_morewfh <- data_aug %>% filter(more_wfh == 1)
data_aug_lesswfh <- data_aug %>% filter(more_wfh != 1)
data_feb_morewfh <- data_feb %>% filter(more_wfh == 1)
data_feb_lesswfh <- data_feb %>% filter(more_wfh != 1)

data_may_morewfh_long <- function_cmatrix(data_may_morewfh)
data_aug_morewfh_long <- function_cmatrix(data_aug_morewfh)
data_feb_morewfh_long <- function_cmatrix(data_feb_morewfh)
data_may_lesswfh_long <- function_cmatrix(data_may_lesswfh)
data_aug_lesswfh_long <- function_cmatrix(data_aug_lesswfh)
data_feb_lesswfh_long <- function_cmatrix(data_feb_lesswfh)

plot_may_morewfh <- function_cplot(data_may_morewfh_long) + theme(legend.position="none") +  coord_equal()
plot_aug_morewfh <- function_cplot(data_aug_morewfh_long) + theme(legend.position="none") +  coord_equal()
plot_feb_morewfh <- function_cplot(data_feb_morewfh_long) + theme(legend.position="none") +  coord_equal()
plot_may_lesswfh <- function_cplot(data_may_lesswfh_long) + theme(legend.position="none") +  coord_equal()
plot_aug_lesswfh <- function_cplot(data_aug_lesswfh_long) + theme(legend.position="none") +  coord_equal()
plot_feb_lesswfh <- function_cplot(data_feb_lesswfh_long) + coord_equal()

figWFH <- ggdraw() +
  draw_plot(plot_may_morewfh, x = 0.15, y = 0, width = 0.30, height = .4) +#x y width height
  draw_plot(plot_aug_morewfh, x = 0.35, y = 0, width = 0.30, height = .4) +
  draw_plot(plot_feb_morewfh, x = 0.55, y = 0, width = 0.30, height = .4) +
  draw_plot(plot_may_lesswfh, x = 0.15, y = 0.5, width = 0.30, height = .4)  +#x y width height
  draw_plot(plot_aug_lesswfh, x = 0.35, y = 0.5, width = 0.30, height = .4) +
  draw_plot(plot_feb_lesswfh, x = 0.55, y = 0.5, width = 0.40, height = .4) +
  draw_plot_label(c("Wave 1", "Wave 2", "Wave 3"), c(0.25, 0.42, 0.62), c(1, 1, 1), size = 15) +
  draw_plot_label(c("Adults able \n to work from home", "Adults unable \n to work from home"), c(0, 0), c(0.3, 0.8), size = 15)
figWFH

legend_b <- get_legend(
  plot_feb_lesswfh +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

prow <- plot_grid(
  plot_may_morewfh + theme(legend.position="none"),
  plot_aug_morewfh + theme(legend.position="none"),
  plot_feb_morewfh + theme(legend.position="none"),
  plot_may_lesswfh + theme(legend.position="none"),
  plot_aug_lesswfh + theme(legend.position="none"),
  plot_feb_lesswfh + theme(legend.position="none"),
  align = 'vh',
  #hjust = -1,
  nrow = 2, 
  rel_widths = c(1, 1), 
  labels = c("Adults work from home", "", "", "Adults unable to work from home", "", ""), label_size = 12)
prow

figWFH <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))
figWFH
ggsave("final-code/figWFH.jpg", dpi = 600, width = 7, height = 6)

#### BOOTSTRAPED COMMUNITY MATRICES: THIS CREATES 'FIGURE 1 FROM 04-12-21 FILE ####

## CREATE FUNCTION TO GENERATE MATRIX FROM A SINGLE BOOTSTRAPPED SUVEY SAMPLE
#Load full datasets
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/MayData_Share.rData")
max(MayData_Share$total_contacts)
data_may <- MayData_Share
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/AugData_Share.rData")
max(AugData_Share$total_contacts)
data_aug <- AugData_Share
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/FebData_Share.rData")
max(FebData_Share$total_contacts) 
data_feb <- FebData_Share
N_age <- c( 82249 ,154397, 90813, 552789, 538811, 231434)


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
    
    # contacts_hh_comm[1,i] = mean(dta_filt$infant.contact.total, na.rm = T)
    # contacts_hh_comm[2,i] = mean(dta_filt$ychild.contact.total, na.rm = T)
    # contacts_hh_comm[3,i] = mean(dta_filt$teen.contact.total, na.rm = T)
    # contacts_hh_comm[4,i] = mean(dta_filt$yadult.contact.total, na.rm = T)
    # contacts_hh_comm[5,i] = mean(dta_filt$madult.contact.total, na.rm = T)
    # contacts_hh_comm[6,i] = mean(dta_filt$oadult.contact.total, na.rm = T)
    
    contacts_hh_comm[1,i] = weighted.mean(dta_filt$infant.contact.total, na.rm = T, dta_filt$weight)
    contacts_hh_comm[2,i] = weighted.mean(dta_filt$ychild.contact.total, na.rm = T, dta_filt$weight)
    contacts_hh_comm[3,i] = weighted.mean(dta_filt$teen.contact.total,   na.rm = T, dta_filt$weight)
    contacts_hh_comm[4,i] = weighted.mean(dta_filt$yadult.contact.total, na.rm = T, dta_filt$weight)
    contacts_hh_comm[5,i] = weighted.mean(dta_filt$madult.contact.total, na.rm = T, dta_filt$weight)
    contacts_hh_comm[6,i] = weighted.mean(dta_filt$oadult.contact.total, na.rm = T, dta_filt$weight)
    
    
  }
  
  #Make matrix symmetric
  # matrix_comm_sym <- matrix(NA, nrow = 6, ncol = 6) #6 age groups 
  # 
  # for(i in 1: length(N_age)){
  #   for (j in 1:length(N_age)){
  #     
  #     matrix_comm_sym[i,j] <- 1/(2*N_age[i])*(contacts_hh_comm[i,j]*N_age[i] + contacts_hh_comm[j,i]*N_age[j])
  #     
  #   }
  # }
  # 
  #return the symmetric matrix
  #return(matrix_comm_sym)
  return(contacts_hh_comm)
  
}
# Try an example for May
bootstrap_survey(MayData_Share)

# Try an example for August
bootstrap_survey(AugData_Share)

# Try an example for Feb
bootstrap_survey(FebData_Share)

#Store 10K bootstrapped matrices
MayComMatrix <- AugComMatrix <- FebComMatrix <- array(NA, c(nrow = 6, ncol = 6, 1e5))
system.time(
  for (i in 1:1e5){
    FebComMatrix[,,i] <-bootstrap_survey(FebData_Share)
    AugComMatrix[,,i] <-bootstrap_survey(AugData_Share)
    MayComMatrix[,,i] <-bootstrap_survey(MayData_Share)
  }
)

apply(1:10, FUN = bootstrap_survey(FebData_Share))

FebComMatrix2 <- apply(FebComMatrix, 1:2, mean)

#To get uncertainity you want to do quantile()
qFunction_med <- function(data) { (quantile(data, c(0.5)))}
qFunction_lb <- function(data) { (quantile(data, c(0.025)))}
qFunction_ub <- function(data) { (quantile(data, c(0.975)))}

FebComMatrix_med <- apply(FebComMatrix, 1:2, qFunction)
FebComMatrix_lb <- apply(FebComMatrix, 1:2, qFunction_lb)
FebComMatrix_ub <- apply(FebComMatrix, 1:2, qFunction_ub)

AugComMatrix_med <- apply(AugComMatrix, 1:2, qFunction)
AugComMatrix_lb <- apply(AugComMatrix, 1:2, qFunction_lb)
AugComMatrix_ub <- apply(AugComMatrix, 1:2, qFunction_ub)

MayComMatrix_med <- apply(MayComMatrix, 1:2, qFunction)
MayComMatrix_lb <- apply(MayComMatrix, 1:2, qFunction_lb)
MayComMatrix_ub <- apply(MayComMatrix, 1:2, qFunction_ub)

save(FebComMatrix_med, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/FebComMatrix_med.Rdata")
save(FebComMatrix_lb, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/FebComMatrix_lb.Rdata")
save(FebComMatrix_ub, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/FebComMatrix_ub.Rdata")

save(AugComMatrix_med, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/AugComMatrix_med.Rdata")
save(AugComMatrix_lb, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/AugComMatrix_lb.Rdata")
save(AugComMatrix_ub, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/AugComMatrix_ub.Rdata")

save(MayComMatrix_med, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/MayComMatrix_med.Rdata")
save(MayComMatrix_lb, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/MayComMatrix_lb.Rdata")
save(MayComMatrix_ub, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/MayComMatrix_ub.Rdata")

load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/FebComMatrix_med.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/AugComMatrix_med.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/MayComMatrix_med.Rdata")

# PLOT 
longFunction <- function(data) {
  mean_comm_contacts <- data.frame(data)
  group <- c(1, 2, 3, 4, 5, 6)
  group_des <- c("infant", "ychild", "teen", "yadult", "madult", "oadult")
  
  rownames(mean_comm_contacts) <- group
  colnames(mean_comm_contacts) <- c("infant", "ychild", "teen", "yadult", "madult" ,"oadult")
  
  data_long <- melt(mean_comm_contacts)
  data_long$contact <- rep(group_des, 6) #age2= contact, age1 = respondent
  names(data_long)[1] <- "respondent"
  return(data_long)
}

longFeb <- longFunction(FebComMatrix_med)
longAug <- longFunction(AugComMatrix_med)
longMay <- longFunction(MayComMatrix_med)

plot_com_feb <- function_cplot(longFeb)
plot_com_aug <- function_cplot(longAug)
plot_com_may <- function_cplot(longMay)

plot_com_may2 <- plot_com_may + theme(legend.position="none") +  coord_equal()
plot_com_aug2 <- plot_com_aug + theme(legend.position="none") +  coord_equal()
plot_com_feb2 <- plot_com_feb +  coord_equal()

# Code to draw a plot
fig1NEW <- ggdraw() +
  draw_plot(plot_com_may2, x = 0.0, y = 0, width = 0.30, height = .6) +#x y width height
  draw_plot(plot_com_aug2, x = 0.30, y = 0, width = 0.30, height = .6) +
  draw_plot(plot_com_feb2, x = 0.60, y = 0, width = 0.40, height = .6) +
  draw_plot_label(c("Wave 1", "Wave 2", "Wave 3"), c(0, 0.3, 0.6), c(0.7, 0.7, 0.7), size = 15)
fig1NEW

#ggsave("final-code/Fig1.jpg", dpi = 600, width = 7, height = 5)

##### 3. Calculate percent change ####

#Bind together may + august long data 
names(longMay)[2] <- "contacts_may"
names(longAug)[2] <- "contacts_aug"
names(longFeb)[2] <- "contacts_feb"

perc_change <- merge(longMay, longAug, by = c("respondent", "contact"))
perc_change <- merge(perc_change, longFeb, by = c("respondent", "contact"))

#Calculate change in community contacts- this just does the difference by means. 
perc_change2 <- perc_change %>% 
  mutate(diffAug = (contacts_aug - contacts_may)) %>% 
  mutate(diffFeb = (contacts_feb - contacts_may))
# mutate(diff = ((contacts_aug - contacts_may)))

perc_change2$respondent <- ifelse(perc_change2$respondent == "infant", 1, 
                                  ifelse(perc_change2$respondent == "ychild", 2, 
                                         ifelse(perc_change2$respondent == "teen", 3,
                                                ifelse(perc_change2$respondent == "yadult", 4,
                                                       ifelse(perc_change2$respondent == "madult", 5, 6)))))
perc_change2$contact <- ifelse(perc_change2$contact == "infant", 1, 
                               ifelse(perc_change2$contact == "ychild", 2, 
                                      ifelse(perc_change2$contact == "teen", 3,
                                             ifelse(perc_change2$contact == "yadult", 4,
                                                    ifelse(perc_change2$contact == "madult", 5, 6)))))
library(colourvalues)
number_vector <- c(0.772, 1.235, 5.78, 8.890, 10.543, 14.702)
colourvalues::colour_values(number_vector)

myPalette <- colorRampPalette(colourvalues::colour_values(number_vector))

# August contacts - May contact / May contacts

plot_com_diffAM <- ggplot(perc_change2, aes(x = respondent, y = contact, fill = diffAug)) + 
  theme(legend.position = "bottom") + geom_tile() + 
  scale_fill_gradientn(colours = myPalette(6), 
                       name = "Difference \n in Contacts \n per Person \n per day", limits = c(-1, 2)) +
  theme_bw() +
  scale_x_continuous("Age of individual", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +
  scale_y_continuous("Age of contact", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +  
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        text = element_text(size = 10)) 
# ggtitle("Difference in mean contacts (August to May)") 
plot_com_diffAM

plot_com_diffFM <- ggplot(perc_change2, aes(x = respondent, y = contact, fill = diffFeb)) + 
  theme(legend.position = "bottom") + geom_tile() + 
  scale_fill_gradientn(colours = myPalette(4), 
                       name = "Difference \n in contacts \n per person \n per day", limits = c(-1,2)) +
  theme_bw() +
  scale_x_continuous("Age of individual", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +
  scale_y_continuous("Age of contact", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +  
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        text = element_text(size = 10)) 
# ggtitle("Difference in mean contacts (February to May)") 
plot_com_diffFM


plot_com_diffAM <- plot_com_diffAM + theme(legend.position="none") +  coord_equal()
plot_com_diffFM <- plot_com_diffFM + coord_equal()

fig1NEW <- ggdraw() +
  draw_plot(plot_com_may2, x = 0.0, y = 0.5, width = 0.25, height = .5) +#x y width height
  draw_plot(plot_com_aug2, x = 0.30, y = 0.5, width = 0.25, height = .5) +
  draw_plot(plot_com_feb2, x = 0.60, y = 0.5, width = 0.4, height = .5) +
  draw_plot(plot_com_diffAM, x = 0.0, y = 0, width = 0.42, height = .4) +#x y width height
  draw_plot(plot_com_diffFM, x = 0.42, y = 0, width = 0.59, height = .4) +
  draw_plot_label(c("A) Wave 1", "B) Wave 2", "C) Wave 3", "D) Difference between \n     Wave 2 to Wave 1", "E) Difference between \n     Wave 3 to Wave 1"), 
                  c(0, 0.3, 0.6, 0, 0.4), c(.95, .95, .95, 0.5, 0.5), size = 12)
fig1NEW

ggsave("final-code/Fig1.jpg", dpi = 600, width = 7, height = 6)


##### COMBINE VALUES AND 95% CI of each MATRIX 

load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/FebComMatrix_med.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/AugComMatrix_med.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/MayComMatrix_med.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/FebComMatrix_lb.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/AugComMatrix_lb.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/MayComMatrix_lb.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/FebComMatrix_ub.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/AugComMatrix_ub.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/MayComMatrix_ub.Rdata")

may <- aug <- feb <- matrix(NA, nrow = 6, ncol = 6)
for (i in 1:6) { for (j in 1:6) {
  feb[i,j] <- paste0(round(FebComMatrix_med[i,j], 2), " \n (", round(FebComMatrix_lb[i,j], 2), ", ", round(FebComMatrix_ub[i,j], 2), ")")
  aug[i,j] <- paste0(round(AugComMatrix_med[i,j], 2), " \n (", round(AugComMatrix_lb[i,j], 2), ", ", round(AugComMatrix_ub[i,j], 2), ")")
  may[i,j] <- paste0(round(MayComMatrix_med[i,j], 2), " \n (", round(MayComMatrix_lb[i,j], 2), ", ", round(MayComMatrix_ub[i,j], 2), ")")
}}

function_clong_estimate <- function(data, mergedata){ #mergedata = longFeb, longAug, longMay
  group <- c(1, 2, 3, 4, 5, 6)
  group_des <- c("infant", "ychild", "teen", "yadult", "madult", "oadult")
  
  data <- data.frame(data)
  rownames(data) <- group
  colnames(data) <- c("infant", "ychild", "teen", "yadult", "madult" ,"oadult")
  
  data_long <- tidyr::gather(data)
  data_long$contact <- rep(group_des, 6) 
  names(data_long)[1] <- "respondent"
  
  data_long2 <- left_join(mergedata, data_long, by = c("respondent", "contact"))
  names(data_long2)[4] <- "estimate"
  names(data_long2)[2] <- "value"
  return(data_long2)
}

longFeb_est <- function_clong_estimate(data = feb, mergedata = longFeb)
longMay_est <- function_clong_estimate(data = may, mergedata = longMay)
longAug_est <- function_clong_estimate(data = aug, mergedata = longAug)


function_cplot_estimates <- function(data){
  data$respondent_code <- ifelse(data$respondent == "infant", 1, 
                                 ifelse(data$respondent == "ychild", 2, 
                                        ifelse(data$respondent == "teen", 3,
                                               ifelse(data$respondent == "yadult", 4,
                                                      ifelse(data$respondent == "madult", 5,6)))))
  
  data$contact_code <- ifelse(data$contact == "infant", 1, 
                              ifelse(data$contact == "ychild", 2, 
                                     ifelse(data$contact == "teen", 3,
                                            ifelse(data$contact == "yadult", 4,
                                                   ifelse(data$contact == "madult", 5,6)))))
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  
  plot <- ggplot(data, aes(x = respondent_code, y = contact_code, fill = value)) +
    theme(legend.position = "bottom") + 
    geom_tile() + 
    scale_fill_gradientn(colours = myPalette(7), name = "Contacts \n per Person \n per day",
                         limits = c(0,3)) +
    geom_text(aes(label=estimate), size = 2.5) + 
    theme_bw() +
    scale_x_continuous("Age of individual", breaks = c(1,2,3,4,5,6),
                       labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +
    scale_y_continuous("Age of contact", breaks = c(1,2,3,4,5,6),
                       labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +  
    theme(axis.text.x = element_text(angle = 60, hjust = 1), 
          text = element_text(size = 10))
  return(plot)
}

plotAugEst <- function_cplot_estimates(longAug_est)
plotMayEst <- function_cplot_estimates(longMay_est)
plotFebEst <- function_cplot_estimates(longFeb_est)

plotMayEst2 <- plotMayEst + theme(legend.position="none") +  coord_equal()
plotAugEst2 <- plotAugEst + theme(legend.position="none") +  coord_equal()
plotFebEst2 <- plotFebEst +  coord_equal()

# Code to draw a plot -> Figure S6
figEsts <- ggdraw() +
  draw_plot(plotMayEst2, x = 0.0, y = 0, width = 0.30, height = .6) +#x y width height
  draw_plot(plotAugEst2, x = 0.30, y = 0, width = 0.30, height = .6) +
  draw_plot(plotFebEst2, x = 0.60, y = 0, width = 0.40, height = .6) +
  draw_plot_label(c("Wave 1", "Wave 2", "Wave 3"), c(0, 0.3, 0.6), c(0.7, 0.7, 0.7), size = 15)
figEsts

ggsave("final-code/FigEsts.jpg", dpi = 600, width = 7, height = 6)


## Figure 2. Age Structured Contact Matrices, by location 
## Last updated: 11-01-20
## Kristin Andrejko

#### 1. Load Data ####
#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/long_data4.rda")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_may.rda") #new may data file that corrects issue in child ID
data_may <- long_data4 
data_may$total_contacts <- ifelse(data_may$total_contacts >29, 29, data_may$total_contacts) #Top code contacts at 29

#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/data/data_aug.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_aug.Rdata")
data_aug$total_contacts <- ifelse(data_aug$total_contacts >29, 29, data_aug$total_contacts) #Top code contacts at 29

#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/data/data_aug.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_feb.Rdata")
data_feb$total_contacts <- ifelse(data_feb$total_contacts >29, 29, data_feb$total_contacts) #Top code contacts at 29


#### 2. Create function to make a location matrix 

    #Loc1: Home
    #Loc2: Work
    #Loc3: Someone else's home
    #Loc4: Childcare center
    #Loc5: Outdoor leisure
    #Loc6: Essential activities
    #Loc7: Public transit
    #Loc8: Child's school
    #Loc9: Other


function_locmatrix <- function(data){
    loc1_loop <- c("loc1.infant", "loc1.ychild", "loc1.teen", "loc1.yadult", "loc1.madult", "loc1.oadult") 
    loc2_loop <- c("loc2.infant", "loc2.ychild", "loc2.teen", "loc2.yadult", "loc2.madult", "loc2.oadult") 
    loc3_loop <- c("loc3.infant", "loc3.ychild", "loc3.teen", "loc3.yadult", "loc3.madult", "loc3.oadult") 
    loc4_loop <- c("loc4.infant", "loc4.ychild", "loc4.teen", "loc4.yadult", "loc4.madult", "loc4.oadult") 
    loc5_loop <- c("loc5.infant", "loc5.ychild", "loc5.teen", "loc5.yadult", "loc5.madult", "loc5.oadult") 
    loc6_loop <- c("loc6.infant", "loc6.ychild", "loc6.teen", "loc6.yadult", "loc6.madult", "loc6.oadult") 
    loc7_loop <- c("loc7.infant", "loc7.ychild", "loc7.teen", "loc7.yadult", "loc7.madult", "loc7.oadult") 
    loc8_loop <- c("loc8.infant", "loc8.ychild", "loc8.teen", "loc8.yadult", "loc8.madult", "loc8.oadult") 
    loc9_loop <- c("loc9.infant", "loc9.ychild", "loc9.teen", "loc9.yadult", "loc9.madult", "loc9.oadult") 
    
    #Recode all the location variables such that if there are NAs they get transformed to zeros 
    #long_data4[,c(33:41, 43:51, 53:61, 63:71, 73:81, 83:91)][is.na(long_data4[,c(33:41, 43:51, 53:61, 63:71, 73:81, 83:91)])] <- 0
    #length(which(is.na(data_may$loc5.ychild)))
    
    group <- seq(1, 6, 1)
    #Create an array where each array is a different location (loc1 -> loc9), and each cell corresponds to the avg # contacts
    # between people of the 6 different age categories in each location 
    matrix_contact_loc <- array(NA, dim = c(nrow = 6, ncol=6, 9))
    #group <- c("infants", "ychild", "teens", "yadult", "madult", "oadult")
    
    #Loop of mean # of contacts per location
    for (i in 1:6) { #outer- filter by age category 
      dta_filt <- data %>% #first create a filtered data frame where you include individuals across each age group 
        filter(age_cat2 == group[i]) #i
      
      for (j in 1:6){
        matrix_contact_loc[j,i,1] = mean(dta_filt[[loc1_loop[j]]], na.rm = T)#among infants, how many had contacts with other infants in locations 1 - THEN repeat thisi for locations 1-9
        matrix_contact_loc[j,i,2] = mean(dta_filt[[loc2_loop[j]]], na.rm = T)
        matrix_contact_loc[j,i,3] = mean(dta_filt[[loc3_loop[j]]], na.rm = T)
        matrix_contact_loc[j,i,4] = mean(dta_filt[[loc4_loop[j]]], na.rm = T)
        matrix_contact_loc[j,i,5] = mean(dta_filt[[loc5_loop[j]]], na.rm = T)
        matrix_contact_loc[j,i,6] = mean(dta_filt[[loc6_loop[j]]], na.rm = T)
        matrix_contact_loc[j,i,7] = mean(dta_filt[[loc7_loop[j]]], na.rm = T)
        matrix_contact_loc[j,i,8] = mean(dta_filt[[loc8_loop[j]]], na.rm = T)
        matrix_contact_loc[j,i,9] = mean(dta_filt[[loc9_loop[j]]], na.rm = T)
      }
    }
    
    array <- array(NA, dim = c(nrow=6, ncol=6, 9))
    array[,,1] <- matrix_contact_loc[,,1]; array[,,2] <- matrix_contact_loc[,,2]; array[,,3] <- matrix_contact_loc[,,3]; array[,,4] <- matrix_contact_loc[,,4]; array[,,5] <-matrix_contact_loc[,,5]
    array[,,6] <- matrix_contact_loc[,,6]; array[,,7] <- matrix_contact_loc[,,7]; array[,,8] <- matrix_contact_loc[,,8]; array[,,9] <- matrix_contact_loc[,,9]
    return(array)
}

### RUN EITHER data_aug or data_may here!! 
#array <- function_locmatrix(data_aug)
#array <- function_locmatrix(data_may) #only run 1 at a time
array <- function_locmatrix(data_feb) #only run 1 at a time

## 2. extract each array into a data frame ####
mean_loc1 <- data.frame(array[,,1])
mean_loc2 <- data.frame(array[,,2]) #Work
mean_loc3 <- data.frame(array[,,3])
mean_loc4 <- data.frame(array[,,4]) #Childcare
mean_loc5 <- data.frame(array[,,5])
mean_loc6 <- data.frame(array[,,6])
mean_loc7 <- data.frame(array[,,7]) #Transit 
mean_loc8 <- data.frame(array[,,8])
mean_loc9 <- data.frame(array[,,9])


#### 3. Create function for melted location data frames #### 

function_melt <- function(data){
  group_des <- c("infant", "ychild", "teen", "yadult", "madult", "oadult")
  data_long <- melt(data)
  data_long$variable <- c(rep("infant", 6), rep("ychild", 6), rep("teen", 6), rep("yadult", 6), rep("madult", 6), rep("oadult", 6))
  names(data_long)[1] <- "respondent"
  data_long$contact <- rep(group_des, 6) #age2= contact, age1 = respondent

  data_long$respondent_code <- ifelse(data_long$respondent == "infant", 1, 
                                 ifelse(data_long$respondent == "ychild", 2, 
                                        ifelse(data_long$respondent == "teen", 3,
                                               ifelse(data_long$respondent == "yadult", 4,
                                                      ifelse(data_long$respondent == "madult", 5,6)))))
  
  data_long$contact_code <- ifelse(data_long$contact == "infant", 1, 
                              ifelse(data_long$contact == "ychild", 2, 
                                     ifelse(data_long$contact == "teen", 3,
                                            ifelse(data_long$contact == "yadult", 4,
                                                   ifelse(data_long$contact == "madult", 5,6)))))
  return(data_long)
}

long_loc1 <- function_melt(mean_loc1);long_loc1$location <- c(rep("Home", nrow(long_loc1))) 
long_loc2 <- function_melt(mean_loc2);long_loc2$location <- c(rep("Work", nrow(long_loc2))) 
long_loc3 <- function_melt(mean_loc3);long_loc3$location <- c(rep("Someone else's home", nrow(long_loc3))) 
long_loc4 <- function_melt(mean_loc4);long_loc4$location <- c(rep("Child care", nrow(long_loc4))) 
long_loc5 <- function_melt(mean_loc5);long_loc5$location <- c(rep("Outdoor Leisure", nrow(long_loc5))) 
long_loc6 <- function_melt(mean_loc6);long_loc6$location <- c(rep("Essential Activities", nrow(long_loc6))) 
long_loc7 <- function_melt(mean_loc7);long_loc7$location <- c(rep("Public Transit", nrow(long_loc7))) 
long_loc8 <- function_melt(mean_loc8);long_loc8$location <- c(rep("Child's school", nrow(long_loc8))) 
long_loc9 <- function_melt(mean_loc9);long_loc9$location <- c(rep("Other", nrow(long_loc9))) 

#Bind data frames together
long_loc_aug <- rbind(long_loc1, long_loc2, long_loc3, long_loc4, long_loc5, 
                      long_loc6, long_loc7, long_loc8, long_loc9)
#save(long_loc_aug, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/long_loc_aug.Rdata")

long_loc_may <- rbind(long_loc1, long_loc2, long_loc3, long_loc4, long_loc5, 
                      long_loc6, long_loc7, long_loc8, long_loc9)
#save(long_loc_may, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/long_loc_may.Rdata")

long_loc_feb <- rbind(long_loc1, long_loc2, long_loc3, long_loc4, long_loc5, 
                      long_loc6, long_loc7, long_loc8, long_loc9)
#save(long_loc_may, file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/long_loc_feb.Rdata")

#### 4. Create plot ####

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))


plot_loc_ns_may <- ggplot(long_loc_may, aes(x = respondent_code, y = contact_code, fill = value)) + theme(legend.position = "bottom") + 
  geom_tile() + 
  scale_fill_gradientn(colours = myPalette(7), name = "Contacts \n per Person \n per day", 
                       limits = c(0, 3)) + 
  facet_wrap(~location) +
  theme_bw() +
  scale_x_continuous("Age of individual", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +
  scale_y_continuous("Age of contact", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +  
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        text = element_text(size = 10)) + 
  ggtitle("May")
#Remove legend from may
plot_loc_ns_may2 <- plot_loc_ns_may + theme(legend.position="none") +  coord_equal()

plot_loc_ns_aug <- ggplot(long_loc_aug, aes(x = respondent_code, y = contact_code, fill = value)) + theme(legend.position = "bottom") + 
  geom_tile() + scale_fill_gradientn(colours = myPalette(7), name = "Contacts \n per Person \n per day", 
                                     limits = c(0, 3)) + 
  facet_wrap(~location) +
  theme_bw() +
  scale_x_continuous("Age of individual", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +
  scale_y_continuous("Age of contact", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +  
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        text = element_text(size = 10)) +
  ggtitle("August")
plot_loc_ns_aug
plot_loc_ns_aug <- plot_loc_ns_aug + coord_equal()

plot_loc_ns_feb <- ggplot(long_loc_feb, aes(x = respondent_code, y = contact_code, fill = value)) + theme(legend.position = "bottom") + 
  geom_tile() + scale_fill_gradientn(colours = myPalette(7), name = "Contacts \n per Person \n per day", 
                                     limits = c(0, 3)) + 
  facet_wrap(~location) +
  theme_bw() +
  scale_x_continuous("Age of individual", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +
  scale_y_continuous("Age of contact", breaks = c(1,2,3,4,5,6),
                     labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +  
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        text = element_text(size = 10)) +
  ggtitle("February")
plot_loc_ns_feb
plot_loc_ns_feb <- plot_loc_ns_feb + coord_equal()


prow <- plot_grid(
  plot_loc_ns_may + theme(legend.position="none"),
  plot_loc_ns_aug + theme(legend.position="none"),
  align = 'vh',
  #hjust = -1,
  nrow = 1, 
  rel_widths = c(1, 1))
prow

# extract a legend that is laid out horizontally
legend_b <- get_legend(
  plot_loc_ns_may +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
fig2 <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))
fig2

##### NEW: Create location matrices with 1 row per location, each column  = study wave 
long_loc_feb$period <- "February 2021"
long_loc_may$period <- "May 2020"
long_loc_aug$period <- "August 2020"

long_loc <- rbind(long_loc_feb, long_loc_may, long_loc_aug)
long_loc$period <- factor(long_loc$period, levels = c("May 2020", "August 2020", "February 2021"))

plotFunction <- function(data, var){
  dataFilt <- data %>% filter(location == var)
  plot <- ggplot(dataFilt, aes(x = respondent_code, y = contact_code, fill = value)) + theme(legend.position = "bottom") + 
    geom_tile() + 
    scale_fill_gradientn(colours = myPalette(7), name = "Contacts \n per Person \n per day", 
                         limits = c(0, 3)) + 
    facet_wrap(~period) +
    theme_bw() +
    scale_x_continuous("Age of individual", breaks = c(1,2,3,4,5,6),
                       labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +
    scale_y_continuous("Age of contact", breaks = c(1,2,3,4,5,6),
                       labels = c("0-4", "5-12", "13-17", "18-39", "40-64", "65+")) +  
    theme(axis.text.x = element_text(angle = 60, hjust = 1), 
          text = element_text(size = 10)) + 
          #strip.text.x = element_blank(), 
          # axis.title.x=element_blank(),
          # axis.title.y=element_blank()) 
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank()) 
    ggtitle(var)
  plot <- plot + coord_equal()
  return(plot)}

unique(long_loc$location)
plotHome <- plotFunction(long_loc, "Home")
plotWork <- plotFunction(long_loc, "Work")
plotOtherHome <- plotFunction(long_loc, "Someone else's home")
plotchildCare <- plotFunction(long_loc, "Child care")
plotOutdoor<- plotFunction(long_loc, "Outdoor Leisure")
plotEssential <- plotFunction(long_loc, "Essential Activities")
plotTransit <- plotFunction(long_loc, "Public Transit")
plotSchool <- plotFunction(long_loc, "Child's school")
plotOther <- plotFunction(long_loc, "Other")

prow <- plot_grid(
  plotHome + theme(legend.position="none"),
  plotWork + theme(legend.position="none"),
  plotEssential + theme(legend.position="none"),
  plotSchool + theme(legend.position="none"),
  # plotOtherHome + theme(legend.position="none"),
  # plotchildCare + theme(legend.position="none"),
  # plotOutdoor + theme(legend.position="none"),
  # plotEssential + theme(legend.position="none"),
  # plotTransit + theme(legend.position="none"),
  # plotSchool + theme(legend.position="none"),
  # plotOther + theme(legend.position="none"),
  align = 'vh',
  #hjust = -1,
  nrow = 2, 
  rel_widths = c(1, 1))
prow

# extract a legend that is laid out horizontally
legend_b <- get_legend(
  plotHome +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
fig2 <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))
fig2


## Figure S1. Distribution of total contacts by wave- histogram
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

load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_feb.Rdata")
data_feb$total_contacts <- ifelse(data_feb$total_contacts >29, 29, data_feb$total_contacts) #Top code contacts at 29

data_may$type <- "Wave 1"
data_aug$type <- "Wave 2"
data_feb$type <- "Wave 3"

may_select <- data_may %>% dplyr::select(type, total_contacts)
aug_select <- data_aug %>% dplyr::select(type, total_contacts)
feb_select <- data_feb %>% dplyr::select(type, total_contacts)

hist_data <- rbind(may_select, aug_select, feb_select)

# may_select_child <- long_data4 %>% dplyr::select(type, total_contacts, child) #USE this code if yout want histograms with only child contacts 
# aug_select_child <- data_aug %>% dplyr::select(type, total_contacts, child)
# hist_data_child <- rbind(may_select_child, aug_select_child)
# hist_data_child <- hist_data_child %>% filter(child != "child0") %>% dplyr::select(type, total_contacts)

#head(hist_data); tail(hist_data)
library(plyr)
mu <- ddply(hist_data, "type", plyr::summarise, grp.mean = mean(total_contacts))
med <- ddply(hist_data, "type", plyr::summarise, grp.med = median(total_contacts))

# mu_child <- ddply(hist_data_child, "type", plyr::summarise, grp.mean = mean(total_contacts)) #USE this code if yout want histograms with only child contacts 
# med_child <- ddply(hist_data_child, "type", plyr::summarise, grp.med = median(total_contacts))

#hist_data$total_contacts <- ifelse(hist_data$total_contacts >20, 20, hist_data$total_contacts)
#hist_data_child$total_contacts <- ifelse(hist_data_child$total_contacts >20, 20, hist_data_child$total_contacts)

#hist_data$type <- factor(hist_data$type, levels = c("May 2020", "August 2020","February 2021" ))

figS1 <- ggplot(transform(hist_data, type = factor(type, levels = c("Wave 1", "Wave 2", "Wave 3")))) + 
  geom_histogram(aes(total_contacts), bins = 21) + 
  facet_grid(type ~ . ,scale = "free_y") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=type),
             linetype="solid") + 
  geom_vline(data = med, aes(xintercept = grp.med, color = type), 
             linetype = "dotted") + 
  scale_x_continuous(breaks = c(seq(0, 29, 2))) + 
  #ylim(0, 855) + 
  #scale_y_continuous(limits = c(0, 180)) + 
  theme_minimal()+
  labs(x = "Number of non-household Contacts" , y = "Density") +
  theme(legend.position = "none", 
        text = element_text(size = 16), 
        axis.text.x = element_text(size  = 16)) 
figS1

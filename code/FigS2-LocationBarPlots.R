## Figure S2. Location Bar Plots 
## Last updated: 11-01-20
## Kristin Andrejko

#### 0. Load Data ####
#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/long_data4.rda")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_may.rda") #new may data file that corrects issue in child ID
data_may <- long_data4 
data_may$total_contacts <- ifelse(data_may$total_contacts >29, 29, data_may$total_contacts) #Top code contacts at 29
#data_aug <- data_may

#load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/data/data_aug.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_aug.Rdata")
data_aug$total_contacts <- ifelse(data_aug$total_contacts >29, 29, data_aug$total_contacts) #Top code contacts at 29

load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/final-code/data_feb.Rdata")
data_feb$total_contacts <- ifelse(data_feb$total_contacts >29, 29, data_feb$total_contacts) #Top code contacts at 29
data_aug <- data_feb

#1. Create a new dataframe that groups all of the locx together into one variable locx

data_aug$loc1 <- c(rep(NA, nrow(data_aug)))
data_aug$loc2 <- c(rep(NA, nrow(data_aug)))
data_aug$loc3 <- c(rep(NA, nrow(data_aug)))
data_aug$loc4 <- c(rep(NA, nrow(data_aug)))
data_aug$loc5 <- c(rep(NA, nrow(data_aug)))
data_aug$loc6 <- c(rep(NA, nrow(data_aug)))
data_aug$loc7 <- c(rep(NA, nrow(data_aug)))
data_aug$loc8 <- c(rep(NA, nrow(data_aug)))
data_aug$loc9 <- c(rep(NA, nrow(data_aug)))

for (i in 1:nrow(data_aug)){
  data_aug$loc1[i] <- sum(data_aug$loc1.infant[i], data_aug$loc1.ychild[i], data_aug$loc1.teen[i],
                          data_aug$loc1.yadult[i], data_aug$loc1.madult[i], data_aug$loc1.oadult[i], na.rm = T)
  data_aug$loc2[i] <- sum(data_aug$loc2.infant[i], data_aug$loc2.ychild[i], data_aug$loc2.teen[i],
                          data_aug$loc2.yadult[i], data_aug$loc2.madult[i], data_aug$loc2.oadult[i], na.rm = T)
  data_aug$loc3[i] <- sum(data_aug$loc3.infant[i], data_aug$loc3.ychild[i], data_aug$loc3.teen[i],
                          data_aug$loc3.yadult[i], data_aug$loc3.madult[i], data_aug$loc3.oadult[i], na.rm = T)
  data_aug$loc4[i] <- sum(data_aug$loc4.infant[i], data_aug$loc4.ychild[i], data_aug$loc4.teen[i],
                          data_aug$loc4.yadult[i], data_aug$loc4.madult[i], data_aug$loc4.oadult[i], na.rm = T)
  data_aug$loc5[i] <- sum(data_aug$loc5.infant[i], data_aug$loc5.ychild[i], data_aug$loc5.teen[i],
                          data_aug$loc5.yadult[i], data_aug$loc5.madult[i], data_aug$loc5.oadult[i], na.rm = T)
  data_aug$loc6[i] <- sum(data_aug$loc6.infant[i], data_aug$loc6.ychild[i], data_aug$loc6.teen[i],
                          data_aug$loc6.yadult[i], data_aug$loc6.madult[i], data_aug$loc6.oadult[i], na.rm = T)
  data_aug$loc7[i] <- sum(data_aug$loc7.infant[i], data_aug$loc7.ychild[i], data_aug$loc7.teen[i],
                          data_aug$loc7.yadult[i], data_aug$loc7.madult[i], data_aug$loc7.oadult[i], na.rm = T)
  data_aug$loc8[i] <- sum(data_aug$loc8.infant[i], data_aug$loc8.ychild[i], data_aug$loc8.teen[i],
                          data_aug$loc8.yadult[i], data_aug$loc8.madult[i], data_aug$loc8.oadult[i], na.rm = T)
  data_aug$loc9[i] <- sum(data_aug$loc9.infant[i], data_aug$loc9.ychild[i], data_aug$loc9.teen[i],
                          data_aug$loc9.yadult[i], data_aug$loc9.madult[i], data_aug$loc9.oadult[i], na.rm = T)
}
names(data_aug)


### NEW: CONFIDENCE INTERVALS FOR BAR PLOT OF MEAN NO. CONTACTS/ LOCATION ###

# Re create loop with each location as an array (9 arrays), 1 row per age gropu, 3 columns (mean, prop low, prop high)
matrix_loc <- array(NA, dim = c(nrow = 6, ncol = 5, 9)) #each row is an age group, each column is a location 
matrix_loc
n <- 1e4 
group <- seq(1, 6, 1)
for (i in 1:length(group)){
  dta_filt <- data_aug %>%  
    filter(age_cat2 == group[i]) 
  
  matrix_loc[i,1, 1] = mean(dta_filt$loc1, na.rm = T)
  #Bootstrap
  bstrap <- c()
  for (k in 1:n){
    bsample <- sample(dta_filt$loc1, n, replace = T)
    
    bestimate <- mean(bsample)
    bstrap <- c(bstrap, bestimate)}
  matrix_loc[i,2, 1] = quantile(bstrap, 0.025)
  matrix_loc[i,3, 1] = quantile(bstrap, 0.975)
  matrix_loc[i,4, 1] = 1
  matrix_loc[i,5, 1] = i
  
  matrix_loc[i,1, 2] = mean(dta_filt$loc2, na.rm = T)
  #Bootstrap
  bstrap <- c()
  for (k in 1:n){
    bsample <- sample(dta_filt$loc2, n, replace = T)
    
    bestimate <- mean(bsample)
    bstrap <- c(bstrap, bestimate)}
  matrix_loc[i,2, 2] = quantile(bstrap, 0.025)
  matrix_loc[i,3, 2] = quantile(bstrap, 0.975)
  matrix_loc[i,4, 2] = 2
  matrix_loc[i,5, 2] = i
  
  matrix_loc[i,1, 3] = mean(dta_filt$loc3, na.rm = T)
  #Bootstrap
  bstrap <- c()
  for (k in 1:n){
    bsample <- sample(dta_filt$loc3, n, replace = T)
    
    bestimate <- mean(bsample)
    bstrap <- c(bstrap, bestimate)}
  matrix_loc[i,2, 3] = quantile(bstrap, 0.025)
  matrix_loc[i,3, 3] = quantile(bstrap, 0.975)
  matrix_loc[i,4, 3] = 3
  matrix_loc[i,5, 3] = i
  
  matrix_loc[i,1, 4] = mean(dta_filt$loc4, na.rm = T)
  #Bootstrap
  bstrap <- c()
  for (k in 1:n){
    bsample <- sample(dta_filt$loc4, n, replace = T)
    
    bestimate <- mean(bsample)
    bstrap <- c(bstrap, bestimate)}
  matrix_loc[i,2, 4] = quantile(bstrap, 0.025)
  matrix_loc[i,3, 4] = quantile(bstrap, 0.975)
  matrix_loc[i,4, 4] = 4
  matrix_loc[i,5, 4] = i
  
  matrix_loc[i,1, 5] = mean(dta_filt$loc5, na.rm = T)
  #Bootstrap
  bstrap <- c()
  for (k in 1:n){
    bsample <- sample(dta_filt$loc5, n, replace = T)
    
    bestimate <- mean(bsample)
    bstrap <- c(bstrap, bestimate)}
  matrix_loc[i,2, 5] = quantile(bstrap, 0.025)
  matrix_loc[i,3, 5] = quantile(bstrap, 0.975)
  matrix_loc[i,4, 5] = 5
  matrix_loc[i,5, 5] = i
  
  matrix_loc[i,1, 6] = mean(dta_filt$loc6, na.rm = T)
  #Bootstrap
  bstrap <- c()
  for (k in 1:n){
    bsample <- sample(dta_filt$loc6, n, replace = T)
    
    bestimate <- mean(bsample)
    bstrap <- c(bstrap, bestimate)}
  matrix_loc[i,2, 6] = quantile(bstrap, 0.025)
  matrix_loc[i,3, 6] = quantile(bstrap, 0.975)
  matrix_loc[i,4, 6] = 6
  matrix_loc[i,5, 6] = i
  
  matrix_loc[i,1, 7] = mean(dta_filt$loc7, na.rm = T)
  #Bootstrap
  bstrap <- c()
  for (k in 1:n){
    bsample <- sample(dta_filt$loc7, n, replace = T)
    
    bestimate <- mean(bsample)
    bstrap <- c(bstrap, bestimate)}
  matrix_loc[i,2, 7] = quantile(bstrap, 0.025)
  matrix_loc[i,3, 7] = quantile(bstrap, 0.975)
  matrix_loc[i,4, 7] = 7
  matrix_loc[i,5, 7] = i
  
  matrix_loc[i,1, 8] = mean(dta_filt$loc8, na.rm = T)
  #Bootstrap
  bstrap <- c()
  for (k in 1:n){
    bsample <- sample(dta_filt$loc8, n, replace = T)
    
    bestimate <- mean(bsample)
    bstrap <- c(bstrap, bestimate)}
  matrix_loc[i,2, 8] = quantile(bstrap, 0.025)
  matrix_loc[i,3, 8] = quantile(bstrap, 0.975)
  matrix_loc[i,4, 8] = 8
  matrix_loc[i,5, 8] = i
  
  matrix_loc[i,1, 9] = mean(dta_filt$loc9, na.rm = T)
  #Bootstrap
  bstrap <- c()
  for (k in 1:n){
    bsample <- sample(dta_filt$loc9, n, replace = T)
    
    bestimate <- mean(bsample)
    bstrap <- c(bstrap, bestimate)}
  matrix_loc[i,2, 9] = quantile(bstrap, 0.025)
  matrix_loc[i,3, 9] = quantile(bstrap, 0.975)
  matrix_loc[i,4, 9] = 9
  matrix_loc[i,5, 9] = i
  
}
matrix_loc #some NaN produced 

df_loc1 <- data.frame(matrix_loc[,,1]); df_loc1 
df_loc2 <- data.frame(matrix_loc[,,2]); df_loc2
df_loc3 <- data.frame(matrix_loc[,,3]); df_loc3
df_loc4 <- data.frame(matrix_loc[,,4]); df_loc4
df_loc5 <- data.frame(matrix_loc[,,5]); df_loc5
df_loc6 <- data.frame(matrix_loc[,,6]); df_loc6
df_loc7 <- data.frame(matrix_loc[,,7]); df_loc7
df_loc8 <- data.frame(matrix_loc[,,8]); df_loc8
df_loc9 <- data.frame(matrix_loc[,,9]); df_loc9


df_loc <- rbind(df_loc1, df_loc2, df_loc3, df_loc4, df_loc5, df_loc6, df_loc7, df_loc8, df_loc9); df_loc
names(df_loc)[1] <- "mean"
names(df_loc)[2] <- "ci_lb"
names(df_loc)[3] <- "ci_ub"
names(df_loc)[4] <- "loc"
names(df_loc)[5] <- "age"
head(df_loc) #Pull this off so we can can compare august vs may data

# Create a new variable with your desired order.
df_loc3 <- df_loc %>% 
  group_by(age) %>% 
  mutate(mean_order = rank(mean))
#View(df_loc3) #THIS WORKED 

# df_loc_feb <- df_loc3
# save(df_loc_feb, 
# file = "/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/df_location_feb.rda")

#Then plot
plot_loc <- ggplot(df_loc3, 
                   aes(x = age, y = mean, fill = as.factor(loc), group = mean_order)) + 
  geom_bar(stat = "identity",  position="dodge") + 
  theme_minimal() + 
  theme(axis.text=element_text(size=16, color = 'black'),
        axis.title=element_text(size=18), 
        legend.text = element_text(size=16, color = 'black'), 
        legend.title = element_text(size=18), 
        axis.line = element_line(colour = 'black', size = 0.5), 
        panel.grid.major = element_blank()) +
  geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub, x= age), width = 0.4, size = 0.5, 
                position=position_dodge(.9), show.legend = FALSE) +
  scale_fill_discrete(name = "", labels = c("Home", "Work", "Someone else's home",
                                            "Childcare", "Outdoor leisure", "Essential activities",
                                            "Public transit", "School", "Other")) +
  scale_x_discrete(expand = c(0,0),limits=c(1, 2, 3, 4, 5, 6), labels = c("0-4", "5-12", "13-17",
                                                                          "18-39", "40-64", "65+")) +
  scale_y_discrete(expand = c(0,0),limits=c(0, 0.5, 1, 1.5, 2, 2.5, 3)) +
  labs(y = "Mean Number of Contacts", x = "Age") +
  scale_fill_brewer(palette = "Paired", labels = c("Home", "Work", "Someone else's home",
                                                   "Childcare", "Outdoor leisure", "Essential activities",
                                                   "Public transit", "School", "Other"), name = "") 
plot_loc

## NOW SAVE AUG DATA AND REPEAT FOR MAY 
plot_loc_aug <- plot_loc
plot_loc_may <- plot_loc
plot_loc_feb <- plot_loc

#Remove legend from may
plot_loc_may2 <- plot_loc_may + theme(legend.position="none")
plot_loc_aug2 <- plot_loc_aug + theme(legend.position="bottom")
plot_loc_feb2 <- plot_loc_feb+ theme(legend.position="bottom")

# Code to draw a plot
plot_loc_may2
plot_loc_aug2

plot2 <- ggdraw() +
  draw_plot(plot_loc_may2, x = 0, y = 0.62, width = 1, height = .3) +#x y width height
  draw_plot(plot_loc_aug2, x = 0, y = 0, width = 1, height = .6) +
  draw_plot_label(c("May", "August"), c(0.05, 0.05), c(1, 0.59), size = 10)
#plot2


### TRY NEW PLOT
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/df_location_feb.rda")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/df_location_aug.rda")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/df_location_may.rda")
df_loc_may <- df_loc

#stack DF 
df_loc_may$mean_order <- NA
df_loc_may$wave <- "Wave 1"
df_loc_aug$wave <- "Wave 2"
df_loc_feb$wave <- "Wave 3"

df_loc <- rbind(df_loc_may, data.frame(df_loc_aug), data.frame(df_loc_feb))

loc.labs <- c("Home", "Work", "Someone else's home","Childcare", "Outdoor leisure", 
              "Essential activities","Public transit", "School", "Other")
  
df_loc$loc <- factor(df_loc$loc, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                     labels = c("Home", "Work", "Someone else's home","Childcare", "Outdoor leisure", 
                                "Essential activities","Public transit", "School", "Other"))
  
plot_loc <- ggplot(df_loc, 
                   aes(x = age, y = mean, fill = as.factor(wave), group = wave)) + #removed group= mean_order
  geom_bar(stat = "identity",  position="dodge") + 
  theme_minimal() +
  facet_wrap(~loc, scales = 'free') + 
  theme(axis.text=element_text(size=8, color = 'black'),
        axis.title=element_text(size=11), 
        legend.text = element_text(size=8, color = 'black'), 
        legend.title = element_text(size=8), 
        axis.line = element_line(colour = 'black', size = 0.5))+ 
  geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub, x= age), width = 0.4, size = 0.5, 
                position=position_dodge(.9), show.legend = FALSE) +
  scale_fill_discrete(name = "", labels = c("Wave 1", "Wave 2", "Wave 3")) +
  scale_x_discrete(expand = c(0,0),limits=c(1, 2, 3, 4, 5, 6), labels = c("0-4", "5-12", "13-17",
                                                                          "18-39", "40-64", "65+")) +
  #scale_y_discrete(expand = c(0,0),limits=c(0, 0.5, 1, 1.5, 2, 2.5)) +
  scale_y_continuous(expand = c(0,0),limits=c(0, 2.7)) +
  labs(y = "Mean Number of Contacts", x = "Age") +
  scale_fill_brewer(palette = "GnBu", name = "")
plot_loc

ggsave("final-code/figS2.jpg", dpi = 600, width = 7.5, height = 6)


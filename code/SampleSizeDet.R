#Power simulation for BAY AREA WAVE 2 of DATA COLLECTION
library(dplyr)
N.sims <- 1000
N <-  seq(300,1200,50) #Total number of people 
b2 <- seq(-3, 3, 0.25) #These end up being columns of the matrix (coef on AA variable)
p.values <- array(NA, dim=c(N.sims, length(b2), length(N)))

tic <- proc.time()

load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/race_income_pop.Rdata")
load("/Users/kristinandrejko/Box/01-Research/04-COVID/School-Survey/output/race_income_hisp.Rdata")

for (j in 1:length(N)){
  
  #Generate exposure data
    race <- c(rep(0, round((N[13] * 0.4006101), 0)), #White only
              rep(1, round((N[13] * 0.1001525), 0)), #African american
              rep(2, round((N[13] * 0.200305), 0)), #Hispanic
              rep(3, round((N[13] * 0.2989324), 0))) #Asian

    white <- race_income_pop %>% 
      filter(race == 1)
    white_prop <- white$n_pop / sum(white$n_pop)
    
    white_length <- length(which(race == 0))
    income_white <- c(rep(10000, round((white_length * white_prop[1]), 0)), 
                     rep(30000, round((white_length*  white_prop[2]), 0)), 
                     rep(50000, round((white_length*  white_prop[3]), 0)),
                     rep(70000, round((white_length*  white_prop[4]), 0)),
                     rep(90000, round((white_length*  white_prop[5]), 0)),
                     rep(125000, round((white_length* white_prop[6]), 0)),
                     rep(150000, round((white_length* white_prop[7]), 0)))
    

    black <- race_income_pop %>% 
      filter(race == 2)
    black_prop <- black$n_pop / sum(black$n_pop)
    black_length <- length(which(race == 1))
    
    income_black <- c(rep(10000, (black_length * black_prop[1])), 
                      rep(30000, (black_length*  black_prop[2])), 
                      rep(50000, (black_length*  black_prop[3])),
                      rep(70000, (black_length*  black_prop[4])),
                      rep(90000, (black_length*  black_prop[5])),
                      rep(125000,(black_length* black_prop[6])),
                      rep(150000,(black_length* black_prop[7])))
      
    hispanic <- race_income_hisp %>%  
      filter(hispanic == 1)
    hispanic_prop <- hispanic$n_pop / sum(hispanic$n_pop)
    hispanic_length <- length(which(race == 2))
    
    income_hispanic <- c(rep(10000, round((hispanic_length * hispanic_prop[1]), 0)), 
                      rep(30000, round((hispanic_length*  hispanic_prop[2]), 0)), 
                      rep(50000, round((hispanic_length*  hispanic_prop[3]), 0)),
                      rep(70000, round((hispanic_length*  hispanic_prop[4]), 0)),
                      rep(90000, round((hispanic_length*  hispanic_prop[5]), 0)),
                      rep(125000, round((hispanic_length* hispanic_prop[6]), 0)),
                      rep(150000, round((hispanic_length* hispanic_prop[7]), 0)))
    
    asian <- race_income_pop %>% 
      filter(race == 6)
    asian_prop <- asian$n_pop / sum(asian$n_pop)
    asian_length <- length(which(race == 3))
    
      income_asian <- c(rep(10000, round((asian_length * asian_prop[1]), 0)), 
                        rep(30000, round((asian_length*  asian_prop[2]), 0)), 
                        rep(50000, round((asian_length*  asian_prop[3]), 0)),
                        rep(70000, round((asian_length*  asian_prop[4]), 0)),
                        rep(90000, round((asian_length*  asian_prop[5]), 0)),
                        rep(125000, round((asian_length* asian_prop[6]), 0)),
                        rep(150000, round((asian_length* asian_prop[7]), 0)))
      
    income <- c(income_white, income_black, income_hispanic, income_asian)
    
    #Update race vector
    race_updated <- c(rep(0, length(income_white)), #White only
              rep(1,length(income_black)), #African american
              rep(2, length(income_hispanic)), #Hispanic
              rep(3, length(income_asian))) #Asiam
    length(income); length(race_updated) #make sure these are even lengths 
    
    #Calculate probability of outcome (# contacts) given exposure 
    
    #Model coefficients: 
    set.seed(252)
        b1 <- 3.5 #intercept 
        #b2 <- rnorm(length(race_updated), mean = -1.5, sd = 2) #black
        b3 <- rnorm(length(race_updated), mean = 2, sd = 2) #hisp
        b4 <- rnorm(length(race_updated), mean = -0.77, sd = 1) #asian 
        b5 <- rnorm(length(race_updated), mean = -1e-5, sd = 1e-6) #income (continuous var)
    
    for (k in 1:length(b2)){  
      for (i in 1:N.sims){
      b2.new <- rnorm(length(race_updated), mean = b2[k], sd = 2) 
      p.contact <- b1 + b2.new*as.numeric(race_updated==1) + b3*as.numeric(race_updated==2) + b4*as.numeric(race_updated==3) + b5*income
      
      #adjust for fact that p.contact shouldn't be <0 
      #p.contact <- ifelse(p.contact <0, 0, p.contact)
      
    #Fit a model 
      fit.glm <- glm(p.contact  ~ as.factor(race_updated) + income)
      
      #Obtain p values 
      p.values[i, k, j] <- coef(summary(fit.glm))[,4][2] #collecting p value on AA variable
      
      } #closes i (n.sims)
    } #closes k (b2)
} #closes J (N)

toc <- proc.time()
toc - tic

dim(p.values)  #each row = N.sim, each col = different values for b2 (black coef), array = N value 
p.values[,,1]
#Calculate power 
power <- matrix(NA, nrow = length(N), ncol = length(b2))
  for (p in 1:length(N)){
    power[p,] <- apply(matrix(as.integer(p.values[,,p]<0.05), nrow=N.sims, ncol=length(b2)),2,mean)
  }
power #where rows are different N sizes, and columns are different b2 values 
  dim(power); length(N); length(b2)

#Ideal sample size for Bay Area 900 (N[13] or 700 N[9]
  race_bay <- c(rep(0, round((N[9] * 0.4006101), 0)), #White only
            rep(1, round((N[9] * 0.1001525), 0)), #African american
            rep(2, round((N[9] * 0.200305), 0)), #Hispanic
            rep(3, round((N[9] * 0.2989324), 0))) #Asian
  
  length(which(race_bay == 0))
  length(which(race_bay == 1))
  length(which(race_bay == 2))
  length(which(race_bay == 3))

  vec <- c(10000, 30000, 50000, 70000, 90000, 125000, 150000)
  totals <- c()
  
  for (i in 1:length(vec)){
    totals[i] <- sum(income_hispanic == vec[i]) #income_white, income_hispanic, income_black, income_asian
  }
totals; sum(totals)
    
    
    
# N[11]= 800
# N[9] = 700


    
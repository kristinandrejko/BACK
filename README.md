## Longitudinal social-contact patterns among school-aged children during the COVID-19 pandemic: the Bay Area Contacts among Kids (BACK) study

### Link to pre-print: *coming soon*

### Contact matrices
Navigate to `contact matrices` folder to download both community and household contact matrices for each wave of the study. 
Notes: 
+ Wave One in the pre-print corresponds to "May" files; Wave Two = "Aug"l Wave Three = "Feb"
+ `contact matrices/community`: Community contact matrices provide median (`med`), 2.5% (`lb`), and 97.5% (`ub`) across 10,000 bootstrapped estimates of average community contacts
+ `contact matrices/community/location`: Location contact matrices include community contacts only for each of the 8 locations listed in the manuscript: home, childcare, essential activities, someone else's home, outdoor leisure,school, public transit, or work. 
+ Contact matrices are provided in matrix form for 6 age bands: 0-4, 5-12, 13-17, 18-39, 40-64, 65+
   + The top left corner of each matrix [1,1] corresponds to average rate 0-4 year olds (survey participant) contacted other 0-4 year olds  
   + Ages of the surveyed individual are listed in columns, Ages of their contact are listed in rows 
      + [,1] represents average contacts for 0-4 year olds surveyed
      + [,2] represents average contacts for 5-12 year olds surveyed
      + [,3] represents average contacts for 13-17 year olds surveyed
      + [,4] represents average contacts for 18-39 year olds surveyed
      + [,5] represents average contacts for 40-64 year olds surveyed
      + [,6] represents average contacts for 65 + year olds surveyed


### Code Files: 
+ **Fig1-CommunityContactMatrix.R**
    + Creates community contact matrices displayed in Figure 1, output can also be found via `contact matrices/community`
+ **Fig2-LocationContactMatrix.R**
    + Creates community contact matrices stratified by location displayed in Figure3 2, output can also be found via  `contact matrices/community/location`
+ **Fig4-REstimation-Code.R**
    + Code for R_e estimation, including code to generate for Figure 4
+ **FigS2-HistogramOfContacts.R**
    + Code to generate Figure S2: histogram of total contacts by study wave 
+ **FigS3-LocationBarPlots.R**
    + Code to generate Figure S3: Average number of non-household contacts stratified by location of contact, age category, and study wave.
+ **Tb1-DistributionOfStudyParticipants.R**
    + Code to generate Table 1: Distribution of Study Participants
+ **Tb2-MeanContacts.R**
    + Code to generate bootstrapped estimates of the number of non-household contacts by demographic characteristics 
+ **Tb3-GeeModels.R** 
    + Code to run generalized estimating equation models, and creates plot for Figure 3


### Data Files: 
+ `data/R0 Calculation`: includes all relevant data files to run the estimation of R0 over each study wave
+ `data/survey data`: includes cleaned survey data from the web-based social contact survey administered across three time points: 
    + `/MayDataShare`: cleaned survey data from May 2020 (wave one) 
    + `/AugDataShare`: cleaned survey data from August 2020 (wave two) 
    + `/FebDataShare`: cleaned survey data from February 2021 (wave three) 
    + `/data_comb`: used to run GEE models, includes truncated data set of relevant variables from all three waves

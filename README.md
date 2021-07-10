## Longitudinal social-contact patterns among school-aged children during the COVID-19 pandemic: the Bay Area Contacts among Kids (BACK) study

### Link to pre-print: *coming soon*

### Contact matrices
Navigate to `contact matrices` folder to download both community and household contact matrices for each wave of the study. 
Notes: 
+ Wave One in the pre-print corresponds to "May" files; Wave Two = "Aug"l Wave Three = "Feb"
+ Community contact matrices provide median (`med`), 2.5% (`lb`), and 97.5% (`ub`) bootstrapped estimates of community contacts 
+ Location contact matrices include community contacts only for each of the 8 locations listed in the manuscript: home, childcare, essential activities, someone else's home, outdoor leisure,school, public transit, or work. 
+ Contact matrices are provided in matrix form for 6 age bands: 0-4, 5-12, 13-17, 18-39, 40-64, 65+
   + The top left corner of each matrix [1,1] corresponds to average rate of 0-4 year olds contacting other 0-4 year olds  

### Code Files: 
+ **Fig1-CommunityContactMatrix.R**
    + Creates community contact matrices displayed in Figure 1, output can also be found via `contact matrices -> community`
+ **Fig2-LocationContactMatrix.R**
    + Creates community contact matrices stratified by location displayed in Figure3 2, output can also be found via  `contact matrices -> community -> location`
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

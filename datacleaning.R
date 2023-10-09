
### "COVID-19 and Beyond for Singaporean Youth" (working title)
### Author: Jin Yao KWAN
### Data Cleaning

---
  title: "Data Cleaning"
author: "Jin Yao Kwan"
date: "2023-10-09"
output: 
  html_document: 
  keep_md: yes
---

#################################### Setup ####################################

### Check and set working directory
getwd()

### Install and load packages
if (!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("readxl")) {install.packages("readxl"); require("readxl")}
if (!require("psych")) {install.packages("psych"); require("psych")}
if (!require("GPArotation")) {install.packages("GPArotation"); require("GPArotation")}
if (!require("table1")) {install.packages("table1"); require("table1")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

### Import data from .xlsx
### Identifiers removed using Excel before import (e.g., phone, email, IP address, postal code)
data <- read_excel("data.xlsx")
view(data)
glimpse(data)

########################### Preliminary Exploration ###########################

### Dimensions (rows/columns), structure, and variable names
dim(data)
str(data)
names(data)

### Identify and remove duplicates
n_distinct(data)
n_distinct(data) == dim(data)
dim(distinct(data))
data <- distinct(data)

### Remove qualitative responses
data <- data %>%
  select(-selfcare_1) %>%
  select(-futureb_1) %>%
  select(-futured_1) %>%
  select (-civicatype_1) %>%
  select (-cworkb_1) %>%
  select (-covidc_1)

######################### (A) PSYCHOLOGICAL WELL-BEING #########################

levels(as.factor(data$burdena_1))
table(as.factor(data$burdena_1))

data <- data %>%
  ### Burdensomeness and belongingness
  mutate(
    across(burdena_1:burdenc_1, 
           ~ recode(.x, "1 - Not at all"=1, "2 - Very slightly"=2, "3 - Somewhat"=3, "4 - Moderately"=4, "5 - Much"=5, "6 - Very much"=6, "7 - Extremely"=7))) %>%
  ### Social trust
  mutate(
    across(strusta_1:strustc_1, 
           ~ recode(.x, "1 - Strongly disagree"=1, "2 - Disagree"=2, "3 - Slightly disagree"=3, "4 - Neither agree nor disagree"=4, "5 - Slightly agree"=5, "6 - Agree"=6, "7 - Strongly agree"=7))) %>%
  ### Depression and anxiety
  mutate( 
    across(anxdepa_1:anxdepp_1,
           ~ recode(.x, "1 - Never"=1, "2 - Almost never"=2, "3 - Sometimes"=3, "4 - Often"=4, "5 - Almost always"=5))) %>%
  ### Life satisfaction
  relocate(lifesatc_1, .after = lifesatg_1) %>% 
  relocate(lifesatd_1, .after = lifesatc_1) %>%
  mutate(
    across(lifesata_1:lifesatg_1,
           ~ recode(.x, "1 - Strongly disagree"=1, "2 - Moderately disagree"=2, "3 - Mildly disagree"=3, "4 - Mildly agree"=4, "5 - Moderately agree"=5, "6 - Strongly agree"=6))) %>%
  mutate(
    across(lifesatc_1:lifesatd_1,
           ~ recode(.x, "1 - Strongly disagree"=6, "2 - Moderately disagree"=5, "3 - Mildly disagree"=4, "4 - Mildly agree"=3, "5 - Moderately agree"=2, "6 - Strongly agree"=1)))

#################### (B) EMPLOYMENT AND EDUCATIONAL OUTLOOK ####################

data <- data %>%
  ### Economic stressors
  mutate(
    across(estressa_1:estressd_1,
           ~ recode(.x, "1 - Not at all"=1, "2 - Once or twice"=2, "3 - About once a month"=3, "4 - Several times a month"=4, "5 - Everyday"=5))) %>%
  ### Economic worry
  mutate(
    across(eworrya_1:eworrye_1,
           ~ recode(.x, "1 - Strongly disagree"=1, "2 - Disagree"=2, "3 - Neither agree nor disagree"=3, "4 - Agree"=4, "5 - Strongly agree"=5))) %>%
  ### Level of education/training + Confidence
  mutate(feduca_1 = recode(feduca_1, "'O' or 'N' level"=1, "'A' level / International Baccalaureate"=2, "ITE or equivalent"=3, "Professional certification"=4, "Diploma"=5, "Bachelor's degree (University degree)"=6, "Postgraduate degree (Master's or PhD)"=7)) %>%
  mutate(feducb_1 = recode(feducb_1, "'O' or 'N' level"=1, "'A' level / International Baccalaureate"=2, "ITE or equivalent"=3, "Professional certification"=4, "Diploma"=5, "Bachelor's degree (University degree)"=6, "Postgraduate degree (Master's or PhD)"=7)) %>%
  mutate(feducc_1 = recode(feducc_1, "1 - Not sure at all"=1, "2 - A little sure"=2, "3 - Quite sure"=3, "4 - Very sure"=4)) %>%
  mutate(futurea_1 = recode(futurea_1, "No"=0, "Yes"=1)) %>%
  mutate(futurec_1 = recode(futurec_1, "No"=0, "Yes"=1)) %>%
  ### Life satisfaction
  mutate(
    across(lsatisa_1:lsatisd_1,
           ~ recode(.x, "1 - Strongly disagree"=1, "2 - Disagree"=2, "3 - Neither agree nor disagree"=3, "4 - Agree"=4, "5 - Strongly agree"=5))) %>%
  ### Stress
  mutate(
    across(lifestressa_1:lifestressi_1,
           ~ recode(.x, "1 - Not at all stressful"=1, "2 - A little stressful"=2, "3 - Moderately stressful"=3, "4 - Stressful"=4, "5 - Extremely stressful"=5)))

################# (C) CIVIC ENGAGEMENT AND SOCIAL CONTRIBUTION #################

data <- data %>%
  ### Civic activities
  mutate(
    across(civicacta_1:civicactm_1,
           ~ recode(.x, "No"=0, "Yes"=1))) %>%
  ### Political talk and socio-politics
  mutate(
    across(poltalka_1:poltalkb_1,
           ~ recode(.x, "1 - Not at all"=1, "2 - Once or twice"=2, "3 - About once a month"=3, "4 - Several times a month"=4, "5 - Everyday"=5, "6 - Multiple times a day"=6))) %>%
  mutate(
    across(pola_1:pold_1,
           ~ recode(.x, "1 - Not at all"=1, "2 - Once or twice"=2, "3 - About once a month"=3, "4 - Several times a month"=4, "5 - Everyday"=5, "6 - Multiple times a day"=6))) %>%
  ### GE2020
  mutate(
    across(ge2020a_1:ge2020d_1,
           ~ recode(.x, "No"=0, "Yes"=1))) %>%
  ### Other civic engagement
  mutate(
    across(otherciva_1:othercive_1,
           ~ recode(.x, "1 - Never"=1, "2 - Once"=2, "3 - Once a week"=3, "4 - Several times a week"=4, "5 - Daily"=5)))

##################### (D) COVID-19 STRESSORS AND SUPPORT #####################

### Household size
table(data$clivea_1)

data <- data %>%
  ### Family support
  mutate(
    across(famsuppa_1:famsuppc_1,
           ~ recode(.x, "1 - Not at all"=1, "2 - Once or twice"=2, "3 - About once a month"=3, "4 - Several times a month"=4, "5 - Everyday"=5, "6 - Multiple times a day"=6))) %>%
  ### Family conflict
  mutate(
    across(famcona_1:famconc_1,
           ~ recode(.x, "1 - Not at all"=1, "2 - Once or twice"=2, "3 - About once a month"=3, "4 - Several times a month"=4, "5 - Everyday"=5, "6 - Multiple times a day"=6))) %>%
  ### Peer support
  mutate(
    across(peersuppa_1:peersuppc_1,
           ~ recode(.x, "1 - Not at all"=1, "2 - Once or twice"=2, "3 - About once a month"=3, "4 - Several times a month"=4, "5 - Everyday"=5, "6 - Multiple times a day"=6))) %>%
  ### Teacher relationships
  mutate(
    across(teachera_1:teacherc_1,
           ~ recode(.x, "1 - Strongly disagree"=1, "2 - Disagree"=2, "3 - Neither agree nor disagree"=3, "4 - Agree"=4, "5 - Strongly agree"=5))) %>%
  ### Work status
  mutate(cworka_1 = recode(cworka_1, "I did not work"=0, "Part-time (Less than 35 hours a week)"=1, "Full-time (35 or more hours a week)"=2)) %>%
  ### Work arrangement
  mutate(cworkc_1 = recode(cworkc_1, "I did not work"=0, "At home"=1, "At the workplace"=2, "Mixed"=3)) %>%
  ### Essential worker
  mutate(cworkd_1 = recode(cworkd_1, "No"=0, "Yes"=1)) %>%
  ### Home living environment
  mutate(cliveb_1 = recode(cliveb_1, "I shared a room with two or more people"=1, "I shared a room with one other person"=2, "I had my own room"=3)) %>%
  ### Computer access
  mutate(clivec_1 = recode(clivec_1, "I did not have a personal computer, laptop, or tablet"=1, "I shared a personal computer, laptop, or tablet"=2, "I had a personal computer, laptop, or tablet"=3)) %>%
  ### Essential worker
  mutate(clived_1 = recode(clived_1, "No"=0, "Yes"=1)) %>%
  ### Internet reliability
  mutate(clivee_1 = recode(clivee_1, "1 - Very unreliable"=1, "2 - Unreliable"=2, "3 - Reliable"=3, "4 - Very reliable"=4)) %>%
  ### Home comfort
  mutate(clivef_1 = recode(clivef_1, "1 - Very uncomfortable"=1, "2 - Uncomfortable"=2, "3 - Comfortable"=3, "4 - Very comfortable"=4)) %>%
  ### Home productivity
  mutate(cliveg_1 = recode(cliveg_1, "1 - Very unproductive"=1, "2 - Unproductive"=2, "3 - Productive"=3, "4 - Very productive"=4)) %>%
  ### Remote learning
  mutate(
    across(clearna_1:clearnc_1,
           ~ recode(.x, "1 - Strongly disagree"=1, "2 - Disagree"=2, "3 - Neither agree nor disagree"=3, "4 - Agree"=4, "5 - Strongly agree"=5))) %>%
  ### Remote learning support
  mutate(
    across(clearnd_1:clearng_1,
           ~ recode(.x, "1 - Strongly disagree"=1, "2 - Disagree"=2, "3 - Neither agree nor disagree"=3, "4 - Agree"=4, "5 - Strongly agree"=5))) %>%
  ### Singapore's COVID-19 response
  mutate(covidres_1 = recode(covidres_1, "1 - Very ineffective"=1, "2 - Ineffective"=2, "3 - Neither ineffective nor effective"=3, "4 - Effective"=4, "5 - Very effective"=5))

############################## (E) DEMOGRAPHICS ##############################

### Age
table(data$age)

table(data$gender)
table(data$sexor)
table(data$race)
table(data$religion)
table(data$moefas)
table(data$schoolaid)
table(data$housing)
table(data$rental)
table(data$finaid)
table(data$momed)
table(data$daded)

data <- data %>%
  ### Gender
  mutate(gender = recode(gender, "Female"=1, "Male"=2, "Non-binary / third gender"=3, "Prefer not to say"=3)) %>%
  ### Sexual orientation
  mutate(sexor = ifelse(sexor=="Straight (Heterosexual)", 1, 0)) %>%
  ### Race
  mutate(race =recode(race, "Chinese"=1, "Malay"=2, "Indian"=3, "Mixed / Others"=4)) %>%
  ### Religion
  mutate(religion = recode(religion, "Buddhism"=1, "Taoism or traditional Chinese beliefs"=1, "Christianity"=2, "Catholicism"=2, "Hinduism"=3, "Islam"=3, "Other religion(s)"=3, "No religion"=4)) %>%
  ### MOE FAS
  mutate(moefas = recode(moefas, "No"=0, "Yes"=1)) %>%
  ### School aid
  mutate(schoolaid = recode(schoolaid, "I am not currently in school"=0, "No"=0, "Yes"=1)) %>%
  ### Housing
  mutate(housing = recode(housing, "HDB 1-2 rooms"=1, "HDB 3 rooms"=1, "HDB 4 rooms"=2, "HDB 5 rooms, executive, and above"=3, "Private flat and condominium"=4, "Private house and bungalow"=5)) %>%
  ### Rental housing
  mutate(rental = recode(rental, "No"=0, "Yes"=1)) %>%
  ### Financial assistance
  mutate(finaid = recode(finaid, "No"=0, "Yes"=1)) %>%
  ### Parent education
  mutate(momed = recode(momed, "Not applicable / I am not in contact with my mother"=0, "PSLE and below (primary school)"=1, "‘O’ or ‘N’ Level (secondary school)"=1, "‘A’ Level"=2, "ITE or equivalent"=2, "Diploma"=2, "University"=3)) %>%
  mutate(daded = recode(daded, "Not applicable / I am not in contact with my father"=0, "PSLE and below (primary school)"=1, "‘O’ or ‘N’ Level (secondary school)"=1, "‘A’ Level"=2, "ITE or equivalent"=2, "Diploma"=2, "University"=3))

################################ Descriptives ################################

### Create dummy variables

### Age
table(data$age)

### Gender
data$female <- ifelse(data$gender == 1, 1, 0)
data$male <- ifelse(data$gender == 2, 1, 0)
data$othergender <- ifelse(data$gender == 3, 1, 0)

### Sexual orientation
table(data$sexor)

### Race
data$chinese <- ifelse(data$race == 1, 1, 0)
data$malay <- ifelse(data$race == 2, 1, 0)
data$indian <- ifelse(data$race == 3, 1, 0)
data$others <- ifelse(data$race == 4, 1, 0)

### Religion
data$buddhismtaosim <- ifelse(data$religion == 1, 1, 0)
data$christiancatholicism <- ifelse(data$religion == 2, 1, 0)
data$hinduismislamothers <- ifelse(data$religion == 3, 1, 0)
data$noreligion <- ifelse(data$religion == 4, 1, 0)

### MOE FAS
table(data$moefas)

### School aid
table(data$schoolaid)

### Housing
data$onetwothreehdb <- ifelse(data$housing == 1, 1, 0)
data$fourhdb <- ifelse(data$housing == 2, 1, 0)
data$fivehdb <- ifelse(data$housing == 3, 1, 0)
data$condo <- ifelse(data$housing == 4, 1, 0)
data$bungalow <- ifelse(data$housing == 5, 1, 0)

### Rental housing
table(data$rental)
  
### Financial assistance
table(data$finaid)

### Parent education
data$momprisec <- ifelse(data$momed == 1, 1, 0)
data$momihl <- ifelse(data$momed == 2, 1, 0)
data$momuni <- ifelse(data$momed == 3, 1, 0)
data$dadprisec <- ifelse(data$daded == 1, 1, 0)
data$dadihl <- ifelse(data$daded == 2, 1, 0)
data$daduni <- ifelse(data$daded == 3, 1, 0)

### Generate descriptives
demo.mean <- data.frame(data$age,data$female,data$male,data$othergender,data$sexor,
                        data$chinese,data$malay,data$indian,data$others,
                        data$buddhismtaosim,data$christiancatholicism,data$hinduismislamothers,data$noreligion,
                        data$moefas,data$schoolaid,
                        data$onetwothreehdb,data$fourhdb,data$fivehdb,data$condo,data$bungalow,data$rental,data$finaid,
                        data$momprisec,data$momihl,data$momuni,data$dadprisec,data$dadihl,data$daduni)
describe(demo.mean, na.rm=TRUE)

### Correlation matrix
psych::corr.test(demo.mean)

##################################### CFA #####################################

### Install and load packages
if (!require("foreign")) {install.packages("foreign"); require("foreign")}
if (!require("lavaan")) {install.packages("lavaan"); require("lavaan")}
if (!require("semPlot")) {install.packages("semPlot"); require("semPlot")}

### Burdensomeness and belongingness
burdensomeness <- data.frame(data$burdena_1,data$burdenb_1,data$burdenc_1)
round(cor(burdensomeness, use='pairwise.complete.obs'), 2)
round(cov(burdensomeness, use='pairwise.complete.obs'), 2)

#One factor, three items, default marker method
m1a <- ' f =~ burdena_1 + burdenb_1 + burdenc_1'
onefac3items_a <- cfa(m1a, data=data)
summary(onefac3items_a)

#One factor, three items, variance standardisation
m1b <- '  f =~ NA*burdena_1 + burdenb_1 + burdenc_1
          f ~~ 1*f '
onefac3items_b <- cfa(m1b, data=data)
summary(onefac3items_b)

#Automatic standardisation in lavaan
#Estimate = Default marker method
#Std.lv = Variance standardisation
#Std.all = Standardised factor loadings
summary(onefac3items_a,standardized=TRUE)

### Depression
depression <- data.frame(data$anxdepa_1,data$anxdepb_1,data$anxdepc_1,data$anxdepd_1,
                         data$anxdepe_1,data$anxdepf_1,data$anxdepg_1,data$anxdeph_1)
round(cor(depression, use='pairwise.complete.obs'), 2)
round(cov(depression, use='pairwise.complete.obs'), 2)

m1 <- 'f =~ anxdepa_1 + anxdepb_1 + anxdepc_1 + anxdepd_1 + anxdepe_1 + anxdepf_1 + anxdepg_1 + anxdeph_1'
mymod <- cfa(m1, data=data)
summary(mymod,fit.measures=TRUE,standardized=TRUE)

m2 <- 'f =~ anxdepb_1 + anxdepc_1 + anxdepd_1 + anxdepe_1 + anxdepf_1 + anxdepg_1 + anxdeph_1'
mymod1 <- cfa(m2, data=data)
summary(mymod1,fit.measures=TRUE,standardized=TRUE)

m3 <- 'f =~ anxdepb_1 + anxdepc_1 + anxdepd_1 + anxdepe_1 + anxdepg_1 + anxdeph_1'
mymod2 <- cfa(m3, data=data)
summary(mymod2,fit.measures=TRUE,standardized=TRUE)

##################################### SEM #####################################

#Simple regression using lm()
m1a <- lm (lifestressa_1 ~ housing, data=data)
(fit1a <- summary(m1a))

#Simple regression using lavaan
m1b <- '
  # regressions
    lifestressa_1 ~ 1 + housing
  # variance (optional)
    housing ~~ housing
'
fit1b <- sem(m1b, data=data)
summary(fit1b)

#Multiple regression using lavaan
m2 <- '
  # regressions
    lifestressa_1 ~ 1 + moefas + housing
  #covariance
    moefas ~~ housing
'
fit2 <- sem(m2, data=data)
summary(fit2)

#Multivariate regression using lavaan
m3a <- '
  # regressions
    lifestressa_1 ~ 1 + moefas + housing
    lifestressb_1 ~ 1 + housing
'
fit3a <- sem(m3a, data=data)
summary(fit3a)

#Path analysis in lavaan
m4a <- '
  lifestressa_1 ~ 1 + moefas + housing
  lifestressb_1 ~ 1 + housing + lifestressa_1
'
fit4a <- sem(m4a, data=data)
summary(fit4a)

semPaths(fit4a)
semPaths(fit4a, "std","est")

#Modification index
modindices(fit4a,sort=TRUE)
modindices(fit4a) %>% arrange(-mi) %>% head(10)

#Model fit statistics
summary(fit4a, fit.measures=TRUE)

#Baseline model (or the worst-fitting model)
m4c <- '
  # variances only
  lifestressa_1 ~~ lifestressa_1
  moefas ~~ moefas
  lifestressb_1 ~~ lifestressb_1
  housing ~~ housing
'
fit4c <- sem(m4c, data=data)
summary(fit4c, fit.measures=TRUE)

#Measurement model

#One factor, three items, default marker method
m1a <- ' f =~ burdena_1 + burdenb_1 + burdenc_1'
onefac3items_a <- cfa(m1a, data=data)
summary(onefac3items_a)
summary(onefac3items_a,standardized=TRUE)

m5a <- 'burdenexo =~ burdena_1 + burdenb_1 + burdenc_1
  #intercepts (nu = tau)
  burdena_1 ~ 1
  burdenb_1 ~ 1
  burdenc_1 ~ 1'
fit5a <- sem(m5a, data=data)
summary(fit5a, standardized=TRUE)

#Structural regression model
m6a <- '
  # measurement model
  burdenfactor =~ burdena_1 + burdenb_1 + burdenc_1
  socialtrustfactor =~ strusta_1 + strustb_1 + strustc_1
  familyconflictfactor =~ famcona_1 + famconb_1 + famconc_1
  # regressions
  familyconflictfactor ~ burdenfactor + socialtrustfactor
'
fit6a <- sem(m6a, data=data)
summary(fit6a, standardized=TRUE, fit.measures=TRUE)

############################ Measurement Invariance ############################

### Configural (configuration; same number of factors and variables)
### Metric (measured similarly; factor loadings are the same, intercepts and residual variances can be different)
### Scalar (compare across groups, "versus"; fixes observed intercepts)
### Residual (residuals are the same across groups)

yesaid<- subset(data,finaid==1)
noaid<- subset(data,finaid==0)
dim(yesaid)
dim(noaid)

# Separate CFA models
onefac <- 'f1  =~ lsatisa_1 + lsatisb_1 + lsatisc_1 + lsatisd_1'

# Equivalent mean structure
onefac_b <- 'f1  =~ lsatisa_1 + lsatisb_1 + lsatisc_1 + lsatisd_1
        lsatisa_1 ~ 1
        lsatisb_1 ~ 1
        lsatisc_1 ~ 1
        lsatisd_1 ~ 1'

fityesaid <- cfa(onefac, data = yesaid, meanstructure = TRUE) 
summary(fityesaid, standardized=TRUE)

fitnoaid <- cfa(onefac, data = noaid, meanstructure = TRUE) 
summary(fitnoaid, standardized=TRUE)

# Configural invariance
fit.Configural <- cfa(onefac, data = data, group = "finaid", meanstructure = TRUE)
summary(fit.Configural, standardized=TRUE)

# Metric invariance *
fit.Metric <- cfa(onefac, data = data, group = "finaid",
                  group.equal = c("loadings"), meanstructure = TRUE) 
summary(fit.Metric, standardized=TRUE)

# Scalar invariance ***
fit.Scalar <- cfa(onefac, data = data, group = "finaid", 
                  group.equal = c("loadings","intercepts"), meanstructure = TRUE) 
summary(fit.Scalar , standardized=TRUE)

# Residual invariance
fit.Residual <- cfa(onefac, data = data, group = "finaid", 
                    group.equal = c("loadings","intercepts","residuals"), meanstructure = TRUE) 
summary(fit.Residual, standardized=TRUE)

# Chi-squared difference test
lavTestLRT(fit.Configural, fit.Metric, fit.Scalar, fit.Residual)


############################# Impute Missing Data #############################

### Install and load packages
if (!require("mice")) {install.packages("mice"); require("mice")}

### Remove those with <60% completion (n = 71)
data <- data[data$Progress > 59, ]

### Impute missing values
### Perform same analysis
### Pool results
### https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/mi.html

set.seed(168)
mids.imp <- mice(data, m = 5, method ="pmm") ### Predictive mean matching
data.imputed <- complete(mids.imp, "long", include = TRUE) ### .imp variable identifies imputation

########################## Composite Measure Creation ##########################

### (A) Psychological well-being

data <- data %>%
  ### Burdensomeness and belongingness
  mutate( 
    burdenmean = rowMeans(select(., burdena_1:burdenc_1), na.rm=TRUE)) %>%
  ### Social trust
  mutate( 
    strustmean = rowMeans(select(., strusta_1:strustc_1), na.rm=TRUE)) %>%
  ### Depression
  mutate( 
    depmean = rowMeans(select(., anxdepa_1:anxdeph_1), na.rm=TRUE)) %>%
  ### Anxiety
  mutate( 
    anxmean = rowMeans(select(., anxdepi_1:anxdepp_1), na.rm=TRUE)) %>%
  ### Life satisfaction
  mutate(
    lifesatamean = rowMeans(select(., lifesata_1:lifesatg_1), na.rm=TRUE))

### Burdensomeness and belongingness
alpha(data[,c("burdena_1","burdenb_1","burdenc_1")]) 
omega(data[,c("burdena_1","burdenb_1","burdenc_1")])

### Social trust
alpha(data[,c("strusta_1","strustb_1","strustc_1")]) 
omega(data[,c("strusta_1","strustb_1","strustc_1")])

### Depression
alpha(data[,c("anxdepa_1","anxdepb_1","anxdepc_1","anxdepd_1","anxdepe_1","anxdepf_1","anxdepg_1","anxdeph_1")]) 
omega(data[,c("anxdepa_1","anxdepb_1","anxdepc_1","anxdepd_1","anxdepe_1","anxdepf_1","anxdepg_1","anxdeph_1")])

### Anxiety
alpha(data[,c("anxdepi_1","anxdepj_1","anxdepk_1","anxdepl_1","anxdepm_1","anxdepn_1","anxdepo_1","anxdepp_1")]) 
omega(data[,c("anxdepi_1","anxdepj_1","anxdepk_1","anxdepl_1","anxdepm_1","anxdepn_1","anxdepo_1","anxdepp_1")])

### Life satisfaction
alpha(data[,c("lifesata_1","lifesatb_1","lifesatc_1","lifesatd_1","lifesate_1","lifesatf_1","lifesatg_1")])
omega(data[,c("lifesata_1","lifesatb_1","lifesatc_1","lifesatd_1","lifesate_1","lifesatf_1","lifesatg_1")])

### (B) Employment and educational outlook

data <- data %>%
  ### Economic stressors
  mutate(
    estressmean = rowMeans(select(., estressa_1:estressd_1), na.rm=TRUE)) %>%
  ### Economic worry
  mutate(
    eworrymean = rowMeans(select(., eworrya_1:eworrye_1), na.rm=TRUE)) %>%
  ### Life satisfaction
  mutate(
    lsatismean = rowMeans(select(., lsatisa_1:lsatisd_1), na.rm=TRUE)) %>%
  ### Stress
  mutate(
    lifestresssum = rowSums(select(., lifestressa_1:lifestressi_1), na.rm=TRUE)) 

### Economic stressors
alpha(data[,c("estressa_1","estressb_1","estressc_1","estressd_1")])
omega(data[,c("estressa_1","estressb_1","estressc_1","estressd_1")])

### Economic worry
alpha(data[,c("eworrya_1","eworryb_1","eworryc_1","eworryd_1","eworrye_1")])
omega(data[,c("eworrya_1","eworryb_1","eworryc_1","eworryd_1","eworrye_1")])

### Life satisfaction
alpha(data[,c("lsatisa_1","lsatisb_1","lsatisc_1","lsatisd_1")])
omega(data[,c("lsatisa_1","lsatisb_1","lsatisc_1","lsatisd_1")])

### (C) Civic engagement and social contribution

data <- data %>%
  ### Civic activities 
  mutate(
    civicactsum = rowSums(select(., civicacta_1:civicactm_1), na.rm=TRUE)) %>%
  ### Political talk and socio-politics
  mutate(
    poltalkmean = rowMeans(select(., poltalka_1:poltalkb_1), na.rm=TRUE)) %>%
  mutate(
    polmean = rowMeans(select(., pola_1:pold_1), na.rm=TRUE)) %>%
  ### GE2020
  mutate(
    ge2020sum = rowSums(select(., ge2020a_1:ge2020d_1), na.rm=TRUE))

### (D) COVID-19 stressors and support

data <- data %>%
  ### Family support
  mutate(
    famsuppmean = rowMeans(select(., famsuppa_1:famsuppc_1), na.rm=TRUE)) %>%
  ### Family conflict
  mutate(
    famconmean = rowMeans(select(., famcona_1:famconc_1), na.rm=TRUE)) %>%
  ### Peer support
  mutate(
    peersuppmean = rowMeans(select(., peersuppa_1:peersuppc_1), na.rm=TRUE)) %>%
  ### Teacher relationships
  mutate(
    teachermean = rowMeans(select(., teachera_1:teacherc_1), na.rm=TRUE)) %>%
  ### Remote learning
  mutate(
    remotemean = rowMeans(select(., clearna_1:clearnc_1), na.rm=TRUE)) %>%
  ### Remote learning support
  mutate(
    remotesupportmean = rowMeans(select(., clearnd_1:clearng_1), na.rm=TRUE)) %>%
  ### Distress
  mutate(
    distressmean = rowMeans(select(., cplay_1_1:cplay_1_7), na.rm=TRUE))

### Family support
alpha(data[,c("famsuppa_1","famsuppb_1","famsuppc_1")])
omega(data[,c("famsuppa_1","famsuppb_1","famsuppc_1")])

### Family conflict
alpha(data[,c("famcona_1","famconb_1","famconc_1")])
omega(data[,c("famcona_1","famconb_1","famconc_1")])

### Peer support
alpha(data[,c("peersuppa_1","peersuppb_1","peersuppc_1")])
omega(data[,c("peersuppa_1","peersuppb_1","peersuppc_1")])

### Teacher relationships
alpha(data[,c("teachera_1","teacherb_1","teacherc_1")])
omega(data[,c("teachera_1","teacherb_1","teacherc_1")])

################################## Save Data ##################################

cleandata <- data
write.csv(cleandata, "cleandata.csv")



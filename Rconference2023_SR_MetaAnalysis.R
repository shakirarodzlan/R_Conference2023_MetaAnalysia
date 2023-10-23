

##### This R Code for demonstration R Conference 2023 (28 Oct 2023, Sunway University) ############

# Speaker: Wan Shakira Rodzlan Hasani (PhD Student, Biostatistics, USM)
# Title: Systematic Review and Meta analysis using R


##########

#Install and load meta package 
install.packages("meta")
library(meta)

########## Continuous outcome #######################

#Load data
data(Fleiss1993cont)
head (Fleiss1993cont)

# run meta-analysis for standardized mean difference (SMD)
m_cont <- metacont(n.psyc, mean.psyc, sd.psyc, n.cont, 
               mean.cont, sd.cont,
               data = Fleiss1993cont, sm = "SMD",
               studlab = paste(study, year))

m_cont

# Forest plot
forest(m_cont)

# Modify forest plot
  # Select only one model (e.g: fixed effect model) 
m_cont <- metacont(n.psyc, mean.psyc, sd.psyc, n.cont, 
                   mean.cont, sd.cont,
                   data = Fleiss1993cont, sm = "SMD",
                   random = F,
                   studlab = paste(study, year))
forest (m_cont)

  # customize parameter position 
forest(m_cont, leftcols = c('studlab'))

  # Revman format
forest(m_cont, layout = "RevMan5")

  # JAMA format
forest.meta(m_cont, layout = "JAMA")


# subgroup analysis 

# add variable for group (region)
Fleiss1993cont$region <- c("Europe", "Europe", "Asia", "Asia", "Europe")

m_cont2 <- metacont(n.psyc, mean.psyc, sd.psyc, n.cont, mean.cont, sd.cont,
                   data = Fleiss1993cont, sm = "SMD", 
                   random = F,
                   studlab = paste(study, year),
                   subgroup = region)
m_cont2
forest(m_cont2)



############### Binary outcome ##########################

#load data
data (Fleiss1993bin)
head(Fleiss1993bin)
View(Fleiss1993bin)

m_bin <- metabin(d.asp, n.asp, d.plac, n.plac, 
                 data = Fleiss1993bin,
                 studlab = paste(study, year),
                 sm = "OR", random = FALSE)
print (m_bin)

#Forest plot
forest (m_bin)


########### Publication bias #######

# Funnel plot
funnel(m_bin)

# Egger test
metabias(m_bin, plotit = T, method.bias = "Egger", k.min = 7)

# Begg test
metabias(m_bin, plotit = T, method.bias = "Begg", k.min = 7)


##### Check outlier ##############

# Influential diagnostics 
baujat(m_bin, studlab = F)
baujat(m_bin)





######### Other type of data ##########################

######### Incidence Rate Ratio #####

data(smoking) #data smoking from meta package
m_smoking <- metainc(d.smokers, py.smokers, d.nonsmokers, py.nonsmokers,
              data = smoking, studlab = study, common = F)
print(m_smoking)

forest (m_smoking)


######### Proportion ##################

# create data frame
abc <- structure(list(Study = c("Study 1", "Study 2", "Study 3", "Study 4", 
                                "Study 5", "Study 6", "Study 7", "Study 8", 
                                "Study 9", "Study 10", "Study 11", "Study 12",
                                "Study 13", "Study 14", "Study 15", "Study 16"), 
                      event.e = c(1L, 4L, 6L, 20L, 69L, 22L, 19L, 26L, 13L, 2L, 
                                  7L, 2L, 29L, 6L, 11L, 4L), 
                      n.e = c(12L, 21L, 43L, 83L, 373L, 219L, 164L, 264L, 102L, 14L, 53L, 60L, 172L, 55L, 49L, 14L)), 
                 class = "data.frame", row.names = c(NA, -16L))

head (abc)

prop <- metaprop(event = event.e, 
                n= n.e, 
                data=abc,
                studlab = Study,
                sm="Plogit",
                level = 0.95,
                comb.fixed=TRUE, comb.random=TRUE,
                hakn=F,
                method.tau="DL")
prop

forest (prop)


################# Single incidence rate ################

m_rate <- metarate(4:1, c(10, 20, 30, 40))
m_rate 
forest(m_rate)

################## single mean #########################


m_mean  <- metamean(rep(100, 3), 1:3, rep(1, 3))
m_mean
forest (m_mean)

############## using metagen function ####################

# Load meta analysis data from my github for paper entitle;
# “The global estimate of premature cardiovascular mortality: a systematic review and meta-analysis of age-standardized mortality rate”
# https://github.com/shakirarodzlan/PhD_MetaAnalysis_PMCVD.git 

# load "ma_asmr5.xlsx" data 

# Install and load required packages
install.packages("readxl")
install.packages("httr")
library(readxl)
library(httr)

# Define the URL of the Excel file
url <- "https://github.com/shakirarodzlan/PhD_MetaAnalysis_PMCVD/raw/main/ma_asmr5.xlsx"
# Download the Excel file
temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file))
# Read the downloaded Excel file
asmr <- read_excel(temp_file)

# View the data
head (asmr)
View(asmr) 

## log effect size (ASMR)
asmr$ASMR_log <- log(asmr$ASMR)
summary(asmr)

## overall ASMR
meta_asmrall <- metagen(TE = ASMR_log, 
                        seTE = SE.asmr, 
                        sm = "RR", 
                        studlab = author_year,
                        common = F,
                        method.tau = "PM",
                        prediction = T,
                        hakn = T,
                        adhoc.hakn = "iqwig6",
                        data = asmr)
meta_asmrall

#Forest plot
forest(meta_asmrall, sortvar = TE, allstudies = F)

##subgroup by CVD type
metareg_type <- update(meta_asmrall, subgroup = cvd_type) 
metareg_type
#forest plot
forest(metareg_type, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "sex", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death",  "Sex", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       bylab = c("Total CVD", "Stroke or Cerebrovascular disease", "IHD or Heart disease"),
       smlab = " ", just = "center")

# Sub analysis within CVD types

# select among total CVD (ICD-10 code: I00-I99 or ICD-9 codes: 350-459) 

library(tidyverse)

asmr_all_CVD <- asmr %>% 
  filter(cvd_type == "All CVD") 

## overall ASMR
meta_allcvd <- metagen(TE = ASMR_log, 
                       seTE = SE.asmr, 
                       sm = "RR", 
                       studlab = author_year,
                       common = F,
                       method.tau = "PM",
                       prediction = T,
                       hakn = T,
                       adhoc.hakn = "iqwig6",
                       data = asmr_all_CVD)
meta_allcvd
forest(meta_allcvd)



 # Total CVD by sex

meta_allcvd_sex <- metagen(TE = ASMR_log, 
                          seTE = SE.asmr, 
                          sm = "RR", 
                          studlab = author_year,
                          common = F,
                          method.tau = "PM",
                          prediction = T,
                          hakn = T,
                          adhoc.hakn = "iqwig6",
                          subgroup = sex,
                          data = asmr_all_CVD)
forest(meta_allcvd_sex)



################### Meta- regression Analysis ########################

# Multiple meta-regression

# Packages
library(metafor)
library(PerformanceAnalytics)
library(dplyr)

# Modify data asmr
asmr2 <- asmr %>%
  mutate(ASMR_log = log(ASMR)) %>% 
  filter(year_grp2 != "2000-2019") %>%  #exclude mix year 2000-2009 (2 studies)
  filter(sex != "Both sex") %>% #exclude both sex
  rename(study_time = "year_grp2",
         sex2 = "sex")

# Reorder the levels
asmr2$cvd_type <- factor(asmr2$cvd_type, 
                         levels = c("Cerebrovascular Disease/Stroke", "IHD", 
                                    "Other heart disease", "All CVD"))

summary (asmr2)

# Multicollinearity -------------------------------------------------------

# Pairwise correlation
asmr2 %>% 
  select(cvd_type, sex2, income_country2, study_time) %>% 
  mutate_all(as.factor) %>% 
  mutate_all(as.numeric) %>% # change all factor to numeric
  chart.Correlation()

# Very crude way to assess MC
# r > 0.8 may indicate MC

# Meta-regression ---------------------------------------------------------

# Backward variable selection

# Model 1 - full model
full_mod <- rma(yi = ASMR_log,
                sei = SE.asmr,
                data = asmr2,
                method = "ML",
                mods = ~ cvd_type + sex2 + income_country2 + study_time + age_grp,
                test = "knha")
full_mod # all significant 

# Model 2 - exclude study time based on the largest p val
mod2 <- rma(yi = ASMR_log,
            sei = SE.asmr,
            data = asmr2,
            method = "ML",
            mods = ~ cvd_type + sex2 + income_country2 + age_grp,
            test = "knha")
mod2 # all significant 

# Model comparison 1
anova(full_mod, mod2)
# P is not significant opt for the simpler model, the model with lower AICc (full_model)


# Final model -------------------------------------------------------------

mod2 # study time not include in the final model

# Permutation test --------------------------------------------------------

# Permutation test is done to ensure the robustness of the final model
permutest(mod2)

# Based on the permutation test:
# cvd_type and sex are still significant- thus can be consider as important predictors that influence the pooled effect size


# FINAL model

mod2



############# End Analysis ##############################

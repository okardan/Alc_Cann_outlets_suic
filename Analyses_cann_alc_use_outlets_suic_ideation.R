#
# Manuscript Title: Associations of access to alcohol and cannabis outlets, alcohol and cannabis use, and suicidal ideation
# For questions please contact Omid Kardan omidk@med.umich.edu
# Scripts by Omid Kardan and Kathryn Schertz

# Section1: Scripts to count alcohol and cannabis outlets in the SAfeGraph POI data (skip to section 3 if you don't have 
# SafeGraph data access (for example subscription to Deweydata.org)

# Section 2: Makes publicly share-able reduced .csv dataset file (skip to section 3 if you didn't start at section 1)

# Section 3: Scripts that run regressions and generate the figures and tables in the manuscript
# 
# Section 4: Makes the US map figures (Figure 1 in the manuscript and supp Figure S1)
########## 

library(dplyr)
library(ggplot2)
library(tidyverse)
library(ppcor)
library(fastDummies)
library(sjPlot)
library(corrplot)
library(lme4)
library(lmerTest)
library(plyr)
library(mediation)
library(effectsize)
library(logistf)
library(lme4)
library(MASS)
library(glmmTMB)
library(survey)
library(jtools)
library(marginaleffects)

##########################################################################################################################################
######### Section 1: This section generates the .csv used in the study analyses, for access to SafeGraph POI data contact deweydata.org and for 
######### access to the NFACS survey full data contact Mark Ilgen marki@med.umich.edu or Brian Hicks brianhic@med.umich.edu

######### IF YOU DON'T HAVE SAFEGRAPH DATA SKIP DIRECTLY TO SECTION 3! ##################################

setwd('~/Analyses')

#Processing SafeGraph data for cannabis and alcohol to USA zip code level and making heat maps

library(readr)      # For fast, tidy reading of CSV and CSV.GZ files
library(tidyverse)

# Reading in Dewey Data
path1 <- "/SafeGraphPOI_US_CA_FL_TX_IL_NY/"
path2 <- "/SafeGraphPOI_US_PA_OH_GA_NC_MI_NJ/"
path3 <- "/SafeGraphPOI_US_other39States/"

files2 <- list.files(path2, pattern = "\\.csv\\.gz$", full.names = TRUE)
files3 <- list.files(path3, pattern = "\\.csv\\.gz$", full.names = TRUE)

data2 <- do.call(dplyr::bind_rows, lapply(files2, read_csv))
data3 <- do.call(dplyr::bind_rows, lapply(files3, read_csv))

#The first set had a problem where some files read in POSTAL_CODE as numeric and some as character so setting it to be character to match each other
col_spec <- cols(
  POSTAL_CODE = col_character(),
  .default = col_guess()
)
read_file_with_types <- function(file_path) {
  read_csv(file_path, col_types = col_spec)
}
files1 <- list.files(path1, pattern = "\\.csv\\.gz$", full.names = TRUE)
data1 <- do.call(dplyr::bind_rows, lapply(files1, read_file_with_types))
data1 <- data1 %>% select(-GEOMETRY_TYPE) #removing the column that the other two groups didn't have

# Keeping alcohol 
naics_alcohol <- c(722410, 445310, 424810, 424820)
data1_naics_alcohol <- data1 %>% filter(NAICS_CODE %in% naics_alcohol)
data2_naics_alcohol <- data2 %>% filter(NAICS_CODE %in% naics_alcohol)
data3_naics_alcohol <- data3 %>% filter(NAICS_CODE %in% naics_alcohol)

alldata_alcohol <- bind_rows(data1_naics_alcohol, data2_naics_alcohol, data3_naics_alcohol) 

# Keeping cannabis 
naics_cannabis <- c(459991, 424590)
data1_naics2022_cannabis <- data1 %>% filter(NAICS_CODE_2022 %in% naics_cannabis)
data2_naics2022_cannabis <- data2 %>% filter(NAICS_CODE_2022 %in% naics_cannabis)
data3_naics2022_cannabis <- data3 %>% filter(NAICS_CODE_2022 %in% naics_cannabis)

alldata_cannabis <- bind_rows(data1_naics2022_cannabis, data2_naics2022_cannabis, data3_naics2022_cannabis)

# filtering for actual cannabis stores since the NAICS code is too expansive
allcannabis_filtered <- alldata_cannabis %>% filter(if_any(c(LOCATION_NAME, CATEGORY_TAGS), ~ str_detect(.x, "Cannabis|Dispensary|Marijuana|Headshop|Weed|THC")))

#saving all alcohol and all cannabis locations to avoid reprocessing
write_csv(alldata_alcohol, "/AllAlcoholLocations.csv")
write_csv(alldata_cannabis, "AllCannabisLocations.csv")
write_csv(allcannabis_filtered, "FilteredCannabisLocations.csv")

#SafeGraph data already has postal (zip) code included
counts_byzip_alcohol <- alldata_alcohol %>% group_by(POSTAL_CODE) %>% summarise(AlcoholCount = n())
counts_byzip_cannabis <- alldata_cannabis_filtered %>% group_by(POSTAL_CODE) %>% summarise(CannabisCount = n())

#Joining to survey data
firearm_survey <- read_csv("Firearm suicide survey - Copy.csv")
firearm_survey2 <- firearm_survey %>% mutate(demo14b_5digit = substr(demo14b,1,5)) #17 people had included +4 on zipcode

firearm_survey2 <- firearm_survey2 %>% left_join(counts_byzip_alcohol, by = join_by(demo14b_5digit == POSTAL_CODE))
firearm_survey2 <- firearm_survey2 %>% left_join(counts_byzip_cannabis, by = join_by(demo14b_5digit == POSTAL_CODE))

# Changing NA to 0 since it represents zip codes that were not in the Safegraph data
firearm_survey2$AlcoholCount[is.na(firearm_survey2$AlcoholCount)] <- 0
firearm_survey2$CannabisCount[is.na(firearm_survey2$CannabisCount)] <- 0

#writing out data
write_csv(firearm_survey2, "FirearmSurvey_AlcoholCannabisCounts.csv")


############## Section 2. Generate the reduced data frame for public sharing ####################################
######################################################################################################
######### IF YOU DON'T HAVE SAFEGRAPH DATA SKIP DIRECTLY TO SECTION 3! ##################################


df_ifip000 <- read.csv('Firearm suicide survey_geocode_ip.csv') # full NFACS survey dataset
cann_alc_counts <- read.csv('FirearmSurvey_AlcoholCannabisCounts.csv')
df_ifip00 <- merge(df_ifip000, cann_alc_counts, by = "case_id",all.x = TRUE)
zipareas <- read.csv('LandAreaZipCodes.csv')
df_ifip0 <- merge(df_ifip00, zipareas[,c('case_id','LandArea')], by = "case_id",all.x = TRUE)

cannlegballot <- read.csv('Ballotpedia_Legalization.csv')
df_ifip <- merge(df_ifip0, cannlegballot[,c("demo14a","Legal_Med_GroundTruth","Legal_Rec_GroundTruth")], by.x = "demo14a.x",by.y = "demo14a",all.x = TRUE)


df_ifip$cann_c <- df_ifip$CannabisCount
df_ifip$adult_pop_approx <- round(df_ifip$adult_pop, -2) # rounded pop density to prevent locating exact zip codes (only in the publicly shared dataset)
df_ifip$weight_nogun <- round(df_ifip$weight_nogun, 2) # rounded survey weights to prevent locating exact zip codes (only in the publicly shared dataset)
df_ifip$adult_pop <- df_ifip$adult_pop_approx
df_ifip_public <- df_ifip[,c('case_id','suic1', 'suic2', 'suic3','phq1','phq2',
                             'Age_cat4' , 'Race_WhNWh' ,'demo4', 'Education_cat4', 'Income_cat4','adult_pop',
                             'demo2','Can_freq_dur','Alc_qnt_freq','assist1a_1j_2','assist1a_1j_3','weight_nogun','Legal_Rec_GroundTruth',
                             'Legal_Med_GroundTruth','neighborcan1','cann_c','AlcoholCount')]

write.csv(df_ifip_public,
          'ifip_public_dataframe.csv')



###############################################################################################################################
###################################################### section 3: Analyses for the study  #####################################
###############################################################################################################################

df_ifip <- read.csv('ifip_public_dataframe.csv')
df_ifip_suic <- df_ifip %>% dplyr::select(case_id,suic1, suic2, suic3)
df_ifip_suic <- df_ifip_suic %>% mutate(suic1_bin = if_else(suic1 == 2,1,0),
                                        suic2_bin = if_else(suic2 == 2,1,0),
                                        suic3_bin = if_else(suic3 == 2,1,0))
df_ifip_suic %>% filter(is.na(suic3))
df_ifip_suic$suic_ideate <- rowMeans(df_ifip_suic[,c("suic1_bin","suic2_bin","suic3_bin")], na.rm=TRUE)
df_ifip_suic$suic_ideate[df_ifip_suic$suic_ideate>0]=1
df_ifip2 <- merge(df_ifip, df_ifip_suic[,c("case_id","suic_ideate")], by = "case_id")

dep_est <-cbind(df_ifip2$phq1, df_ifip2$phq2)
df_ifip2$dep_est <- rowMeans(dep_est, na.rm = TRUE)

###Table 1
df_ifip2_complete <- df_ifip2 %>% drop_na(Age_cat4 , Race_WhNWh ,demo4, Education_cat4, Income_cat4,adult_pop)
df_ifip2_complete$Race_WhNWh2 <- -1*df_ifip2_complete$Race_WhNWh
df_ifip2_complete$female <- -1*df_ifip2_complete$demo2


mean(df_ifip2_complete$demo1*df_ifip2_complete$weight_nogun) # 47.98332
mean(df_ifip2_complete$demo1) # 51.20849
sd(df_ifip2_complete$demo1) # 16.87364
n_18_29 <- df_ifip2_complete$demo1[df_ifip2_complete$Age_cat4==0] #867
n_18_29W <- length(n_18_29)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Age_cat4==0])# 1383.5

n_30_49 <- df_ifip2_complete$demo1[df_ifip2_complete$Age_cat4==1] #2346
n_30_49W <-  length(n_30_49)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Age_cat4==1]) #2366.0
  
n_50_64 <- df_ifip2_complete$demo1[df_ifip2_complete$Age_cat4==2] #1968
n_50_64W <-  length(n_50_64)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Age_cat4==2]) #1616.5

n_64_95 <- df_ifip2_complete$demo1[df_ifip2_complete$Age_cat4==3] #1812
n_64_95W <-  length(n_64_95)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Age_cat4==3]) #1620.2


n_male <- df_ifip2_complete$demo2[df_ifip2_complete$demo2==1] #2885
n_maleW <- length(n_male)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$demo2==1])#3398.2

n_fem <- df_ifip2_complete$demo2[df_ifip2_complete$demo2==0] #4108
n_femW <- length(n_fem)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$demo2==0])#3588.0

n_nwh <- df_ifip2_complete$Race_WhNWh[df_ifip2_complete$Race_WhNWh==1 ]# 1480
n_nwhW <- length(n_nwh)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Race_WhNWh==1 ])#2064.1

n_wh <- df_ifip2_complete$Race_WhNWh[df_ifip2_complete$Race_WhNWh==0 ]
n_whW <- length(n_wh)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Race_WhNWh==0])#4922.1

n_hisp <- df_ifip2_complete$demo4[df_ifip2_complete$demo4==1]
n_hispW <- length(n_hisp)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$demo4==1])#1302.8

n_nhisp <- df_ifip2_complete$demo4[df_ifip2_complete$demo4==0]
n_nhispW <- length(n_nhisp)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$demo4==0])#1302.8


n_0_1 <- df_ifip2_complete$demo6[df_ifip2_complete$Education_cat4==0] #894  highschool or lower
n_0_1W <-  length(n_0_1)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Education_cat4==0])#2594.8

n_2_4 <- df_ifip2_complete$demo6[df_ifip2_complete$Education_cat4==1] #  2317 technical/associate/some college
n_2_4W <-  length(n_2_4)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Education_cat4==1])#1784.4

n_5_5 <- df_ifip2_complete$demo1[df_ifip2_complete$Education_cat4==2] # 1968 bachelor
n_5_5W <-  length(n_5_5)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Education_cat4==2]) # 1380.8 bachelor


n_6_7 <- df_ifip2_complete$demo1[df_ifip2_complete$Education_cat4==3] # 1814 masters or higher
n_6_7W <- length(n_6_7)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Education_cat4==3])#1226.2


n_0_1 <- df_ifip2_complete$demo5[df_ifip2_complete$Income_cat4==0] #2078  <50K 
n_0_1W <- length(n_0_1)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Income_cat4==0])#1922.5


n_2_4 <- df_ifip2_complete$demo5[df_ifip2_complete$Income_cat4==1] #2186  50-100K
n_2_4W <- length(n_2_4)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Income_cat4==1])#2629.7

n_5_5 <- df_ifip2_complete$demo5[df_ifip2_complete$Income_cat4==2] #2212  100-250K
n_5_5W <- length(n_5_5)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Income_cat4==2])#1995.8

n_6_7 <- df_ifip2_complete$demo5[df_ifip2_complete$Income_cat4==3] #517  >250K
n_6_7W <- length(n_6_7)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Income_cat4==3])#438.2


n_alc_0_0 <- df_ifip2_complete$assist1a_1j_2[df_ifip2_complete$assist1a_1j_2==0] # 979
n_alc_0_0W <- length(n_alc_0_0)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$assist1a_1j_2==0])#1276.2

n_alc_0_1 <- df_ifip2_complete$Alc_qnt_freq[df_ifip2_complete$Alc_qnt_freq<=1 & df_ifip2_complete$assist1a_1j_2>0] # 2675
n_alc_0_1W <- length(n_alc_0_1)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Alc_qnt_freq<=1 & df_ifip2_complete$assist1a_1j_2>0])#2532.3

n_alc_2_4 <- df_ifip2_complete$Alc_qnt_freq[df_ifip2_complete$Alc_qnt_freq>1 & df_ifip2_complete$Alc_qnt_freq<=4] #2536
n_alc_2_4W <- length(n_alc_2_4)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Alc_qnt_freq>1 & df_ifip2_complete$Alc_qnt_freq<=4])#2382.9

n_alc_5_10 <- df_ifip2_complete$Alc_qnt_freq[df_ifip2_complete$Alc_qnt_freq>4 & df_ifip2_complete$Alc_qnt_freq<=10] 
n_alc_5_10W <- length(n_alc_5_10)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Alc_qnt_freq>4 & df_ifip2_complete$Alc_qnt_freq<=10])#593.0

n_alc_11 <- df_ifip2_complete$Alc_qnt_freq[df_ifip2_complete$Alc_qnt_freq>10] #174
n_alc_11W <- length(n_alc_11)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Alc_qnt_freq>10])#201

n_cann_0_0 <- df_ifip2_complete$assist1a_1j_3[df_ifip2_complete$assist1a_1j_3==0] #3154
n_cann_0_0W <- length(n_cann_0_0)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$assist1a_1j_3==0])#3355.6

n_cann_0_1 <- df_ifip2_complete$Can_freq_dur[df_ifip2_complete$Can_freq_dur<=1 & df_ifip2_complete$assist1a_1j_3>0]#2003
n_cann_0_1W <- length(n_cann_0_1)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Can_freq_dur<=1 & df_ifip2_complete$assist1a_1j_3>0])#1777.3

n_cann_2_4 <- df_ifip2_complete$Can_freq_dur[df_ifip2_complete$Can_freq_dur>1 & df_ifip2_complete$Can_freq_dur<=4] #905
n_cann_2_4W <- length(n_cann_2_4)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Can_freq_dur>1 & df_ifip2_complete$Can_freq_dur<=4])#850.9

n_cann_5_10 <- df_ifip2_complete$Can_freq_dur[df_ifip2_complete$Can_freq_dur>4 & df_ifip2_complete$Can_freq_dur<=10] #519
n_cann_5_10W <- length(n_cann_5_10)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Can_freq_dur>4 & df_ifip2_complete$Can_freq_dur<=10])#553.9

n_cann_11 <- df_ifip2_complete$Can_freq_dur[df_ifip2_complete$Can_freq_dur>10] #412
n_cann_11W <- length(n_cann_11)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$Can_freq_dur>10])#448.5


n_suic <- df_ifip2_complete$suic_ideate[df_ifip2_complete$suic_ideate>0]#955
n_suicW <- length(n_suic)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$suic_ideate>0])#976.6

n_homic <- df_ifip2_complete$gunhomic1_yr[df_ifip2_complete$gunhomic1_yr>0]#221
n_homicW <- length(n_homic)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$gunhomic1_yr>0])# 230.7

n_dep <- df_ifip2_complete$dep_est[df_ifip2_complete$dep_est>0] # 2975
n_depW <- length(n_dep)*mean(df_ifip2_complete$weight_nogun[df_ifip2_complete$dep_est>0])# 2899.6

## Supplementary Table S1
survey_design <- svydesign(
  id = ~1, 
  weights = ~weight_nogun, 
  data = df_ifip2_complete
)
weighted_corr <- svycor(~suic_ideate + Alc_qnt_freq, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$Alc_qnt_freq)

weighted_corr <- svycor(~suic_ideate + Can_freq_dur, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$Can_freq_dur)

weighted_corr <- svycor(~suic_ideate + adult_pop, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$adult_pop)

weighted_corr <- svycor(~suic_ideate + female, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$female)

weighted_corr <- svycor(~suic_ideate + Race_WhNWh2, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$Race_WhNWh2)

weighted_corr <- svycor(~suic_ideate + Age_cat4, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$Age_cat4)

weighted_corr <- svycor(~suic_ideate + Education_cat4, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$Education_cat4)

weighted_corr <- svycor(~suic_ideate + Income_cat4, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$Income_cat4)

weighted_corr <- svycor(~suic_ideate + AlcoholCount, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$AlcoholCount)

weighted_corr <- svycor(~suic_ideate + cann_c, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$cann_c)

weighted_corr <- svycor(~suic_ideate + Legal_Med_GroundTruth, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$Legal_Med_GroundTruth)

weighted_corr <- svycor(~suic_ideate + Legal_Rec_GroundTruth, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$Legal_Rec_GroundTruth)

weighted_corr <- svycor(~suic_ideate + dep_est, design = survey_design, sig.stats = TRUE)
cor.test(df_ifip2_complete$suic_ideate,df_ifip2_complete$dep_est)


#################################################
# Figure 2 (weighted regressions)
df_ifip2_complete$Race_WhNWh2 <- -1*df_ifip2_complete$Race_WhNWh
df_ifip2_complete$female <- -1*df_ifip2_complete$demo2
fit<-logistf(suic_ideate~  1 +scale(Alc_qnt_freq) + scale(female)+scale(adult_pop)+
               scale(Race_WhNWh2)+  scale(demo4) + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4), 
             data = df_ifip2_complete,weights = weight_nogun)

summary(fit)
exp(0.216327) # 1.242 odds ratio for alcohol
alc_freq_coefs <- exp(as.numeric(fit$coefficients[2:9]))
alc_freq_lowerci <- exp(as.numeric(fit$ci.lower[2:9]))
alc_freq_upperci <- exp(as.numeric(fit$ci.upper[2:9]))
alc_freq_regressionresults2 <- as.data.frame(cbind(alc_freq_coefs, alc_freq_lowerci, alc_freq_upperci))
alc_freq_regressionresults2$index <- 8:1
p1a<-ggplot(alc_freq_regressionresults2) + aes(x = index) +  
  geom_linerange(aes(ymin = alc_freq_lowerci, ymax = alc_freq_upperci,x = index,linewidth = 3, color = as.factor(index)))+
  scale_color_manual(values = c("1"= "#E64B35","2" = "#D3D3D3","3" = "#E64B35","4"= "#D3D3D3","5" = "#D3D3D3",
                                "6" = "#D3D3D3","7" = "#4DBBD5","8" = "#4DBBD5"))+
  geom_point(aes(y = alc_freq_coefs)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(breaks = seq(1,8,by=1))+
  coord_flip() + theme_classic(base_size = 29) +theme(legend.position = "none")+ labs(y = "", x = "")+ylim(.5, 1.5)
p1a
ggsave(file = "Fig2a_weighted.png", p1a, width = 8, height = 7)


#Figure 3
df_ifip2_complete$Race_WhNWh2 <- -1*df_ifip2_complete$Race_WhNWh
df_ifip2_complete$female <- -1*df_ifip2_complete$demo2
fit<-glmer(suic_ideate ~ scale(Can_freq_dur) +scale(Legal_Med_GroundTruth)+scale(Legal_Rec_GroundTruth)+ scale(female)+scale(adult_pop)+
             scale(Race_WhNWh2)+  scale(demo4) + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+
             (1|Legal_Med_GroundTruth)+(1|Legal_Rec_GroundTruth), data=df_ifip2_complete,family=binomial,weights = weight_nogun)
summary(fit)
exp(0.247447) # 1.280751 odds ratio for cannabis

can_freq_coefs <- exp(as.numeric(coefficients(fit)$Legal_Med_GroundTruth[1,c(2,5:11)]))  
a<- confint(fit, method="Wald")
can_freq_lowerci <- exp(a[c(4,7:13),1])
can_freq_upperci <- exp(a[c(4,7:13),2])
can_freq_regressionresults2 <- as.data.frame(cbind(can_freq_coefs, can_freq_lowerci, can_freq_upperci))
can_freq_regressionresults2$index <- 8:1
p3a<-ggplot(can_freq_regressionresults2) + aes(x = index) +  
  geom_linerange(aes(ymin = can_freq_lowerci, ymax = can_freq_upperci,x = index,linewidth = 3, color = as.factor(index)))+
  scale_color_manual(values = c("1"= "#E64B35","2" = "#D3D3D3","3" = "#E64B35","4"= "#D3D3D3","5" = "#D3D3D3",
                                "6" = "#D3D3D3","7" = "#4DBBD5","8" = "#4DBBD5"))+
  geom_point(aes(y = can_freq_coefs)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(breaks = seq(1,8,by=1))+
  coord_flip() + theme_classic(base_size = 29) +theme(legend.position = "none")+ labs(y = "", x = "") +ylim(.5, 1.5)
p3a
ggsave(file = "Fig3a_weighted.png", p3a, width = 8, height = 7)


## Figure 4
##### Depression adjusted and alc cann unique over each other
#alc and cann use together
fit<-glmer(suic_ideate ~ scale(Alc_qnt_freq)+scale(Can_freq_dur) +scale(Legal_Med_GroundTruth)+scale(Legal_Rec_GroundTruth)+ scale(female)+scale(adult_pop)+
             scale(Race_WhNWh2)+  scale(demo4) + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+scale(dep_est)+
             (1|Legal_Med_GroundTruth)+(1|Legal_Rec_GroundTruth), data=df_ifip2_complete,family=binomial,weights = weight_nogun)
summary(fit)

exp(0.113046) # 1.119683 odds ratio for alcohol
exp(0.124134) # 1.132168 odds ratio for cannabis

can_freq_coefs <- exp(as.numeric(coefficients(fit)$Legal_Med_GroundTruth[1,c(2,3,6:13)]))  
a<- confint(fit, method="Wald")
(can_freq_lowerci <- exp(a[c(4,5,8:15),1]))
can_freq_upperci <- exp(a[c(4,5,8:15),2])
can_freq_regressionresults2 <- as.data.frame(cbind(can_freq_coefs, can_freq_lowerci, can_freq_upperci))
can_freq_regressionresults2$index <- 10:1
p4b<-ggplot(can_freq_regressionresults2) + aes(x = index) +  
  geom_linerange(aes(ymin = can_freq_lowerci, ymax = can_freq_upperci,x = index,linewidth = 3, color = as.factor(index)))+
  scale_color_manual(values = c("1"= "#4DBBD5","2"= "#D3D3D3","3" = "#D3D3D3","4" = "#E64B35","5"= "#4DBBD5","6" = "#4DBBD5",
                                "7" = "#D3D3D3","8" = "#D3D3D3","9" = "#4DBBD5","10" = "#4DBBD5"))+
  geom_point(aes(y = can_freq_coefs)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(breaks = seq(1,10,by=1))+
  scale_y_continuous(breaks = c(.5,.7,1,1.3,2,3),labels = function(x) round(x, digits = 1))+
  coord_flip() + theme_classic(base_size = 29) +theme(legend.position = "none")+ labs(y = "", x = "") 
p4b
ggsave(file = "Fig4b_weighted_subsadj.png", p4b, width = 12, height = 7)


#### average marginal effects 
mmmcann <- glmmTMB(Can_freq_dur ~  cann_c + scale(demo2)+scale(adult_pop)+
                     scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                     scale(neighborcan1)
                   + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth)
                   , data=df_ifip2,family=nbinom2,weights = weight_nogun)
# For the change in the expected count:
avg_slopes(mmmcann, variables = "cann_c")
(0.0679/sd(df_ifip2$cann_c))*100 # average marginal effects for 1% higher odds of 4 more hours of high per week
1/2.749149 # turn above into number of outlets for 1%

mmmalc <- glm.nb(Alc_qnt_freq ~  1 + AlcoholCount + demo2+adult_pop+Race_WhNWh +  demo4+ Age_cat4 + Education_cat4 + Income_cat4, 
                 data = df_ifip2, weights = weight_nogun)

# For the change in the expected count:
avg_slopes(mmmalc, variables = "AlcoholCount")
(0.013/sd(df_ifip2$Alc_qnt_freq))*100 # average marginal effects for 1% higher odds of 6 more drinks per week
1/0.457 # turn above into number of outlets for 1%

################################################
### cann suic indirect models (weighted with hypothesized configuration)
mediation_func2 <- function(data){
  resampled_data = data %>%
    sample_frac(1, replace=TRUE)
  lmapath_resampled <-glmmTMB(Can_freq_dur ~  scale(cann_c) + scale(demo2)+scale(adult_pop)+
                                      scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                                      scale(neighborcan1)
                                    + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth)
                                    , data=resampled_data,family=nbinom2,weights = weight_nogun)
  
  
  
  lmbpath_resampled <- glmer(suic_ideate ~ scale(Can_freq_dur) +scale(cann_c)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=binomial,weights = weight_nogun)
  
  
  
  lmnomed_resampled <- glmer(suic_ideate ~ scale(cann_c)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=binomial,weights = weight_nogun)
  
  a0 <- coef(lmapath_resampled)
  a <- a0[1]$cond$Legal_Med_GroundTruth$`scale(cann_c)`
  b <-  coefficients(lmbpath_resampled)$Legal_Med_GroundTruth[-1]
  c <- coefficients(lmnomed_resampled)$Legal_Med_GroundTruth[-1]
  c(unname(a[1]), unname(b[1,1]), unname(c[1,1]), unname(b[1,2]))
}
counter <-0
med_coefs2 <- replicate(1000,{
  counter <<- counter + 1
  print(paste("Running iteration:", counter))
  mediation_func2(df_ifip2) 
} )

abccp2 <- as.data.frame(t(med_coefs2))
abccp2$ind <- abccp2$V1 * abccp2$V2
abccp2$dir <- abccp2$V3 - abccp2$V4
write.csv(abccp2,'~/boots_cann_suicOutcome_weighted.csv')


### cann suic indirect models (weighted with reversed configuration)
mediation_func2 <- function(data){
  resampled_data = data %>%
    sample_frac(1, replace=TRUE)
  lmapath_resampled <-glmmTMB(Can_freq_dur ~  scale(suic_ideate) + scale(demo2)+scale(adult_pop)+
                                scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                                scale(neighborcan1)
                              + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth)
                              , data=resampled_data,family=nbinom2,weights = weight_nogun)
  
  
  
  lmbpath_resampled <- glmmTMB(cann_c ~ scale(Can_freq_dur) +scale(suic_ideate)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=nbinom2,weights = weight_nogun)
  
  
  
  lmnomed_resampled <- glmmTMB(cann_c ~ scale(suic_ideate)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=nbinom2,weights = weight_nogun)
  
  a0 <- coef(lmapath_resampled)
  a <- a0[1]$cond$Legal_Med_GroundTruth$`scale(suic_ideate)`
  b0 <- coef(lmbpath_resampled)
  b <- b0[1]$cond$Legal_Med_GroundTruth$`scale(Can_freq_dur)`
  cpri <- b0[1]$cond$Legal_Med_GroundTruth$`scale(suic_ideate)`
  c0 <- coef(lmnomed_resampled)
  c <- c0[1]$cond$Legal_Med_GroundTruth$`scale(suic_ideate)`
  c(unname(a[1]), unname(b[1]), unname(c[1]), unname(cpri[1]))
}
counter <-0
med_coefs2 <- replicate(1000,{
  counter <<- counter + 1
  print(paste("Running iteration:", counter))
  mediation_func2(df_ifip2) 
} )

abccp2 <- as.data.frame(t(med_coefs2))
abccp2$ind <- abccp2$V1 * abccp2$V2
abccp2$dir <- abccp2$V3 - abccp2$V4
write.csv(abccp2,'boots_cann_outletsOutcome_weighted.csv')


### cann suic indirect models (weighted with suic as mediator)
mediation_func2 <- function(data){
  resampled_data = data %>%
    sample_frac(1, replace=TRUE)
  lmapath_resampled <- glmer(suic_ideate ~  scale(cann_c) + scale(demo2)+scale(adult_pop)+
                                scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                                scale(neighborcan1)
                              + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth)
                              , data=resampled_data,family=binomial,weights = weight_nogun)
  
  
  
  lmbpath_resampled <- glmmTMB(Can_freq_dur ~ scale(suic_ideate) +scale(cann_c)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=nbinom2,weights = weight_nogun)
  
  
  
  lmnomed_resampled <- glmmTMB(Can_freq_dur ~ scale(cann_c)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=nbinom2,weights = weight_nogun)
  
  a <-  coefficients(lmapath_resampled)$Legal_Med_GroundTruth[-1]
  b0 <- coef(lmbpath_resampled)
  b <- b0[1]$cond$Legal_Med_GroundTruth$`scale(suic_ideate)`
  cpri <- b0[1]$cond$Legal_Med_GroundTruth$`scale(cann_c)`
  c0 <- coef(lmnomed_resampled)
  c <- c0[1]$cond$Legal_Med_GroundTruth$`scale(cann_c)`
  

  c(unname(a[1,1]), unname(b[1]), unname(c[1]), unname(cpri[1]))
}
counter <-0
med_coefs2 <- replicate(1000,{
  counter <<- counter + 1
  print(paste("Running iteration:", counter))
  mediation_func2(df_ifip2) 
} )

abccp2 <- as.data.frame(t(med_coefs2))
abccp2$ind <- abccp2$V1 * abccp2$V2
abccp2$dir <- abccp2$V3 - abccp2$V4
write.csv(abccp2,'boots_cann_suicMediator_weighted.csv')


### alc suic indirect models (weighted hypothesized config)
mediation_func2 <- function(data){
  resampled_data = data %>%
    sample_frac(1, replace=TRUE)
  lmapath_resampled <- glm.nb(Alc_qnt_freq ~  1 + scale(AlcoholCount) + demo2+adult_pop+Race_WhNWh +  demo4+ Age_cat4 + Education_cat4 + Income_cat4, 
                              data = resampled_data, weights = weight_nogun)
  
  
  
  lmbpath_resampled <- logistf(suic_ideate~  1 +scale(Alc_qnt_freq)+scale(AlcoholCount) + demo2+adult_pop+
                                 Race_WhNWh+  demo4 + Age_cat4 + Education_cat4 + Income_cat4, data = resampled_data,weights = weight_nogun)
  
  
  
  lmnomed_resampled <- logistf(suic_ideate~  1 +scale(AlcoholCount) + demo2+adult_pop+
                                 Race_WhNWh+  demo4 + Age_cat4 + Education_cat4 + Income_cat4, 
                               data = resampled_data,weights = weight_nogun)
  
  a <- coefficients(lmapath_resampled)[-1]
  b <-  coefficients(lmbpath_resampled)[-1]
  c <- coefficients(lmnomed_resampled)[-1]
  c(unname(a[1]), unname(b[1]), unname(c[1]), unname(b[2]))
}
counter <-0
med_coefs2 <- replicate(1000,{
  counter <<- counter + 1
  print(paste("Running iteration:", counter))
  mediation_func2(df_ifip2) 
} )

abccp2 <- as.data.frame(t(med_coefs2))

abccp2$ind <- abccp2$V1 * abccp2$V2
abccp2$dir <- abccp2$V3 - abccp2$V4
write.csv(abccp2,'~/boots_alc_suic_weighted.csv')



### alc suic indirect models (weighted suic as mediator config)
mediation_func2 <- function(data){
  resampled_data = data %>%
    sample_frac(1, replace=TRUE)
  lmapath_resampled <- logistf(suic_ideate ~  1 + scale(AlcoholCount) + demo2+adult_pop+Race_WhNWh +  demo4+ Age_cat4 + Education_cat4 + Income_cat4, 
                              data = resampled_data, weights = weight_nogun)
  
  
  
  lmbpath_resampled <- glm.nb(Alc_qnt_freq~  1 +scale(suic_ideate)+scale(AlcoholCount) + demo2+adult_pop+
                                 Race_WhNWh+  demo4 + Age_cat4 + Education_cat4 + Income_cat4, data = resampled_data,weights = weight_nogun)
  
  
  
  lmnomed_resampled <- glm.nb(Alc_qnt_freq~  1 +scale(AlcoholCount) + demo2+adult_pop+
                                 Race_WhNWh+  demo4 + Age_cat4 + Education_cat4 + Income_cat4, 
                               data = resampled_data,weights = weight_nogun)
  
  a <- coefficients(lmapath_resampled)[-1]
  b <-  coefficients(lmbpath_resampled)[-1]
  c <- coefficients(lmnomed_resampled)[-1]
  c(unname(a[1]), unname(b[1]), unname(c[1]), unname(b[2]))
}
counter <-0
med_coefs2 <- replicate(1000,{
  counter <<- counter + 1
  print(paste("Running iteration:", counter))
  mediation_func2(df_ifip2) 
} )

abccp2 <- as.data.frame(t(med_coefs2))

abccp2$ind <- abccp2$V1 * abccp2$V2
abccp2$dir <- abccp2$V3 - abccp2$V4
write.csv(abccp2,'~/boots_alc_suicMediator_weighted.csv')


### alc suic indirect models (weighted suic as predictor config)
mediation_func2 <- function(data){
  resampled_data = data %>%
    sample_frac(1, replace=TRUE)
  lmapath_resampled <- glm.nb(Alc_qnt_freq ~  1 + scale(suic_ideate) + demo2+adult_pop+Race_WhNWh +  demo4+ Age_cat4 + Education_cat4 + Income_cat4, 
                              data = resampled_data, weights = weight_nogun)
  
  
  
  lmbpath_resampled <- glm.nb(AlcoholCount~  1 +scale(Alc_qnt_freq)+scale(suic_ideate) + demo2+adult_pop+
                                 Race_WhNWh+  demo4 + Age_cat4 + Education_cat4 + Income_cat4, data = resampled_data,weights = weight_nogun)
  
  
  
  lmnomed_resampled <- glm.nb(AlcoholCount~  1 +scale(suic_ideate) + demo2+adult_pop+
                                 Race_WhNWh+  demo4 + Age_cat4 + Education_cat4 + Income_cat4, 
                               data = resampled_data,weights = weight_nogun)
  
  a <- coefficients(lmapath_resampled)[-1]
  b <-  coefficients(lmbpath_resampled)[-1]
  c <- coefficients(lmnomed_resampled)[-1]
  c(unname(a[1]), unname(b[1]), unname(c[1]), unname(b[2]))
}
counter <-0
med_coefs2 <- replicate(1000,{
  counter <<- counter + 1
  print(paste("Running iteration:", counter))
  mediation_func2(df_ifip2) 
} )

abccp2 <- as.data.frame(t(med_coefs2))

abccp2$ind <- abccp2$V1 * abccp2$V2
abccp2$dir <- abccp2$V3 - abccp2$V4
write.csv(abccp2,'~/boots_alc_suicPredictor_weighted.csv')


### cann suic indirect models (weighted wtih depression and acl use and count)
pcor.test(df_ifip2_complete$cann_c,df_ifip2_complete$AlcoholCount,df_ifip2_complete$adult_pop)
cor.test(df_ifip2_complete$Alc_qnt_freq,df_ifip2_complete$Can_freq_dur)


mediation_func2 <- function(data){
  resampled_data = data %>%
    sample_frac(1, replace=TRUE)
  lmapath_resampled <-glmmTMB(Can_freq_dur ~  scale(cann_c) + scale(AlcoholCount)+scale(Alc_qnt_freq)+scale(demo2)+scale(adult_pop)+
                                scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                                scale(neighborcan1)+dep_est
                              + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth)
                              , data=resampled_data,family=nbinom2,weights = weight_nogun)
  
  
  
  lmbpath_resampled <- glmer(suic_ideate ~ scale(Can_freq_dur) +scale(cann_c)+ scale(AlcoholCount)+scale(Alc_qnt_freq)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)+dep_est
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=binomial,weights = weight_nogun)
  
  
  
  lmnomed_resampled <- glmer(suic_ideate ~ scale(cann_c)+ scale(AlcoholCount)+scale(Alc_qnt_freq)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)+dep_est
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=binomial,weights = weight_nogun)
  
  a0 <- coef(lmapath_resampled)
  a <- a0[1]$cond$Legal_Med_GroundTruth$`scale(cann_c)`
  b <-  coefficients(lmbpath_resampled)$Legal_Med_GroundTruth[-1]
  c <- coefficients(lmnomed_resampled)$Legal_Med_GroundTruth[-1]
  c(unname(a[1]), unname(b[1,1]), unname(c[1,1]), unname(b[1,2]))
}
counter <-0
med_coefs2 <- replicate(1000,{
  counter <<- counter + 1
  print(paste("Running iteration:", counter))
  mediation_func2(df_ifip2) 
} )

abccp2 <- as.data.frame(t(med_coefs2))
abccp2$ind <- abccp2$V1 * abccp2$V2
abccp2$dir <- abccp2$V3 - abccp2$V4
write.csv(abccp2,'~/boots_cann_suic_weighted_withDepressionandalc2.csv')


### alc suic indirect models (weighted) with depression and cann use and caan count
mediation_func2 <- function(data){
  resampled_data = data %>%
    sample_frac(1, replace=TRUE)
  lmapath_resampled <- glm.nb(Alc_qnt_freq ~  1 + scale(AlcoholCount) + scale(cann_c) + scale(Can_freq_dur)+ demo2+adult_pop+Race_WhNWh +  demo4+ Age_cat4 + Education_cat4 + 
                                Income_cat4+dep_est, 
                              data = resampled_data, weights = weight_nogun)
  
  
  
  lmbpath_resampled <- logistf(suic_ideate~  1 +scale(Alc_qnt_freq)+scale(AlcoholCount)+ scale(cann_c) + scale(Can_freq_dur) + demo2+adult_pop+
                                 Race_WhNWh+  demo4 + Age_cat4 + Education_cat4 + Income_cat4+dep_est, data = resampled_data,weights = weight_nogun)
  
  
  
  lmnomed_resampled <- logistf(suic_ideate~  1 +scale(AlcoholCount)+ scale(cann_c) + scale(Can_freq_dur) + demo2+adult_pop+
                                 Race_WhNWh+  demo4 + Age_cat4 + Education_cat4 + Income_cat4+dep_est, 
                               data = resampled_data,weights = weight_nogun)
  
  a <- coefficients(lmapath_resampled)[-1]
  b <-  coefficients(lmbpath_resampled)[-1]
  c <- coefficients(lmnomed_resampled)[-1]
  c(unname(a[1]), unname(b[1]), unname(c[1]), unname(b[2]))
}
counter <-0
med_coefs2 <- replicate(1000,{
  counter <<- counter + 1
  print(paste("Running iteration:", counter))
  mediation_func2(df_ifip2) 
} )

abccp2 <- as.data.frame(t(med_coefs2))

abccp2$ind <- abccp2$V1 * abccp2$V2
abccp2$dir <- abccp2$V3 - abccp2$V4
write.csv(abccp2,'~/boots_alc_suic_weighted_withDepressionandcann.csv')


##### Unweighted regression (supplementary section) #############

### cann suic indirect models (unweighted)
mediation_func2 <- function(data){
  resampled_data = data %>%
    sample_frac(1, replace=TRUE)
  lmapath_resampled <-glmmTMB(Can_freq_dur ~  scale(cann_c) + scale(demo2)+scale(adult_pop)+
                                scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                                scale(neighborcan1)
                              + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth)
                              , data=resampled_data,family=nbinom2)
  
  
  
  lmbpath_resampled <- glmer(suic_ideate ~ scale(Can_freq_dur) +scale(cann_c)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=binomial)
  
  
  
  lmnomed_resampled <- glmer(suic_ideate ~ scale(cann_c)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=binomial)
  
  a0 <- coef(lmapath_resampled)
  a <- a0[1]$cond$Legal_Med_GroundTruth$`scale(cann_c)`
  b <-  coefficients(lmbpath_resampled)$Legal_Med_GroundTruth[-1]
  c <- coefficients(lmnomed_resampled)$Legal_Med_GroundTruth[-1]
  c(unname(a[1]), unname(b[1,1]), unname(c[1,1]), unname(b[1,2]))
}
counter <-0
med_coefs2 <- replicate(1000,{
  counter <<- counter + 1
  print(paste("Running iteration:", counter))
  mediation_func2(df_ifip2) 
} )

abccp2 <- as.data.frame(t(med_coefs2))
abccp2$ind <- abccp2$V1 * abccp2$V2
abccp2$dir <- abccp2$V3 - abccp2$V4
write.csv(abccp2,'~/boots_cann_suic_unweighted.csv')



### alc suic indirect models (unweighted)
mediation_func2 <- function(data){
  resampled_data = data %>%
    sample_frac(1, replace=TRUE)
  lmapath_resampled <- glm.nb(Alc_qnt_freq ~  1 + scale(AlcoholCount) + demo2+adult_pop+Race_WhNWh +  demo4+ Age_cat4 + Education_cat4 + Income_cat4, 
                              data = resampled_data)
  
  
  
  lmbpath_resampled <- logistf(suic_ideate~  1 +scale(Alc_qnt_freq)+scale(AlcoholCount) + demo2+adult_pop+
                                 Race_WhNWh+  demo4 + Age_cat4 + Education_cat4 + Income_cat4, data = resampled_data)
  
  
  
  lmnomed_resampled <- logistf(suic_ideate~  1 +scale(AlcoholCount) + demo2+adult_pop+
                                 Race_WhNWh+  demo4 + Age_cat4 + Education_cat4 + Income_cat4, 
                               data = resampled_data)
  
  a <- coefficients(lmapath_resampled)[-1]
  b <-  coefficients(lmbpath_resampled)[-1]
  c <- coefficients(lmnomed_resampled)[-1]
  c(unname(a[1]), unname(b[1]), unname(c[1]), unname(b[2]))
}
counter <-0
med_coefs2 <- replicate(1000,{
  counter <<- counter + 1
  print(paste("Running iteration:", counter))
  mediation_func2(df_ifip2) 
} )

abccp2 <- as.data.frame(t(med_coefs2))

abccp2$ind <- abccp2$V1 * abccp2$V2
abccp2$dir <- abccp2$V3 - abccp2$V4
write.csv(abccp2,'~/boots_alc_suic_unweighted.csv')


##############
#####
### Read the bootstraps and make confidence intervals and OR
abccp2 <- read.csv('~/boots_alc_suicMediator_weighted.csv')
(mean(abccp2$V1)) #   a path
(pnorm(mean(abccp2$V1)/sd(abccp2$V1),lower.tail = FALSE)) # 
exp(quantile(abccp2$V1, probs = 0.025))
exp(quantile(abccp2$V1, probs = 0.975))

(mean(abccp2$V2,na.rm = TRUE)) #  for b path
(pnorm(mean(abccp2$V2)/sd(abccp2$V2),lower.tail = FALSE)) # 
exp(quantile(abccp2$V2, probs = 0.025))
exp(quantile(abccp2$V2, probs = 0.975))


(mean(abccp2$V3,na.rm = TRUE))  #  c =  
(pnorm(mean(abccp2$V3)/sd(abccp2$V3),lower.tail = FALSE)) # 
exp(quantile(abccp2$V3, probs = 0.025))
exp(quantile(abccp2$V3, probs = 0.975))

(mean(abccp2$ind,na.rm = TRUE) ) #  a*b
(pnorm(mean(abccp2$ind,na.rm = TRUE)/sd(abccp2$ind,na.rm = TRUE),lower.tail = FALSE))  # 
length(abccp2$ind[abccp2$ind<=0])
exp(quantile(abccp2$ind, probs = 0.025,na.rm = TRUE))
exp(quantile(abccp2$ind, probs = 0.975,na.rm = TRUE))

(mean(abccp2$V4))  #  c_prime
(pnorm(mean(abccp2$V4)/sd(abccp2$V4),lower.tail = FALSE))  # 
exp(quantile(abccp2$V4, probs = 0.025))
exp(quantile(abccp2$V4, probs = 0.975))

#######
# Supplemantary Figure S2 (unweighted regressions)
df_ifip2_complete$Race_WhNWh2 <- -1*df_ifip2_complete$Race_WhNWh
fit<-logistf(suic_ideate~  1 +scale(Alc_qnt_freq) + scale(demo2)+scale(adult_pop)+
               scale(Race_WhNWh2)+  scale(demo4) + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4), 
             data = df_ifip2_complete)

summary(fit)
exp(0.159593974) # 1.173034 odds ratio for alcohol
alc_freq_coefs <- exp(as.numeric(fit$coefficients[2:9]))
alc_freq_lowerci <- exp(as.numeric(fit$ci.lower[2:9]))
alc_freq_upperci <- exp(as.numeric(fit$ci.upper[2:9]))
alc_freq_regressionresults2 <- as.data.frame(cbind(alc_freq_coefs, alc_freq_lowerci, alc_freq_upperci))
alc_freq_regressionresults2$index <- 8:1
p1a<-ggplot(alc_freq_regressionresults2) + aes(x = index) +  
  geom_linerange(aes(ymin = alc_freq_lowerci, ymax = alc_freq_upperci,x = index,linewidth = 3, color = as.factor(index)))+
  scale_color_manual(values = c("1"= "#E64B35","2" = "#D3D3D3","3" = "#E64B35","4"= "#D3D3D3","5" = "#4DBBD5",
                                "6" = "#D3D3D3","7" = "#D3D3D3","8" = "#4DBBD5"))+
  geom_point(aes(y = alc_freq_coefs)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(breaks = seq(1,8,by=1))+
  coord_flip() + theme_classic(base_size = 29) +theme(legend.position = "none")+ labs(y = "", x = "")+ylim(.5, 1.5)
p1a
ggsave(file = "FigS2a_unweighted.png", p1a, width = 8, height = 7)



df_ifip2_complete$Race_WhNWh2 <- -1*df_ifip2_complete$Race_WhNWh
fit<-glmer(suic_ideate ~ scale(Can_freq_dur) +scale(Legal_Med_GroundTruth)+scale(Legal_Rec_GroundTruth)+ scale(demo2)+scale(adult_pop)+
             scale(Race_WhNWh2)+  scale(demo4) + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+
             (1|Legal_Med_GroundTruth)+(1|Legal_Rec_GroundTruth), data=df_ifip2_complete,family=binomial)
summary(fit)
exp(0.27281) # 1.280751 odds ratio for cannabis

can_freq_coefs <- exp(as.numeric(coefficients(fit)$Legal_Med_GroundTruth[1,c(2,5:11)]))  
a<- confint(fit, method="Wald")
can_freq_lowerci <- exp(a[c(4,7:13),1])
can_freq_upperci <- exp(a[c(4,7:13),2])
can_freq_regressionresults2 <- as.data.frame(cbind(can_freq_coefs, can_freq_lowerci, can_freq_upperci))
can_freq_regressionresults2$index <- 8:1
p3a<-ggplot(can_freq_regressionresults2) + aes(x = index) +  
  geom_linerange(aes(ymin = can_freq_lowerci, ymax = can_freq_upperci,x = index,linewidth = 3, color = as.factor(index)))+
  scale_color_manual(values = c("1"= "#E64B35","2" = "#D3D3D3","3" = "#E64B35","4"= "#D3D3D3","5" = "#4DBBD5",
                                "6" = "#D3D3D3","7" = "#D3D3D3","8" = "#4DBBD5"))+
  geom_point(aes(y = can_freq_coefs)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(breaks = seq(1,8,by=1))+
  coord_flip() + theme_classic(base_size = 29) +theme(legend.position = "none")+ labs(y = "", x = "") +ylim(.5, 1.5)
p3a
ggsave(file = "FigS2b_unweighted.png", p3a, width = 8, height = 7)


## supplementary Figure S4 (unweighted with depression and alc cann)
### cann suic indirect models 
mediation_func2 <- function(data){
  resampled_data = data %>%
    sample_frac(1, replace=TRUE)
  lmapath_resampled <-glmmTMB(Can_freq_dur ~  scale(cann_c) +scale(Alc_qnt_freq)+scale(AlcoholCount)+ scale(demo2)+scale(adult_pop)+
                                scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                                scale(neighborcan1)+dep_est
                              + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth)
                              , data=resampled_data,family=nbinom2)
  
  
  
  lmbpath_resampled <- glmer(suic_ideate ~ scale(Can_freq_dur) +scale(cann_c)+scale(Alc_qnt_freq)+scale(AlcoholCount)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)+dep_est
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=binomial)
  
  
  
  lmnomed_resampled <- glmer(suic_ideate ~ scale(cann_c)+scale(Alc_qnt_freq)+scale(AlcoholCount)+scale(demo2)+scale(adult_pop)+
                               scale(Race_WhNWh)+  demo4 + scale(Age_cat4) + scale(Education_cat4) + scale(Income_cat4)+Legal_Rec_GroundTruth+
                               scale(neighborcan1)+dep_est
                             + (1|Legal_Med_GroundTruth) + (1|Legal_Rec_GroundTruth), data=resampled_data,family=binomial)
  
  a0 <- coef(lmapath_resampled)
  a <- a0[1]$cond$Legal_Med_GroundTruth$`scale(cann_c)`
  b <-  coefficients(lmbpath_resampled)$Legal_Med_GroundTruth[-1]
  c <- coefficients(lmnomed_resampled)$Legal_Med_GroundTruth[-1]
  c(unname(a[1]), unname(b[1,1]), unname(c[1,1]), unname(b[1,2]))
}
counter <-0
med_coefs2 <- replicate(1000,{
  counter <<- counter + 1
  print(paste("Running iteration:", counter))
  mediation_func2(df_ifip2) 
} )

abccp2 <- as.data.frame(t(med_coefs2))
abccp2$ind <- abccp2$V1 * abccp2$V2
abccp2$dir <- abccp2$V3 - abccp2$V4
write.csv(abccp2,'~/boots_cann_suic_unweighted_withDepressionandalc.csv')

### ########  ####   Section 4: Making the map figures (Fig 1 and supp figure S1)
################################################################################################
################################################################################################

#Making heat maps of alcohol and cannabis stores (Figure 1)

library(tidyverse)
library(readr) # Fast reading of CSV and CSV.GZ files
library(tigris) # Shapefiles of US Census boundaries
library(sf) #Spatial data
library(zippeR) # zip code crosswalks

# Shapefiles
# Getting shapefile of counties for plotting. Removing territories
counties_lowres <- counties(cb=TRUE)
counties_lowres <- counties_lowres %>% dplyr::filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS"))
st_crs(counties_lowres)
# Getting state borders
states <- states(cb = TRUE)
state_main <- states %>% dplyr::filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS"))
state_main2 <- state_main %>% shift_geometry()
# ZCTA
zc_tigris_lowres <- zctas(cb = TRUE, year = 2020)
# ZIP Code to state crosswalk
crosswalk <- zi_load_crosswalk(year = 2020)
crosswalk_nonterritories <- crosswalk %>% filter(!STATE %in% c("PR", "VI", "GU", "MP", "AS"))
# colors
inferno_colors <- c("white","#FCFFA4FF","#F1ED6FFF","#F7D340FF", "#FBB91FFF","#FCA007FF")

# States where cannabis is illegal
legal_cannabis_states <- read_csv("Ballotpedia_Legalization.csv")

#Reading in all alcohol and filtered cannabis locations
alldata_alcohol <- read_csv( "AllAlcoholLocations.csv")
alldata_cannabis_filtered <- read_csv("FilteredCannabisLocations.csv")

#SafeGraph data already has postal (zip) code included
counts_byzip_alcohol <- alldata_alcohol %>% group_by(POSTAL_CODE) %>% summarise(AlcoholCount = n())
counts_byzip_cannabis <- alldata_cannabis_filtered %>% group_by(POSTAL_CODE) %>% summarise(CannabisCount = n())

# removing territory zipcodes/states
zc_tigris_lowres2 <- zc_tigris_lowres %>% left_join(crosswalk, by = join_by("ZCTA5CE20" == "ZCTA"))
zc_tigris_lowres_mainland <- zc_tigris_lowres2 %>% filter(STATE %in% crosswalk_nonterritories$STATE)

# alcohol
zc_tigris_lowres_mainland_walcohol <- zc_tigris_lowres_mainland %>% left_join(counts_byzip_alcohol, by = join_by("ZCTA5CE20" == "POSTAL_CODE"))
zc_tigris_lowres_mainland_walcohol$AlcoholCount[is.na(zc_tigris_lowres_mainland_walcohol$AlcoholCount)] <- 0
zc_tigris_lowres_mainland_walcohol2 <- zc_tigris_lowres_mainland_walcohol %>% shift_geometry()
zc_tigris_lowres_mainland_walcohol2 <- zc_tigris_lowres_mainland_walcohol2 %>% mutate(AlcoholBinned = case_when(
  AlcoholCount == 0 ~ "Zero",
  AlcoholCount == 1 ~ "One",
  AlcoholCount == 2 ~ "Two",
  between(AlcoholCount,3,4) ~ "Three/Four", 
  between(AlcoholCount,5,10) ~ "Five-Ten",
  AlcoholCount > 10 ~ "10+"
)) %>% mutate(AlcoholBinned = factor(AlcoholBinned, levels = c("Zero","One","Two","Three/Four","Five-Ten","10+")))


plot_alcohol_zip_tigris_flipstates <- ggplot()+
  geom_sf(data = state_main2, size = .8, fill = "grey80") + #need state below to create a grey for the non-coverage of Zipcode
  geom_sf(data = zc_tigris_lowres_mainland_walcohol2, aes(fill = AlcoholBinned), color = "NA", size =.1) +
  scale_fill_manual(values = inferno_colors, name = "Alcohol Count", na.value = "grey80") + 
  geom_sf(data = state_main2, size = .8, fill = "NA") + #need state again after for the lines to show up
  theme_void()
ggsave("alcohol_zip_tigris_flip.png", plot = plot_alcohol_zip_tigris_flipstates, width = 10, height = 8)

# Cannabis
zc_tigris_lowres_mainland_wcannabis <- zc_tigris_lowres_mainland %>% left_join(counts_byzip_cannabis, by = join_by("ZCTA5CE20" == "POSTAL_CODE"))
zc_tigris_lowres_mainland_wcannabis$CannabisCount[is.na(zc_tigris_lowres_mainland_wcannabis$CannabisCount)] <- 0
zc_tigris_lowres_mainland_wcannabis2 <- zc_tigris_lowres_mainland_wcannabis %>% shift_geometry()
zc_tigris_lowres_mainland_wcannabis3 <- zc_tigris_lowres_mainland_wcannabis2 %>% mutate(CannabisBinned = case_when(
  CannabisCount == 0 ~ "Zero",
  CannabisCount == 1 ~ "One",
  CannabisCount == 2 ~ "Two",
  CannabisCount == 3 ~ "Three", 
  CannabisCount == 4 ~ "Four",
  CannabisCount > 4 ~ "Five or more"
)) %>% mutate(CannabisBinned = factor(CannabisBinned, levels = c("Zero","One","Two","Three","Four","Five or more")))

# Need crosswalk to remove illegal states
zc_tigris_lowres_mainland_wcannabis3 <- zc_tigris_lowres_mainland_wcannabis3 %>% left_join(legal_cannabis_states, by = join_by("STATE" == "STUSPS"))
zc_tigris_lowres_mainland_wcannabis3 <- zc_tigris_lowres_mainland_wcannabis3 %>% mutate(
  CannabisBinned_Legal = if_else(Legal_Med_GroundTruth == 1, CannabisBinned, NA)
)

plot_cannabis_zip_tigris_flipstates <- ggplot()+
  geom_sf(data = state_main2, size = .8, fill = "grey80") +
  geom_sf(data = zc_tigris_lowres_mainland_wcannabis3, aes(fill = CannabisBinned_Legal), color = "NA", size =.1) +
  scale_fill_manual(values = inferno_colors, name = "Cannabis Count", na.value = "grey80") + 
  geom_sf(data = state_main2, size = .8, fill = "NA") +
  theme_void()
ggsave("cannabis_zip_filtered.png", plot = plot_cannabis_zip_tigris_flipstates, width = 10, height = 8)

#All states, not just legal
state_main2 <- state_main2 %>% left_join(legal_cannabis_states)

plot_cannabis_zip_allstates <- ggplot()+
  geom_sf(data = state_main2, size = .8, fill = "grey80") + #need state below to create a grey for the non-coverage of Zipcode
  geom_sf(data = zc_tigris_lowres_mainland_wcannabis3, aes(fill = CannabisBinned), color = "NA", size =.1) +
  scale_fill_manual(values = inferno_colors, name = "Cannabis Count", na.value = "grey80", aesthetics = "fill") + 
  geom_sf(data = state_main2, size = .8, fill = "NA") +
  theme_void()
ggsave("cannabis_zip_filtered_allstates.png", plot = plot_cannabis_zip_allstates, width = 10, height = 8)


# Plotting just the zipcodes in our dataset, plotting county level centroids so need county to zcta crosswalk
# zips of participants
firearm_survey <- read_csv("Firearm suicide survey - Copy.csv")
survey_uniquezips <- unique(firearm_survey$demo14b)
sf_uniquezips <- zc_tigris_lowres_mainland %>% filter(ZCTA5CE20 %in% survey_uniquezips)
zip_centroids <- st_centroid(sf_uniquezips) #first just need unique zips
zip_centroids_unique <- zip_centroids %>% distinct(ZCTA5CE20, .keep_all = TRUE)
st_crs(zip_centroids_unique)
st_crs(counties_lowres)
zip_centroids_unique_wcounty <- st_join(zip_centroids_unique, counties_lowres)
zip_centroids_unique_wcounty_uniqcounty <- zip_centroids_unique_wcounty %>% distinct(GEOID, .keep_all = TRUE) #now going to unique counties

counties_insurvey <- counties_lowres %>% filter(GEOID %in% zip_centroids_unique_wcounty_uniqcounty$GEOID)
counties_insurvey_centroids <- st_centroid(counties_insurvey)
counties_insurvey_centroids_shifted <- counties_insurvey_centroids %>% shift_geometry()

plot_county_centroids <- ggplot() +
  geom_sf(data = state_main2, size = .8, fill = "white") +
  geom_sf(data = counties_insurvey_centroids_shifted, size = .5, color = "purple4", alpha = .5) + 
  theme_void()
ggsave("county_centroids2.png", plot = plot_county_centroids, width = 10, height = 8)

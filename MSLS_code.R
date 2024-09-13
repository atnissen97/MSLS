# ############################################################################ #
# Minority stressors and the life satisfaction of sexual minority adults       #
# Created by: Adam Nissen                                                      #
# Date updated: 09.13.2024                                                     #
# Data comes from the Generations Study: http://www.generations-study.com      #
# ############################################################################ #

################### Packages and Data ####################

library(psych)
library(dplyr)
library(nlme)
library(lme4)
library(psych)
library(naniar)
library(lavaan)
library(semTools)
library(ggplot2)
library(scales)
library(rje)
library(reshape2)
library(cowplot)
library(tidyr)
library(lme4)
library(ggeffects)
library(GPArotation)
library(e1071)
library(Hmisc)
library(ggpubr)
library(naniar)
library(tidyverse)
library(specr)
options(max.print = 9999)

load("/Users/atnissen/Desktop/Minority Stressors and LS of LGB People/ICPSR_37166/DS0007/37166-0007-Data.rda")
dat = da37166.0007

data = select(dat, c(STUDYID, COHORT,GEDUC1, GEDUC2, GEDUCATION,
                     W1AGE, W1Q20_1, W1Q20_2, W1Q20_3, W1Q20_4, W1Q20_5, W1Q20_6, W1Q20_7, 
                     W1RACE, SCREEN_RACE, W1SEX, W1GENDER, W1SEX_GENDER,W1SEXUALID, W1HINC, #demograpahics
                     W1Q186, W1Q187, W1Q188, W1Q189, W1Q190, # life satisfaction
                     W1Q40, W1Q41, W1Q42, W1Q43, W1Q44, #sexual identity centrality
                     W1Q53, W1Q54, W1Q55, W1Q56, W1Q57, W1Q58, W1Q59, #LGBT community connectedness
                     W1Q123A, W1Q123B, W1Q123C, W1Q123D, # sexual identity concealment
                     W1Q125, W1Q126, W1Q127, #felt stigma
                     W1Q128, W1Q129, W1Q130, W1Q131, W1Q132, #internalized homophobia
                     W1Q144A, W1Q144B, W1Q144C, W1Q144D, W1Q144E, W1Q144F, W1Q144G, W1Q144H, 
                     W1Q144I,#everyday discrimination
                     W1Q164A, W1Q164B, W1Q164C, W1Q164D, W1Q164E, W1Q164F, W1Q164G, W1Q164H, 
                     W1Q164I, W1Q164J, W1Q164K, W1Q164L, #social support
                     W1Q135A, W1Q135B, W1Q135C, W1Q135D, W1Q135E, W1Q135F, #victimization
                     W1Q146A, W1Q146B, W1Q146C, W1Q146D, W1Q146E, W1Q146F, W1Q146G,
                     W1Q146H, W1Q146I, W1Q146J, W1Q146K, W1Q146L # chronic strains
)) 

################### Data Prep and Recoding Values ####################

# Demographic Variables
data$cohort = recode(data$COHORT, '(1) Younger' = 1, '(2) Middle' = 2,'(3) Older' = 3)
data$education = recode(data$GEDUCATION, '(1) Less than high school diploma' = 1, 
                        '(2) High school degree or diploma' = 2, '(3) Technical/vocational school' = 3,
                        '(4) Some college' = 4, '(5) College graduate' = 5, 
                        '(6) Post graduate work or degree' = 6)
data$colldegree = recode(data$GEDUCATION, '(1) Less than high school diploma' = 0, 
                         '(2) High school degree or diploma' = 0, '(3) Technical/vocational school' = 0,
                         '(4) Some college' = 0, '(5) College graduate' = 1, 
                         '(6) Post graduate work or degree' = 1)
data$age = data$W1AGE
data$race = recode(data$SCREEN_RACE, '(1) White' = 1, '(2) Black/African American' = 2,
                   '(3) Latino/Hispanic' = 3)
data$nonwhite = recode(data$SCREEN_RACE, '(1) White' = 0, '(2) Black/African American' = 1,
                       '(3) Latino/Hispanic' = 1)
data$black = recode(data$SCREEN_RACE, '(1) White' = 0, '(2) Black/African American' = 1,
                    '(3) Latino/Hispanic' = 0)
data$latino = recode(data$SCREEN_RACE, '(1) White' = 0, '(2) Black/African American' = 0,
                     '(3) Latino/Hispanic' = 1)
data$gender = recode(data$W1SEX_GENDER, '(1) Women, non-transgender' = 1, '(2) Men, non-transgender' = 2,
                     '(3) Genderqueer non-binary, female' = 3, '(4) Genderqueer non-binary, male' = 3)
data$female = recode(data$W1SEX_GENDER, '(1) Women, non-transgender' = 1, '(2) Men, non-transgender' = 0,
                     '(3) Genderqueer non-binary, female' = 0, '(4) Genderqueer non-binary, male' = 0)
data$male = recode(data$W1SEX_GENDER, '(1) Women, non-transgender' = 0, '(2) Men, non-transgender' = 1,
                   '(3) Genderqueer non-binary, female' = 0, '(4) Genderqueer non-binary, male' = 0)
data$nonbinary = recode(data$W1SEX_GENDER, '(1) Women, non-transgender' = 0, '(2) Men, non-transgender' = 0,
                        '(3) Genderqueer non-binary, female' = 1, '(4) Genderqueer non-binary, male' = 1)
data$sexor = recode(data$W1SEXUALID, '(01) Straight/heterosexual' = 0, '(02) Lesbian' = 1, '(03) Gay' = 1,
                    '(04) Bisexual' = 2, '(05) Queer' = 3, '(06) Same-gender loving' = 3,
                    '(07) Other' = 3, '(08) Asexual spectrum' = 3, '(09) Pansexual' = 3,
                    '(10) Anti-label' = 3)
data$homosexual = recode(data$W1SEXUALID, '(01) Straight/heterosexual' = 0, '(02) Lesbian' = 1, '(03) Gay' = 1, '(04) Bisexual' = 0,
                         '(05) Queer' = 0, '(06) Same-gender loving' = 0, '(07) Other' = 0, 
                         '(08) Asexual spectrum' = 0, '(09) Pansexual' = 0, '(10) Anti-label' = 0)
data$bisexual = recode(data$W1SEXUALID, '(01) Straight/heterosexual' = 0, '(02) Lesbian' = 0, '(03) Gay' = 0, '(04) Bisexual' = 1,
                       '(05) Queer' = 0, '(06) Same-gender loving' = 0, '(07) Other' = 0, 
                       '(08) Asexual spectrum' = 0, '(09) Pansexual' = 0, '(10) Anti-label' = 0)
data$othsexor = recode(data$W1SEXUALID, '(01) Straight/heterosexual' = 0, '(02) Lesbian' = 0, '(03) Gay' = 0, '(04) Bisexual' = 0,
                       '(05) Queer' = 1, '(06) Same-gender loving' = 1, '(07) Other' = 1, 
                       '(08) Asexual spectrum' = 1, '(09) Pansexual' = 1, '(10) Anti-label' = 1)
data$income = recode(data$W1HINC, '(01) Under $720' = 1, '(02) $720 to $5,999' = 2, '(03) $6,000 to $11,999' = 3, 
                     '(04) $12,000 to $23,999' = 4, '(05) $24,000 to $35,999' = 5, '(06) $36,000 to $47,999' = 6, 
                     '(07) $48,000 to $59,999' = 7, '(08) $60,000 to $89,999' = 8, '(09) $90,000 to $119,999' = 9, 
                     '(10) $120,000 to $179,999' = 10, '(11) $180,000 to $239,999' = 11, '(12) $240,000 and over' = 12) 
#Removing people who identify as straight
straight = data[data$sexor == "0",] #n = 11
data = data[data$sexor != "0",] 

#Satisfaction with Life
data$swl_1 = recode(data$W1Q186, '(1) Strongly disagree' = 1, '(2) Moderately disagree' = 2,
                    '(3) Slightly disagree' = 3, '(4) Neither agree nor disagree' = 4,
                    '(5) Slightly agree' = 5, '(6) Moderately agree' = 6,
                    '(7) Strongly agree' = 7)
data$swl_2 = recode(data$W1Q187, '(1) Strongly disagree' = 1, '(2) Moderately disagree' = 2,
                    '(3) Slightly disagree' = 3, '(4) Neither agree nor disagree' = 4,
                    '(5) Slightly agree' = 5, '(6) Moderately agree' = 6,
                    '(7) Strongly agree' = 7)
data$swl_3 = recode(data$W1Q188, '(1) Strongly disagree' = 1, '(2) Moderately disagree' = 2,
                    '(3) Slightly disagree' = 3, '(4) Neither agree nor disagree' = 4,
                    '(5) Slightly agree' = 5, '(6) Moderately agree' = 6,
                    '(7) Strongly agree' = 7)
data$swl_4 = recode(data$W1Q189, '(1) Strongly disagree' = 1, '(2) Moderately disagree' = 2,
                    '(3) Slightly disagree' = 3, '(4) Neither agree nor disagree' = 4,
                    '(5) Slightly agree' = 5, '(6) Moderately agree' = 6,
                    '(7) Strongly agree' = 7)
data$swl_5 = recode(data$W1Q190, '(1) Strongly disagree' = 1, '(2) Moderately disagree' = 2,
                    '(3) Slightly disagree' = 3, '(4) Neither agree nor disagree' = 4,
                    '(5) Slightly agree' = 5, '(6) Moderately agree' = 6,
                    '(7) Strongly agree' = 7)

# Chronic Strains (General Stress)
data$chronic_1 = recode(data$W1Q146A, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                        '(2) Very true' = 3)
data$chronic_2 = recode(data$W1Q146B, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                        '(2) Very true' = 3)
data$chronic_3 = recode(data$W1Q146C, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                        '(2) Very true' = 3)
data$chronic_4 = recode(data$W1Q146D, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                        '(2) Very true' = 3)
data$chronic_5 = recode(data$W1Q146E, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                        '(2) Very true' = 3)
data$chronic_6 = recode(data$W1Q146F, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                        '(2) Very true' = 3)
data$chronic_7 = recode(data$W1Q146G, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                        '(2) Very true' = 3)
data$chronic_8 = recode(data$W1Q146H, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                        '(2) Very true' = 3)
data$chronic_9 = recode(data$W1Q146I, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                        '(2) Very true' = 3)
data$chronic_10 = recode(data$W1Q146J, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                         '(2) Very true' = 3)
data$chronic_11 = recode(data$W1Q146K, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                         '(2) Very true' = 3)
data$chronic_12 = recode(data$W1Q146L, '(0) Not true' = 1, '(1) Somewhat true' = 2,
                         '(2) Very true' = 3)

# Everyday Discrimination
data$dis_1 = recode(data$W1Q144A, '(1) Often' = 1, '(2) Sometimes' = 2,
                    '(3) Rarely' = 3, '(4) Never' = 4)
data$dis_2 = recode(data$W1Q144B, '(1) Often' = 1, '(2) Sometimes' = 2,
                    '(3) Rarely' = 3, '(4) Never' = 4)
data$dis_3 = recode(data$W1Q144C, '(1) Often' = 1, '(2) Sometimes' = 2,
                    '(3) Rarely' = 3, '(4) Never' = 4)
data$dis_4 = recode(data$W1Q144D, '(1) Often' = 1, '(2) Sometimes' = 2,
                    '(3) Rarely' = 3, '(4) Never' = 4)
data$dis_5 = recode(data$W1Q144E, '(1) Often' = 1, '(2) Sometimes' = 2,
                    '(3) Rarely' = 3, '(4) Never' = 4)
data$dis_6 = recode(data$W1Q144F, '(1) Often' = 1, '(2) Sometimes' = 2,
                    '(3) Rarely' = 3, '(4) Never' = 4)
data$dis_7 = recode(data$W1Q144G, '(1) Often' = 1, '(2) Sometimes' = 2,
                    '(3) Rarely' = 3, '(4) Never' = 4)
data$dis_8 = recode(data$W1Q144H, '(1) Often' = 1, '(2) Sometimes' = 2,
                    '(3) Rarely' = 3, '(4) Never' = 4)
data$dis_9 = recode(data$W1Q144I, '(1) Often' = 1, '(2) Sometimes' = 2,
                    '(3) Rarely' = 3, '(4) Never' = 4)
#reverse scoring composite so higher numbers mean for discrimination
data$dis_1_r = 5 - data$dis_1
data$dis_2_r = 5 - data$dis_2
data$dis_3_r = 5 - data$dis_3
data$dis_4_r = 5 - data$dis_4
data$dis_5_r = 5 - data$dis_5
data$dis_6_r = 5 - data$dis_6
data$dis_7_r = 5 - data$dis_7
data$dis_8_r = 5 - data$dis_8
data$dis_9_r = 5 - data$dis_9

# Victimization
data$vic_1 = recode(data$W1Q135A, '(1) Never' = 1, '(2) Once' = 2,
                    '(3) Twice' = 3, '(4) Three or more times' = 4)
data$vic_2 = recode(data$W1Q135B, '(1) Never' = 1, '(2) Once' = 2,
                    '(3) Twice' = 3, '(4) Three or more times' = 4)
data$vic_3 = recode(data$W1Q135C, '(1) Never' = 1, '(2) Once' = 2,
                    '(3) Twice' = 3, '(4) Three or more times' = 4)
data$vic_4 = recode(data$W1Q135D, '(1) Never' = 1, '(2) Once' = 2,
                    '(3) Twice' = 3, '(4) Three or more times' = 4)
data$vic_5 = recode(data$W1Q135E, '(1) Never' = 1, '(2) Once' = 2,
                    '(3) Twice' = 3, '(4) Three or more times' = 4)
data$vic_6 = recode(data$W1Q135F, '(1) Never' = 1, '(2) Once' = 2,
                    '(3) Twice' = 3, '(4) Three or more times' = 4)

#Felt Stigma
data$fs_1 = recode(data$W1Q125, '(1) Strongly disagree' = 1, '(2) Somewhat disagree' = 2,
                   '(3) Neither agree nor disagree' = 3, '(4) Somewhat agree' = 4, '(5) Strongly agree' = 5)
data$fs_2 = recode(data$W1Q126, '(1) Strongly disagree' = 1, '(2) Somewhat disagree' = 2,
                   '(3) Neither agree nor disagree' = 3, '(4) Somewhat agree' = 4, '(5) Strongly agree' = 5)
data$fs_3 = recode(data$W1Q127, '(1) Strongly disagree' = 1, '(2) Somewhat disagree' = 2,
                   '(3) Neither agree nor disagree' = 3, '(4) Somewhat agree' = 4, '(5) Strongly agree' = 5)
#reverse scoring
data$fs_2_r = 6 - data$fs_2

# Internalized Homophobia
data$ih_1 = recode(data$W1Q128, '(1) Strongly disagree' = 1, '(2) Somewhat disagree' = 2,
                   '(3) Neither agree nor disagree' = 3, '(4) Somewhat agree' = 4, '(5) Strongly agree' = 5)
data$ih_2 = recode(data$W1Q129, '(1) Strongly disagree' = 1, '(2) Somewhat disagree' = 2,
                   '(3) Neither agree nor disagree' = 3, '(4) Somewhat agree' = 4, '(5) Strongly agree' = 5)
data$ih_3 = recode(data$W1Q130, '(1) Strongly disagree' = 1, '(2) Somewhat disagree' = 2,
                   '(3) Neither agree nor disagree' = 3, '(4) Somewhat agree' = 4, '(5) Strongly agree' = 5)
data$ih_4 = recode(data$W1Q131, '(1) Strongly disagree' = 1, '(2) Somewhat disagree' = 2,
                   '(3) Neither agree nor disagree' = 3, '(4) Somewhat agree' = 4, '(5) Strongly agree' = 5)
data$ih_5 = recode(data$W1Q132, '(1) Strongly disagree' = 1, '(2) Somewhat disagree' = 2,
                   '(3) Neither agree nor disagree' = 3, '(4) Somewhat agree' = 4, '(5) Strongly agree' = 5)

#Sexual Identity Concealment
data$conceal_1 = recode(data$W1Q123A, '(1) All' = 3, '(2) Most' = 2,
                        '(3) Some' = 1, '(4) None' = 0)
data$conceal_2 = recode(data$W1Q123B, '(1) All' = 3, '(2) Most' = 2,
                        '(3) Some' = 1, '(4) None' = 0)
data$conceal_3 = recode(data$W1Q123C, '(1) All' = 3, '(2) Most' = 2,
                        '(3) Some' = 1, '(4) None' = 0)
data$conceal_4 = recode(data$W1Q123D, '(1) All' = 3, '(2) Most' = 2,
                        '(3) Some' = 1, '(4) None' = 0)
#reverse score so greater scores mean more concealment
data$conceal_1_r = 3 - data$conceal_1
data$conceal_2_r = 3 - data$conceal_2
data$conceal_3_r = 3 - data$conceal_3
data$conceal_4_r = 3 - data$conceal_4

# Perceived Social Support
data$pss_1 = recode(data$W1Q164A, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                    '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                    '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                    '(7) Very strongly agree' = 7)
data$pss_2 = recode(data$W1Q164B, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                    '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                    '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                    '(7) Very strongly agree' = 7)
data$pss_3 = recode(data$W1Q164C, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                    '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                    '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                    '(7) Very strongly agree' = 7)
data$pss_4 = recode(data$W1Q164D, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                    '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                    '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                    '(7) Very strongly agree' = 7)
data$pss_5 = recode(data$W1Q164E, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                    '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                    '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                    '(7) Very strongly agree' = 7)
data$pss_6 = recode(data$W1Q164F, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                    '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                    '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                    '(7) Very strongly agree' = 7)
data$pss_7 = recode(data$W1Q164G, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                    '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                    '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                    '(7) Very strongly agree' = 7)
data$pss_8 = recode(data$W1Q164H, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                    '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                    '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                    '(7) Very strongly agree' = 7)
data$pss_9 = recode(data$W1Q164I, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                    '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                    '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                    '(7) Very strongly agree' = 7)
data$pss_10 = recode(data$W1Q164J, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                     '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                     '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                     '(7) Very strongly agree' = 7)
data$pss_11 = recode(data$W1Q164K, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                     '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                     '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                     '(7) Very strongly agree' = 7)
data$pss_12 = recode(data$W1Q164L, '(1) Very strongly disagree' = 1, '(2) Strongly disagree' = 2,
                     '(3) Mildly disagree' = 3, '(4) Neutral' = 4,
                     '(5) Mildly agree' = 5, '(6) Strongly agree' = 6,
                     '(7) Very strongly agree' = 7)

#LGBTQ+ Community Connectedness
data$lcc_1 = recode(data$W1Q53, '(1) Agree strongly' = 1, '(2) Agree' = 2,
                    '(3) Disagree' = 3, '(4) Disagree strongly' = 4)
data$lcc_2 = recode(data$W1Q54, '(1) Agree strongly' = 1, '(2) Agree' = 2,
                    '(3) Disagree' = 3, '(4) Disagree strongly' = 4)
data$lcc_3 = recode(data$W1Q55, '(1) Agree strongly' = 1, '(2) Agree' = 2,
                    '(3) Disagree' = 3, '(4) Disagree strongly' = 4)
data$lcc_4 = recode(data$W1Q56, '(1) Agree strongly' = 1, '(2) Agree' = 2,
                    '(3) Disagree' = 3, '(4) Disagree strongly' = 4)
data$lcc_5 = recode(data$W1Q57, '(1) Agree strongly' = 1, '(2) Agree' = 2,
                    '(3) Disagree' = 3, '(4) Disagree strongly' = 4)
data$lcc_6 = recode(data$W1Q58, '(1) Agree strongly' = 1, '(2) Agree' = 2,
                    '(3) Disagree' = 3, '(4) Disagree strongly' = 4)
data$lcc_7 = recode(data$W1Q59, '(1) Agree strongly' = 1, '(2) Agree' = 2,
                    '(3) Disagree' = 3, '(4) Disagree strongly' = 4)
#reverse scoring all items so greater scores represent greater connectedness
data$lcc_1_r = 5 - data$lcc_1
data$lcc_2_r = 5 - data$lcc_2
data$lcc_3_r = 5 - data$lcc_3
data$lcc_4_r = 5 - data$lcc_4
data$lcc_5_r = 5 - data$lcc_5
data$lcc_6_r = 5 - data$lcc_6
data$lcc_7_r = 5 - data$lcc_7

#Sexual Identity Centrality
data$sic_1 = recode(data$W1Q40, '(1) Disagree strongly' = 1, '(2) Disagree' = 2,
                    '(3) Disagree somewhat' = 3, '(4) Agree somewhat' = 4,
                    '(5) Agree' = 5, '(6) Agree strongly' = 6)
data$sic_2 = recode(data$W1Q41, '(1) Disagree strongly' = 1, '(2) Disagree' = 2,
                    '(3) Disagree somewhat' = 3, '(4) Agree somewhat' = 4,
                    '(5) Agree' = 5, '(6) Agree strongly' = 6)
data$sic_3 = recode(data$W1Q42, '(1) Disagree strongly' = 1, '(2) Disagree' = 2,
                    '(3) Disagree somewhat' = 3, '(4) Agree somewhat' = 4,
                    '(5) Agree' = 5, '(6) Agree strongly' = 6)
data$sic_4 = recode(data$W1Q43, '(1) Disagree strongly' = 1, '(2) Disagree' = 2,
                    '(3) Disagree somewhat' = 3, '(4) Agree somewhat' = 4,
                    '(5) Agree' = 5, '(6) Agree strongly' = 6)
data$sic_5 = recode(data$W1Q44, '(1) Disagree strongly' = 1, '(2) Disagree' = 2,
                    '(3) Disagree somewhat' = 3, '(4) Agree somewhat' = 4,
                    '(5) Agree' = 5, '(6) Agree strongly' = 6)
data$sic_1_r = 7 - data$sic_1

################### Descriptives ####################
###### Ns ######
# Overall Sample
nrow(data) #1507
# Age Groups
table(data$COHORT)
young = data[data$COHORT == "(1) Younger",] #n Younger = 664
middle = data[data$COHORT == "(2) Middle",] #n Middle = 369
old = data[data$COHORT == "(3) Older",] #n Older = 474

###### Composites #####
data$ls = (data$swl_1 + data$swl_2 + data$swl_3 + data$swl_4 + data$swl_5) / 5
data$chronic = (data$chronic_1 + data$chronic_2 + data$chronic_3 + data$chronic_4 + data$chronic_5
                + data$chronic_6 + data$chronic_7 + data$chronic_8 + data$chronic_9 + data$chronic_10
                + data$chronic_11 + data$chronic_12) / 12

data$evdis = (data$dis_1_r + data$dis_2_r + data$dis_3_r + data$dis_4_r + data$dis_5_r + data$dis_6_r + data$dis_7_r + data$dis_8_r + data$dis_9_r) / 9
data$vic = (data$vic_1 + data$vic_2 + data$vic_3 + data$vic_4 + data$vic_5 + data$vic_6) / 6
data$feltstig = (data$fs_1 + data$fs_2_r + data$fs_3) / 3
data$conceal = (data$conceal_1_r + data$conceal_2_r + data$conceal_3_r + data$conceal_4_r) / 4
data$inthom = (data$ih_1 + data$ih_2 + data$ih_3 + data$ih_4 + data$ih_5) / 5
data$pss = (data$pss_1 + data$pss_2 + data$pss_3 + data$pss_4 + data$pss_5 + data$pss_6 + data$pss_7 + data$pss_8 + data$pss_9 
            + data$pss_10 + data$pss_11 + data$pss_12) / 12
data$lcc = (data$lcc_1_r + data$lcc_2_r + data$lcc_3_r + data$lcc_4_r + data$lcc_5_r + data$lcc_6_r + data$lcc_7_r) / 7
data$sic = (data$sic_1_r + data$sic_2 + data$sic_3 + data$sic_4 + data$sic_5) / 5

###### Demographics #####

demo = data.frame(Cohort = c("Younger", "Middle", "Older", "Overall"), 
                  Age.Mean = c(mean(young$age, na.rm = T),mean(middle$age, na.rm = T),
                               mean(old$age, na.rm = T), mean(data$age, na.rm = T)), 
                  Age.SD = c(sd(young$age, na.rm = T),sd(middle$age, na.rm = T),
                             sd(old$age, na.rm = T), sd(data$age, na.rm = T)), 
                  per.White = c(((1 - mean(young$nonwhite, na.rm = T))*100),
                                ((1 - mean(middle$nonwhite, na.rm = T))*100),
                                ((1 - mean(old$nonwhite, na.rm = T))*100),
                                ((1 - mean(data$nonwhite, na.rm = T))*100)),
                  per.Black = c(((mean(young$black, na.rm = T))*100),
                                ((mean(middle$black, na.rm = T))*100),
                                ((mean(old$black, na.rm = T))*100),
                                ((mean(data$black, na.rm = T))*100)),
                  per.latino = c(((mean(young$latino, na.rm = T))*100),
                                 ((mean(middle$latino, na.rm = T))*100),
                                 ((mean(old$latino, na.rm = T))*100),
                                 ((mean(data$latino, na.rm = T))*100)),
                  Homosexual = c((mean(young$homosexual, na.rm = T)*100),
                                 (mean(middle$homosexual, na.rm = T)*100),
                                 (mean(old$homosexual, na.rm = T)*100),
                                 (mean(data$homosexual, na.rm = T)*100)),
                  Bisexual = c((mean(young$bisexual, na.rm = T)*100), 
                               (mean(middle$bisexual, na.rm = T)*100),
                               (mean(old$bisexual, na.rm = T)*100),
                               (mean(data$bisexual, na.rm = T)*100)), 
                  OtherSexOr = c((mean(young$othsexor, na.rm = T)*100), 
                                 (mean(middle$othsexor, na.rm = T)*100),
                                 (mean(old$othsexor, na.rm = T)*100),
                                 (mean(data$othsexor, na.rm = T)*100)), 
                  Cisgender = c((1 - mean(young$nonbinary, na.rm = T))*100, 
                                (1 - mean(middle$nonbinary, na.rm = T))*100,
                                (1 - mean(old$nonbinary, na.rm = T))*100,
                                (1 - mean(data$nonbinary, na.rm = T))*100),
                  Nonbinary = c((mean(young$nonbinary, na.rm = T))*100, 
                                (mean(middle$nonbinary, na.rm = T))*100,
                                (mean(old$nonbinary, na.rm = T))*100,
                                (mean(data$nonbinary, na.rm = T))*100),
                  Male = c((mean(young$male, na.rm = T))*100, 
                           (mean(middle$male, na.rm = T))*100,
                           (mean(old$male, na.rm = T))*100,
                           (mean(data$male, na.rm = T))*100),
                  Female = c((mean(young$female, na.rm = T)*100), 
                             (mean(middle$female, na.rm = T)*100),
                             (mean(old$female, na.rm = T)*100),
                             (mean(data$female, na.rm = T)*100))) 

demographics = demo %>%                   
  mutate_if(is.numeric,
            round,
            digits = 2)
demographics

###### Summary Statistics and Internal Consistency #####

###Life satisfaction
summary(data$ls)
#Internal consistencies
psych::alpha(select(data, c(swl_1, swl_2, swl_3, swl_4, swl_5)))
omega(select(data, c(swl_1, swl_2, swl_3, swl_4, swl_5)), nfactors = 1)
fa_satisfaction_with_life <- select(data, swl_1, swl_2, swl_3, swl_4, swl_5)
fa.parallel(fa_satisfaction_with_life, fa="fa")  
fa.maximum.likelihood <- fa(fa_satisfaction_with_life, 1, iter = 25, rotate="varimax")
print(fa.maximum.likelihood, digits=2, cut=.3, sort=TRUE)

#General stress
psych::alpha(select(data, c(chronic_1, chronic_2, chronic_3, chronic_4, chronic_5, chronic_6,
                            chronic_7, chronic_8, chronic_9, chronic_10, chronic_11, chronic_12)))
omega(select(data, c(chronic_1, chronic_2, chronic_3, chronic_4, chronic_5, chronic_6,
                     chronic_7, chronic_8, chronic_9, chronic_10, chronic_11, chronic_12)),nfactors = 1)
fa_general_stress <- select(data, c(chronic_1, chronic_2, chronic_3, chronic_4, chronic_5, chronic_6,
                                    chronic_7, chronic_8, chronic_9, chronic_10, chronic_11, chronic_12))
fa.parallel(fa_general_stress, fa="fa")  
fa.maximum.likelihood <- fa(fa_general_stress, 1, iter = 25, rotate="varimax")
print(fa.maximum.likelihood, digits=2, cut=.3, sort=TRUE)

###Discrimination
summary(data$evdis)
#Internal consistencies
psych::alpha(select(data, c(dis_1_r, dis_2_r, dis_3_r, dis_4_r, dis_5_r, dis_6_r, dis_7_r, dis_8_r, dis_9_r)))
omega(select(data, c(dis_1_r, dis_2_r, dis_3_r, dis_4_r, dis_5_r, dis_6_r, dis_7_r, dis_8_r, dis_9_r)), nfactors = 1)
fa_everday_discrimination <- select(data, c(dis_1_r, dis_2_r, dis_3_r, dis_4_r, dis_5_r, dis_6_r, dis_7_r, dis_8_r, dis_9_r))
fa.parallel(fa_everday_discrimination, fa="fa")  
fa.maximum.likelihood <- fa(fa_everday_discrimination, 1, iter = 25, rotate="varimax")
print(fa.maximum.likelihood, digits=2, cut=.3, sort=TRUE)

###Victimization
summary(data$vic)
#Internal consistencies
psych::alpha(select(data, c(vic_1, vic_2, vic_3, vic_4, vic_5, vic_6)))
omega(select(data, c(vic_1, vic_2, vic_3, vic_4, vic_5, vic_6)), nfactors = 1)
fa_victimization <- select(data, c(vic_1, vic_2, vic_3, vic_4, vic_5, vic_6))
fa.parallel(fa_victimization, fa="fa")  
fa.maximum.likelihood <- fa(fa_victimization, 1, iter = 25, rotate="varimax")
print(fa.maximum.likelihood, digits=2, cut=.3, sort=TRUE)

###Felt Stigmatization
summary(data$feltstig)
#Internal consistencies
psych::alpha(select(data, c(fs_1, fs_2_r, fs_3)))
omega(select(data, c(fs_1, fs_2_r, fs_3)), nfactors = 1)
fa_stigma <- select(data, c(fs_1, fs_2_r, fs_3))
fa.parallel(fa_stigma, fa="fa")  
fa.maximum.likelihood <- fa(fa_stigma, 1, iter = 25, rotate="varimax")
print(fa.maximum.likelihood, digits=2, cut=.3, sort=TRUE)

###Internalized homophobia
summary(data$inthom)
#Internal consistencies
psych::alpha(select(data, c(ih_1, ih_2, ih_3, ih_4, ih_5)))
omega(select(data, c(ih_1, ih_2, ih_3, ih_4, ih_5)), nfactors = 1)
fa_internalized_homophobia <- select(data, c(ih_1, ih_2, ih_3, ih_4, ih_5))
fa.parallel(fa_internalized_homophobia, fa="fa")  
fa.maximum.likelihood <- fa(fa_internalized_homophobia, 1, iter = 25, rotate="varimax")
print(fa.maximum.likelihood, digits=2, cut=.3, sort=TRUE)

###Sexual identity concealment
summary(data$conceal)
#Internal consistencies
psych::alpha(select(data, c(conceal_1_r, conceal_2_r, conceal_3_r, conceal_4_r)))
omega(select(data, c(conceal_1_r, conceal_2_r, conceal_3_r, conceal_4_r)), nfactors = 1)
fa_sexual_identity_concealment <- select(data, c(conceal_1_r, conceal_2_r, conceal_3_r, conceal_4_r))
fa.parallel(fa_sexual_identity_concealment, fa="fa")  
fa.maximum.likelihood <- fa(fa_sexual_identity_concealment, 1, iter = 25, rotate="varimax")
print(fa.maximum.likelihood, digits=2, cut=.3, sort=TRUE)

###Perceived social support
summary(data$pss)
#Internal consistencies
psych::alpha(select(data, c(pss_1, pss_2, pss_3, pss_4, pss_5, pss_6, pss_7, pss_8, pss_9, pss_10, pss_11, pss_12)))
omega(select(data, c(pss_1, pss_2, pss_3, pss_4, pss_5, pss_6, pss_7, pss_8, pss_9, pss_10, pss_11, pss_12)), nfactors = 1)
fa_perceived_support <- select(data, c(pss_1, pss_2, pss_3, pss_4, pss_5, pss_6, pss_7, pss_8, pss_9, pss_10, pss_11, pss_12))
fa.parallel(fa_perceived_support, fa="fa")  
fa.maximum.likelihood <- fa(fa_perceived_support, 3, iter = 25, rotate="varimax")
print(fa.maximum.likelihood, digits=2, cut=.3, sort=TRUE)

#LGBTQ community connectedness
summary(data$lcc)
#Internal consistencies
psych::alpha(select(data, c(lcc_1_r, lcc_2_r, lcc_3_r, lcc_4_r, lcc_5_r, lcc_6_r, lcc_7_r)))
omega(select(data, c(lcc_1_r, lcc_2_r, lcc_3_r, lcc_4_r, lcc_5_r, lcc_6_r, lcc_7_r)), nfactors = 1)
fa_community_connectedness <- select(data, c(lcc_1_r, lcc_2_r, lcc_3_r, lcc_4_r, lcc_5_r, lcc_6_r, lcc_7_r))
fa.parallel(fa_community_connectedness, fa="fa")  
fa.maximum.likelihood <- fa(fa_community_connectedness, 1, iter = 25, rotate="varimax")
print(fa.maximum.likelihood, digits=2, cut=.3, sort=TRUE)

#Sexual identity centrality
summary(data$sic)
#Internal consistencies
psych::alpha(select(data, c(sic_1_r, sic_2, sic_3, sic_4, sic_5)))
omega(select(data, c(sic_1_r, sic_2, sic_3, sic_4, sic_5)), nfactors = 1)
fa_identity_centrality <- select(data, c(sic_1_r, sic_2, sic_3, sic_4, sic_5))
fa.parallel(fa_identity_centrality, fa="fa")  
fa.maximum.likelihood <- fa(fa_identity_centrality, 1, iter = 25, rotate="varimax")
print(fa.maximum.likelihood, digits=2, cut=.3, sort=TRUE)

###### Intercorrelations #####
#Source: http://www.sthda.com/english/wiki/correlation-matrix-formatting-and-visualization

intcor = dplyr::select(data, ls, chronic, evdis, feltstig, vic, conceal, 
                       inthom, pss, lcc, sic)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# Creating Correlation Matrix
setup.corr = rcorr(as.matrix(intcor), type = "pearson")

# Using flatten function created to make the correlation matrix
correlmatrix = flattenCorrMatrix(setup.corr$r,setup.corr$P)
correlmatrix[,3:4] = round(correlmatrix[,3:4], 2)
correlmatrix

#### Power ####
# For RQ1
WebPower::wp.regression(n = 1507, p1 = 6, f2 = .02)#small
WebPower::wp.regression(n = 1507, p1 = 6, f2 = .15)#medium
WebPower::wp.regression(n = 1507, p1 = 6, f2 = .35)#large
#For RQ2
WebPower::wp.regression(n = 1507, p1 = 12, f2 = .02)#small
WebPower::wp.regression(n = 1507, p1 = 12, f2 = .15)#medium
WebPower::wp.regression(n = 1507, p1 = 12, f2 = .35)#large

################### Measurement Models and Invariance Tests ###################

##### Measurement Models #####

#Life Satisfaction
MI.LS <- '
LS =~ swl_1 + swl_2 + swl_3 + swl_4 + swl_5'
syntax.LS <- measEq.syntax(configural.model = MI.LS, 
                           data = data, 
                           parameterization = "theta",
                           ID.fac = "std.lv",
                           ID.cat = "Wu.Estabrook.2016",
                           missing = "fiml")
cat(as.character(syntax.LS))
summary(syntax.LS)
mod.LS <- as.character(syntax.LS)
cat(mod.LS)
fit.LS <- cfa(mod.LS, data = data, parameterization = "theta")
lavInspect(fit.LS, "cov.lv")
summary(fit.LS, fit.measures = T)
fitmeasures(fit.LS, c("df", "cfi", "tli", "rmsea", "bic"))
# CFI = .991, RMSEA = .079 
ls.score = as.data.frame(lavPredict(fit.LS, assemble = T))
ls.ids = as.data.frame(inspect(fit.LS, "case.idx"))
ls_dat = cbind(ls.ids,ls.score)

#General Stress
MI.CHR <- '
CHR =~ chronic_1 + chronic_2 + chronic_3 + chronic_4 + chronic_5 + chronic_6 + chronic_7 
                + chronic_8 + chronic_9 + chronic_10 + chronic_11 + chronic_12'
syntax.CHR  <- measEq.syntax(configural.model = MI.CHR, 
                           data = data, 
                           parameterization = "theta",
                           ID.fac = "std.lv",
                           ID.cat = "Wu.Estabrook.2016",
                           missing = "fiml")
cat(as.character(syntax.CHR))
summary(syntax.CHR)
mod.CHR <- as.character(syntax.CHR)
cat(mod.CHR)
fit.CHR <- cfa(mod.CHR, data = data, parameterization = "theta")
lavInspect(fit.CHR, "cov.lv")
summary(fit.CHR, fit.measures = T)
fitmeasures(fit.CHR, c("df", "cfi", "tli", "rmsea", "bic"))
# CFI = .566, RMSEA = .103 

#Fit is poor. Upon looking at the items of the measure, it may not make sense to have a reflective model.
#As such we use the raw sum score for general stress from the Chronic Strains Scale.

#Discrimination
MI.DIS <- ' 
DIS =~ dis_1_r + dis_2_r + dis_3_r + dis_4_r + dis_5_r + dis_6_r
       + dis_7_r + dis_8_r + dis_9_r
'
syntax.DIS <- measEq.syntax(configural.model = MI.DIS, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.DIS))
summary(syntax.DIS)
mod.DIS <- as.character(syntax.DIS)
cat(mod.DIS)
fit.DIS <- cfa(mod.DIS, data = data, parameterization = "theta")
summary(fit.DIS, fit.measures = T)
fitmeasures(fit.DIS, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = .856, RMSEA = .168
#Checking modifications to see where misfit is occurring
modindices(fit.DIS, sort = T)
#Adding contingencies between item 1 and item 2

MI.DIS <- ' 
DIS =~ dis_1_r + dis_2_r + dis_3_r + dis_4_r + dis_5_r + dis_6_r
       + dis_7_r + dis_8_r + dis_9_r

dis_1_r ~~ dis_2_r       
'
syntax.DIS <- measEq.syntax(configural.model = MI.DIS, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.DIS))
summary(syntax.DIS)
mod.DIS <- as.character(syntax.DIS)
cat(mod.DIS)
fit.DIS <- cfa(mod.DIS, data = data, parameterization = "theta")
summary(fit.DIS, fit.measures = T)
fitmeasures(fit.DIS, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = .929, RMSEA = .121
modindices(fit.DIS, sort = T)
#Adding contingencies between item 8 and item 9
MI.DIS <- ' 
DIS =~ dis_1_r + dis_2_r + dis_3_r + dis_4_r + dis_5_r + dis_6_r
       + dis_7_r + dis_8_r + dis_9_r

dis_1_r ~~ dis_2_r   
dis_8_r ~~ dis_9_r
'
syntax.DIS <- measEq.syntax(configural.model = MI.DIS, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.DIS))
summary(syntax.DIS)
mod.DIS <- as.character(syntax.DIS)
cat(mod.DIS)
fit.DIS <- cfa(mod.DIS, data = data, parameterization = "theta")
summary(fit.DIS, fit.measures = T)
fitmeasures(fit.DIS, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = .969, RMSEA = .082
#We will accept this model given the desire to not oversaturate with contingencies
dis.score = as.data.frame(lavPredict(fit.DIS, assemble = T))
dis.ids = as.data.frame(inspect(fit.DIS, "case.idx"))
dis_dat = cbind(dis.ids,dis.score)

#Victimization
MI.VIC <- '
VIC =~ vic_1 + vic_2 + vic_3 + vic_4 + vic_5 + vic_6'
syntax.VIC <- measEq.syntax(configural.model = MI.VIC, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.VIC))
summary(syntax.VIC)
mod.VIC <- as.character(syntax.VIC)
cat(mod.VIC)
fit.VIC <- cfa(mod.VIC, data = data, parameterization = "theta")
summary(fit.VIC, fit.measures = T)
fitmeasures(fit.VIC, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = .959, RMSEA = .096
#Checking modifications to see where misfit is occurring
modindices(fit.VIC, sort = T)
#Adding contingency between items 2 and 3

MI.VIC <- '
VIC =~ vic_1 + vic_2 + vic_3 + vic_4 + vic_5 + vic_6
vic_2 ~~ vic_3
'
syntax.VIC <- measEq.syntax(configural.model = MI.VIC, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.VIC))
summary(syntax.VIC)
mod.VIC <- as.character(syntax.VIC)
cat(mod.VIC)
fit.VIC <- cfa(mod.VIC, data = data, parameterization = "theta")
summary(fit.VIC, fit.measures = T)
fitmeasures(fit.VIC, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = .986, RMSEA = .060
vic.score = as.data.frame(lavPredict(fit.VIC, assemble = T))
vic.ids = as.data.frame(inspect(fit.VIC, "case.idx"))
vic_dat = cbind(vic.ids,vic.score)

# Stigmatization
MI.FS <- '
FS=~ fs_1 + fs_2_r + fs_3'
syntax.FS <- measEq.syntax(configural.model = MI.FS, 
                           data = data, 
                           parameterization = "theta",
                           ID.fac = "std.lv",
                           ID.cat = "Wu.Estabrook.2016",
                           missing = "fiml")
cat(as.character(syntax.FS))
summary(syntax.FS)
mod.FS <- as.character(syntax.FS)
cat(mod.FS)
fit.FS <- cfa(mod.FS, data = data, parameterization = "theta")
lavInspect(fit.FS, "cov.lv")
summary(fit.FS, fit.measures = T)
fitmeasures(fit.FS, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = 1.000, RMSEA = .000
fs.score = as.data.frame(lavPredict(fit.FS, assemble = T))
fs.ids = as.data.frame(inspect(fit.FS, "case.idx"))
fs_dat = cbind(fs.ids,fs.score)

#Sexual Identity Concealment
MI.CON <- '
CON =~ conceal_1_r + conceal_2_r + conceal_3_r + conceal_4_r
'
syntax.CON <- measEq.syntax(configural.model = MI.CON, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.CON))
summary(syntax.CON)
mod.CON <- as.character(syntax.CON)
cat(mod.CON)
fit.CON <- cfa(mod.CON, data = data, parameterization = "theta")
lavInspect(fit.CON, "cov.lv")
summary(fit.CON)
fitmeasures(fit.CON, c("df", "cfi", "tli", "rmsea", "bic"))
# CFI = .993, RMSEA = .080
con.score = as.data.frame(lavPredict(fit.CON, assemble = T))
con.ids = as.data.frame(inspect(fit.CON, "case.idx"))
con_dat = cbind(con.ids,con.score)

#Internalized Homophobia
MI.IH <- '
IH =~ ih_1 + ih_2 + ih_3 + ih_4 + ih_5'
syntax.IH <- measEq.syntax(configural.model = MI.IH, 
                           data = data, 
                           parameterization = "theta",
                           ID.fac = "std.lv",
                           ID.cat = "Wu.Estabrook.2016",
                           missing = "fiml")
cat(as.character(syntax.IH))
summary(syntax.IH)
mod.IH <- as.character(syntax.IH)
cat(mod.IH)
fit.IH <- cfa(mod.IH, data = data, parameterization = "theta")
summary(fit.IH)
fitmeasures(fit.IH, c("df", "cfi", "tli", "rmsea", "bic"))
# CFI = .985, RMSEA = .068
ih.score = as.data.frame(lavPredict(fit.IH, assemble = T))
ih.ids = as.data.frame(inspect(fit.IH, "case.idx"))
ih_dat = cbind(ih.ids,ih.score)

# Perceived Social Support
MI.PSS <- '
spe =~ pss_1 + pss_2 + pss_5 + pss_10
fam =~ pss_3 + pss_4 + pss_8 + pss_11
fri =~ pss_6 + pss_7 + pss_9 + pss_12


PSS =~ spe + fam + fri
'
syntax.PSS <- measEq.syntax(configural.model = MI.PSS, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.PSS))
summary(syntax.PSS)
mod.PSS <- as.character(syntax.PSS)
cat(mod.PSS)
fit.PSS <- cfa(mod.PSS, data = data, parameterization = "theta")
lavInspect(fit.PSS, "cov.lv")
summary(fit.PSS)
fitmeasures(fit.PSS, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = .968, RMSEA = .086
#Checking modifications to see where misfit is occurring
modindices(fit.PSS, sort = T)
#Adding contingency between items 1 & 2
MI.PSS <- '
spe =~ pss_1 + pss_2 + pss_5 + pss_10
fam =~ pss_3 + pss_4 + pss_8 + pss_11
fri =~ pss_6 + pss_7 + pss_9 + pss_12

pss_1 ~~  pss_2

PSS =~ spe + fam + fri
'
syntax.PSS <- measEq.syntax(configural.model = MI.PSS, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.PSS))
summary(syntax.PSS)
mod.PSS <- as.character(syntax.PSS)
cat(mod.PSS)
fit.PSS <- cfa(mod.PSS, data = data, parameterization = "theta")
lavInspect(fit.PSS, "cov.lv")
summary(fit.PSS)
fitmeasures(fit.PSS, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = .979, RMSEA = .069
pss.score = as.data.frame(lavPredict(fit.PSS, assemble = T))
pss.ids = as.data.frame(inspect(fit.PSS, "case.idx"))
pss_dat = cbind(pss.ids,pss.score[,4])

#LCC
MI.LCC <- '
LCC =~ lcc_1_r + lcc_2_r + lcc_3_r + lcc_4_r + lcc_5_r + lcc_6_r
       + lcc_7_r'
syntax.LCC <- measEq.syntax(configural.model = MI.LCC, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.LCC))
summary(syntax.LCC)
mod.LCC <- as.character(syntax.LCC)
cat(mod.LCC)
fit.LCC <- cfa(mod.LCC, data = data, parameterization = "theta")
summary(fit.LCC)
fitmeasures(fit.LCC, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = 0.891, RMSEA = 0.155 
#Checking modifications to see where misfit is occurring
modindices(fit.LCC, sort = T)
#Upon adding previous contingencies, the model still fit poorly. We examined factor loadings
#and noticed that item 6 has a factor loading of .261 in the original measurement model.
#We consequently removed item 6 from the measurement model and proceeded accordingly.
MI.LCC <- '
LCC =~ lcc_1_r + lcc_2_r + lcc_3_r + lcc_4_r + lcc_5_r + lcc_7_r
'
syntax.LCC <- measEq.syntax(configural.model = MI.LCC, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.LCC))
summary(syntax.LCC)
mod.LCC <- as.character(syntax.LCC)
cat(mod.LCC)
fit.LCC <- cfa(mod.LCC, data = data, parameterization = "theta")
summary(fit.LCC)
fitmeasures(fit.LCC, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = 0.938, RMSEA = 0.138
#Checking modifications to see where misfit is occurring
modindices(fit.LCC, sort = T)
#Adding contingency between items 1 and 3
MI.LCC <- '
LCC =~ lcc_1_r + lcc_2_r + lcc_3_r + lcc_4_r + lcc_5_r + lcc_7_r
lcc_1_r ~~ lcc_3_r
'
syntax.LCC <- measEq.syntax(configural.model = MI.LCC, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.LCC))
summary(syntax.LCC)
mod.LCC <- as.character(syntax.LCC)
cat(mod.LCC)
fit.LCC <- cfa(mod.LCC, data = data, parameterization = "theta")
summary(fit.LCC)
fitmeasures(fit.LCC, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = 0.966, RMSEA = 0.108
modindices(fit.LCC, sort = T)
#Adding contingency between items 5 and 7
MI.LCC <- '
LCC =~ lcc_1_r + lcc_2_r + lcc_3_r + lcc_4_r + lcc_5_r + lcc_7_r
lcc_1_r ~~ lcc_3_r
lcc_5_r ~~ lcc_7_r
'
syntax.LCC <- measEq.syntax(configural.model = MI.LCC, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.LCC))
summary(syntax.LCC)
mod.LCC <- as.character(syntax.LCC)
cat(mod.LCC)
fit.LCC <- cfa(mod.LCC, data = data, parameterization = "theta")
summary(fit.LCC)
fitmeasures(fit.LCC, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = 0.984, RMSEA = 0.081
#We will accept this model given the desire to not oversaturate with contingencies
lcc.score = as.data.frame(lavPredict(fit.LCC, assemble = T))
lcc.ids = as.data.frame(inspect(fit.LCC, "case.idx"))
lcc_dat = cbind(lcc.ids,lcc.score)

# Sexual Identity Centrality
MI.SIC <- '
SIC =~ sic_1_r + sic_2 + sic_3 + sic_4 + sic_5
'
syntax.SIC <- measEq.syntax(configural.model = MI.SIC, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.SIC))
summary(syntax.SIC)
mod.SIC <- as.character(syntax.SIC)
cat(mod.SIC)
fit.SIC <- cfa(mod.SIC, data = data, parameterization = "theta")
lavInspect(fit.SIC, "cov.lv")
summary(fit.SIC)
fitmeasures(fit.SIC, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = .979, RMSEA = .098
#Checking modifications to see where misfit is occurring
modindices(fit.SIC, sort = T)
#Adding a contingency between items 4 and 5
MI.SIC <- '
SIC =~ sic_1_r + sic_2 + sic_3 + sic_4 + sic_5
sic_4 ~~ sic_5
'
syntax.SIC <- measEq.syntax(configural.model = MI.SIC, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.SIC))
summary(syntax.SIC)
mod.SIC <- as.character(syntax.SIC)
cat(mod.SIC)
fit.SIC <- cfa(mod.SIC, data = data, parameterization = "theta")
lavInspect(fit.SIC, "cov.lv")
summary(fit.SIC)
fitmeasures(fit.SIC, c("df", "cfi", "tli", "rmsea", "bic"))
#CFI = .995, RMSEA = .054
sic.score = as.data.frame(lavPredict(fit.SIC, assemble = T))
sic.ids = as.data.frame(inspect(fit.SIC, "case.idx"))
sic_dat = cbind(sic.ids,sic.score)

################### Specification Curve Analyses ###################

# Selecting only the data we want for specification curve analysis models
colnames(ls_dat)[1] = "ID"
colnames(dis_dat)[1] = "ID"
colnames(vic_dat)[1] = "ID"
colnames(fs_dat)[1] = "ID"
colnames(con_dat)[1] = "ID"
colnames(ih_dat)[1] = "ID"
colnames(pss_dat)[1] = "ID"
colnames(pss_dat)[2] = "PSS"
colnames(lcc_dat)[1] = "ID"
colnames(sic_dat)[1] = "ID"

chr_dat = data %>% select(STUDYID, chronic,cohort,sexor,race) %>%
  mutate(ID = 1:1507) %>%
  rename(CHR = chronic) %>%
  select(-STUDYID)

list_dat = list(ls_dat,chr_dat,dis_dat,vic_dat,fs_dat,
                con_dat,ih_dat,pss_dat,lcc_dat,sic_dat)

spec_data = list_dat %>% purrr::reduce(full_join,by = "ID") 

# Now making interaction terms for continuous moderators
# Perceived Social Support
spec_data$chronpss = spec_data$CHR*spec_data$PSS
spec_data$dispss = spec_data$DIS*spec_data$PSS
spec_data$vicpss = spec_data$VIC*spec_data$PSS
spec_data$stigpss = spec_data$FS*spec_data$PSS
spec_data$inthompss = spec_data$IH*spec_data$PSS
spec_data$conpss = spec_data$CON*spec_data$PSS

# LGBTQ Community Connectedness
spec_data$chronlcc = spec_data$CHR*spec_data$LCC
spec_data$dislcc = spec_data$DIS*spec_data$LCC
spec_data$viclcc = spec_data$VIC*spec_data$LCC
spec_data$stiglcc = spec_data$FS*spec_data$LCC
spec_data$conlcc = spec_data$CON*spec_data$LCC
spec_data$inthomlcc = spec_data$IH*spec_data$LCC

# Sexual Identity Centrality
spec_data$chronsic = spec_data$CHR*spec_data$SIC
spec_data$dissic = spec_data$DIS*spec_data$SIC
spec_data$vicsic = spec_data$VIC*spec_data$SIC
spec_data$stigsic = spec_data$FS*spec_data$SIC
spec_data$consic = spec_data$CON*spec_data$SIC
spec_data$inthomsic = spec_data$IH*spec_data$SIC

colnames(spec_data)[2:3] = c("ls", "chronic")
colnames(spec_data)[7:14] = c("evdis", "vic", "feltstig", "conceal", 
                              "inthom", "pss", "lcc", "sic")

### RQ1. Minority stressor influence on life satisfaction ####

rq1 <- setup(data = spec_data,      
             y = "ls",            
             x = c("feltstig", "evdis", "vic", "inthom", "conceal", "chronic"),       
             model = c("lm"),
             controls = c("feltstig", "evdis", "vic", "inthom", "conceal", "chronic"))                 
results.rq1 <- specr(rq1)
data.rq1 = results.rq1[["data"]]
data.rq1$sig = ifelse(data.rq1$p.value < .05, "sig", "ns")

### RQ2. Continuous moderation variables ####
#Perceived Social Support
rq2.pss <- setup(data = spec_data,      
                 y = "ls",            
                 x = c("feltstig", "evdis", "vic", "inthom", "conceal", 
                       "stigpss", "dispss", "vicpss", "inthompss", "conpss", "pss", "chronic", "chronpss"),      
                 model = c("lm"),
                 controls = c("feltstig", "evdis", "vic", "inthom", "conceal", 
                              "stigpss", "dispss", "vicpss", "inthompss", "conpss", "pss", "chronic", "chronpss"))                 
results.pss <- specr(rq2.pss)
data.pss = results.pss[["data"]]
data.pss$sig = ifelse(data.pss$p.value < .05, "sig", "ns")

# LGBTQ+ Community Connectedness
rq2.lcc <- setup(data = spec_data,      
                 y = "ls",            
                 x = c("feltstig", "evdis", "vic", "inthom", "conceal", 
                       "stiglcc", "dislcc", "viclcc", "inthomlcc", "conlcc", "lcc", "chronic", "chronlcc"),      
                 model = c("lm"),
                 controls = c("feltstig", "evdis", "vic", "inthom", "conceal", 
                              "stiglcc", "dislcc", "viclcc", "inthomlcc", "conlcc", "lcc", "chronic", "chronlcc"))                 
results.lcc <- specr(rq2.lcc)
data.lcc = results.lcc[["data"]]
data.lcc$sig = ifelse(data.lcc$p.value < .05, "sig", "ns")

#Sexual Identity Centrality
rq2.sic <- setup(data = spec_data,      
                 y = "ls",            
                 x = c("feltstig", "evdis", "vic", "inthom", "conceal", 
                       "stigsic", "dissic", "vicsic", "inthomsic", "consic", "sic","chronic", "chronsic"),      
                 model = c("lm"),
                 controls = c("feltstig", "evdis", "vic", "inthom", "conceal", 
                              "stigsic", "dissic", "vicsic", "inthomsic", "consic", "sic", "chronic", "chronsic"))                 
results.sic <- specr(rq2.sic)
data.sic = results.sic[["data"]]
data.sic$sig = ifelse(data.sic$p.value < .05, "sig", "ns")

#####Combining and Visualizing Results####
#####RQ1#####
#Main Effects
main.rq1 = data.rq1[data.rq1$controls == "no covariates",] %>%
  select(x, estimate, conf.low, conf.high)

#Now getting effects from all models from specification curve
all.rq1 = data.rq1 %>%
  select(x, estimate, conf.low, conf.high) %>%
  group_by(x) %>%
  dplyr::summarize(m.est = mean(estimate, na.rm = T),
                   m.conf.low = mean(conf.low), 
                   m.conf.high = mean(conf.high),
                   min = min(conf.low),
                   max = max(conf.high)) 

# Checking change in R2 following including all minority stressors
gen <- data.rq1 %>% 
  filter(x == "chronic" & controls == "no covariates") %>%
  select(x, estimate, conf.low, conf.high, fit_r.squared, fit_adj.r.squared)
gen
# R2 with just general stress is .277, meaning 27.7% of the variance in LS explained by

gen1 <- data.rq1 %>%
  filter(x != "chronic" & controls != "no covariates") %>%
  filter(grepl('chronic',controls)) %>%
  select(x, controls, estimate, conf.low, conf.high, fit_r.squared, fit_adj.r.squared) %>%
  mutate(chgn_r.sqr = fit_r.squared -  gen$fit_r.squared,
         chng_r.sqr_adj = fit_adj.r.squared - gen$fit_adj.r.squared) %>%
  # group_by(x) %>%
  summarise(mean.chng = mean(chgn_r.sqr), 
            mean.chng_adj = mean(chng_r.sqr_adj),
            min.chng = min(chgn_r.sqr), 
            min.chng_adj = min(chng_r.sqr_adj),
            max.chng = max(chgn_r.sqr), 
            max.chng_adj = max(chng_r.sqr_adj)) %>%
  round(digits = 3)
gen
gen1
# Minority stressors explain about 3% addiitonal variance in life satisfaction above and beyond
# more general stress (R2 range = .3% to 4.9%)

##Visualizing effects
my_theme <- function(){
  theme_classic() + 
    theme(
      legend.position = "top"
      , legend.title = element_text(face = "bold", size = rel(1.3))
      , legend.text = element_text(face = "italic", size = rel(1.3))
      , axis.text = element_text(face = "bold", size = rel(1.5), color = "black")
      , axis.title = element_text(face = "bold", size = rel(1.5))
      , plot.title = element_text(face = "bold", size = rel(1.5), hjust = .5)
      , plot.subtitle = element_text(face = "italic", size = rel(1.7), hjust = .5)
      , strip.text = element_text(face = "bold", size = rel(1.5), color = "white")
      , strip.background = element_rect(fill = "black")
    )
}

#Main effects
main.rq1 = main.rq1 %>%
  mutate(sig = ifelse(sign(conf.low) == sign(conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic")
                           , labels = c("General", "Identity \nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized \nHomophobia",
                                        "Victimization")),
         type = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic"),
                       labels = c("General", "Internal",
                                  "External","External",
                                  "Internal",
                                  "External")))

ord <- c("Internalized \nHomophobia", "Identity \nConcealment", "Victimization"
         , "Stigmatization", "Discrimination",  "General")

main.rq1 %>%
  mutate(stress= factor(stress, ord)) %>%
  ggplot(aes(x = estimate, y = stress)) + 
  scale_fill_manual(values=c("red4", "darksalmon", "tomato2"), labels = c("General", "External", "Internal")) +
  scale_shape_manual(values = c(22), labels = "Significant") + 
  labs(x = "Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL,
       title = "RQ1: Main Effects") +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = 2, width = 0) +
  geom_point(aes(fill = type, shape = sig), size = 5) +
  guides(fill = guide_legend(nrow = 3, override.aes = list(colour = c("red4", "tomato2", "darksalmon")))) + 
  scale_x_continuous(limits = c(-2,.01))+
  my_theme()

#Adjusted effects
all.rq1 = all.rq1 %>%
  mutate(sig = ifelse(sign(min) == sign(max), "sig", "ns"))
all.rq1 = all.rq1 %>%
  mutate(sig = ifelse(sign(min) == sign(max), "sig", "ns")
         , stress = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic")
                           , labels = c("General", "Identity \nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized \nHomophobia",
                                        "Victimization")),
         type = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic"),
                       labels = c("General", "Internal",
                                  "External","External",
                                  "Internal",
                                  "External")))

all.rq1 %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = m.est, y = stress)) +
  scale_fill_manual(values=c("red4", "tomato2", "darksalmon"), labels = c("General", "External", "Internal")) +
  scale_shape_manual(values = c(21, 22), labels = c("Significant", "Non-significant")) + 
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL,
       title = "RQ1: Adjusted Effects") +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0) +
  geom_errorbar(aes(xmin = min, xmax = max), linewidth = 1, width = 0) +
  geom_point(aes(fill = type, shape = sig), size = 5) +
  guides(shape = guide_legend(nrow = 2, override.aes = list(shape= c(22,21))),
         fill = guide_legend(nrow = 3, override.aes = list(colour = c("red4", "tomato2", "darksalmon")))) + 
  scale_x_continuous(limits = c(-1.75,.25))+
  my_theme()

#Combined
gg.rq1 = main.rq1 %>%
  select(x, estimate, conf.low, conf.high) 
gg.rq1$min = NA
gg.rq1$max = NA
colnames(gg.rq1) = c("x", "m.est", "m.conf.low", "m.conf.high", "min", "max")
gg.rq1 = gg.rq1 %>%
  mutate(sig = ifelse(sign(m.conf.low) == sign(m.conf.high), "sig", "ns"), 
         stress = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic")
                         , labels = c("General", "Identity \nConcealment",
                                      "Discrimination","Stigmatization",
                                      "Internalized \nHomophobia",
                                      "Victimization")),
         type = factor(gg.rq1$x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic"),
                       labels = c("General", "Internal",
                                  "External","External",
                                  "Internal",
                                  "External")))
comp.rq1 = rbind(all.rq1, gg.rq1)

comp.rq1 = comp.rq1 %>%
  mutate(stress = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic")
                         , labels = c("General","Identity \nConcealment",
                                      "Discrimination","Stigmatization",
                                      "Internalized \nHomophobia",
                                      "Victimization")),
         effect = rep(c("Adjusted", "Main"), c(6,6)),
         effect = factor(effect, levels = c("Adjusted", "Main"), labels = c("Adjusted", "Main")))

fig1 <- comp.rq1 %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = m.est, y = stress, fill = effect)) + 
  scale_shape_manual(name = "Sig", values = c(21, 22), labels = c("Significant", "Non-significant")) + 
  scale_fill_manual(name = "Effect", values=c("darksalmon", "red4"), labels = c("Main", "Adjusted")) +
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(nrow = 2, override.aes = list(shape= c(22,21))), 
         fill = guide_legend(nrow = 2, override.aes = list(colour = c("red4", "darksalmon")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = min, xmax = max), linewidth = 1, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = effect, shape = sig), size = 5, colour = "black", position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-1.75,.1), breaks = c(-1.75, -1.25, -.75,-.25, 0)) +
  geom_hline(aes(yintercept = 5.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 4.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 3.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 2.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 1.5), linetype = "dashed", alpha = .4) +
  my_theme()
fig1

ggsave(fig1
       , file = "Users/atnissen/Desktop/Figure1.png"
       , width = 10
       , height = 8)

#####RQ2#####
#Making a large data frame with all data
rq2 = rbind(data.pss, data.lcc, data.sic)

# Gathering the data
#Main Effects
main.rq2 = rq2[rq2$controls == "no covariates",] %>%
  select(x, estimate, conf.low, conf.high,p.value) %>%
  mutate(type = rep(c("pss", "lcc","sic"), c(13,13,13)))

#Adjusted effects
all.rq2 = rq2 %>%
  select(x, estimate, conf.low, conf.high) %>%
  group_by(x) %>%
  dplyr::summarize(m.est = mean(estimate, na.rm = T),
                   m.conf.low = mean(conf.low), 
                   m.conf.high = mean(conf.high),
                   min = min(conf.low),
                   max = max(conf.high)) 

#Perceived Social Support
stigpss = rq2 %>%
  filter(grepl('stigpss',x)) %>%
  filter(grepl('feltstig', formula)) %>%
  filter(grepl('+ pss',formula)) %>%
  filter(!grepl('conpss',formula)) %>%
  filter(!grepl('chronpss',formula)) %>%
  filter(!grepl('inthompss',formula)) %>%
  filter(!grepl('dispss',formula)) %>%
  filter(!grepl('vicpss',formula)) 

evdispss = rq2 %>%
  filter(grepl('dispss',x)) %>%
  filter(grepl('evdis', formula)) %>%
  filter(grepl('+ pss',formula)) %>%
  filter(!grepl('conpss',formula)) %>%
  filter(!grepl('stigpss',formula)) %>%
  filter(!grepl('inthompss',formula)) %>%
  filter(!grepl('chronpss',formula)) %>%
  filter(!grepl('vicpss',formula)) 

vicpss = rq2 %>%
  filter(grepl('vicpss',x)) %>%
  filter(grepl('vic', formula)) %>%
  filter(grepl('+ pss',formula)) %>%
  filter(!grepl('conpss',formula)) %>%
  filter(!grepl('stigpss',formula)) %>%
  filter(!grepl('inthompss',formula)) %>%
  filter(!grepl('dispss',formula)) %>%
  filter(!grepl('chronpss',formula)) 

inthompss = rq2 %>%
  filter(grepl('inthompss',x)) %>%
  filter(grepl('inthom', formula)) %>%
  filter(grepl('+ pss',formula)) %>%
  filter(!grepl('conpss',formula)) %>%
  filter(!grepl('stigpss',formula)) %>%
  filter(!grepl('chronpss',formula)) %>%
  filter(!grepl('dispss',formula)) %>%
  filter(!grepl('vicpss',formula)) 

concealpss = rq2 %>%
  filter(grepl('conpss',x)) %>%
  filter(grepl('conceal', formula)) %>%
  filter(grepl('+ pss',formula)) %>%
  filter(!grepl('chronpss',formula)) %>%
  filter(!grepl('stigpss',formula)) %>%
  filter(!grepl('inthompss',formula)) %>%
  filter(!grepl('dispss',formula)) %>%
  filter(!grepl('vicpss',formula)) 

chronpss = rq2 %>%
  filter(grepl('chronpss',x)) %>%
  filter(grepl('chronic', formula)) %>%
  filter(grepl('+ pss',formula)) %>%
  filter(!grepl('conpss',formula)) %>%
  filter(!grepl('stigpss',formula)) %>%
  filter(!grepl('inthompss',formula)) %>%
  filter(!grepl('dispss',formula)) %>%
  filter(!grepl('vicpss',formula)) 

pss = rbind(stigpss, evdispss, vicpss, inthompss, concealpss, chronpss)

all.pss = pss %>%
  select(x, estimate, conf.low, conf.high,p.value) %>%
  group_by(x) %>%
  dplyr::summarize(m.est = mean(estimate, na.rm = T),
                   m.conf.low = mean(conf.low), 
                   m.conf.high = mean(conf.high),
                   min = min(conf.low),
                   max = max(conf.high)) 


#Main effects
main.pss = pss[c(1,33,68,133,193, 225),]
main.pss = main.pss %>%
  mutate(sig = ifelse(sign(conf.low) == sign(conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss")
                           , labels = c("General","Identity \nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized \nHomophobia",
                                        "Victimization")),
         type = factor(gg.rq1$x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic"),
                       labels = c("General", "Internal",
                                  "External","External",
                                  "Internal",
                                  "External"))) %>%
  select(x,term,estimate, conf.low, conf.high, sig, stress, type)

main.pss %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = estimate, y = stress, fill = type)) + 
  scale_shape_manual(values = c(21, 22), labels = c("Non-significant", "Signficiant")) + 
  scale_fill_manual(values=c("darkgreen", "palegreen", "green3"), labels = c("General", "External", "Internal")) +
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL,
       title = "Perceived Social Support: Main Effects") +
  guides(shape = guide_legend(nrow = 1, order = 1, override.aes = list(shape= c(21))),
         fill = guide_legend(nrow = 3, override.aes = list(colour = c("darkgreen", "green3", "palegreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = 2, width = 0) +
  geom_point(aes(shape = sig, fill = type), size = 5) +
  scale_x_continuous(limits = c(-.5,.5))+
  my_theme()

#Adjusted effects
all.pss = all.pss %>%
  mutate(sig = ifelse(sign(min) == sign(max), "sig", "ns")
         , stress = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss")
                           , labels = c("General","Identity \nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized \nHomophobia",
                                        "Victimization"))
         ,type = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss"),
                        labels = c("General", "Internal",
                                   "External","External",
                                   "Internal",
                                   "External")))

all.pss %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = m.est, y = stress, fill = type)) + 
  scale_shape_manual(values = c(21, 22), labels = c("Non-significant", "Signficiant")) + 
  scale_fill_manual(values=c("darkgreen", "palegreen", "green3"), labels = c("General", "External", "Internal")) +
  labs(x = "Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL,
       title = "Perceived Social Support: Adjusted Effects") +
  guides(shape = guide_legend(nrow = 1, order = 1, override.aes = list(shape= c(21))),
         fill = guide_legend(nrow = 3, override.aes = list(colour = c("darkgreen", "green3", "palegreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0) +
  geom_errorbar(aes(xmin = min, xmax = max), linewidth = 1, width = 0) +
  geom_point(aes(fill = type, shape = sig), size = 5, colour = "black") +
  scale_x_continuous(limits = c(-.5,.5))+
  my_theme()


gg.pss = main.pss %>%
  select(x, estimate, conf.low, conf.high) 
gg.pss$min = NA
gg.pss$max = NA
colnames(gg.pss) = c("x", "m.est", "m.conf.low", "m.conf.high", "min", "max")
gg.pss = gg.pss %>%
  mutate(sig = ifelse(sign(m.conf.low) == sign(m.conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss")
                           , labels = c("General","Identity \nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized \nHomophobia",
                                        "Victimization"))
         ,type = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss"),
                        labels = c("General", "Internal",
                                   "External","External",
                                   "Internal",
                                   "External")))

comp.pss = rbind(all.pss,gg.pss)

comp.pss = comp.pss %>%
  mutate(stress = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss")
                         , labels = c("General","Identity \nConcealment",
                                      "Discrimination","Stigmatization",
                                      "Internalized \nHomophobia",
                                      "Victimization")),
         effect = rep(c("Adjusted", "Main"), c(6,6)),
         effect = factor(effect, levels = c("Adjusted", "Main"), labels = c("Adjusted", "Main")))

mod1 <- comp.pss %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = m.est, y = stress, fill = effect)) + 
  scale_fill_manual(name = "Effect", values=c("springgreen", "darkgreen"), labels = c("Main", 'Adjusted')) +
  scale_shape_manual(name = "Sig", values = c(21, 22), labels = c("Non-significant", "Signficant")) + 
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(nrow = 2), fill = guide_legend(nrow = 2, override.aes = list(colour = c("darkgreen", "springgreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = min, xmax = max), linewidth = 1, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = effect, shape = sig), size = 5, position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.5,.5))+
  my_theme() + 
  ggtitle("Perceived Social \nSupport") +
  geom_hline(aes(yintercept = 5.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 4.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 3.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 2.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 1.5), linetype = "dashed", alpha = .4) +
  theme(plot.title = element_text(face = "bold", size = rel(1.5), hjust = .5))
mod1

#LGBTQ+ Community Connectedness
stiglcc = rq2 %>%
  filter(grepl('stiglcc',x)) %>%
  filter(grepl('feltstig', formula)) %>%
  filter(grepl('+ lcc',formula)) %>%
  filter(!grepl('conlcc',formula)) %>%
  filter(!grepl('chronlcc',formula)) %>%
  filter(!grepl('inthomlcc',formula)) %>%
  filter(!grepl('dislcc',formula)) %>%
  filter(!grepl('viclcc',formula)) 

evdislcc = rq2 %>%
  filter(grepl('dislcc',x)) %>%
  filter(grepl('evdis', formula)) %>%
  filter(grepl('+ lcc',formula)) %>%
  filter(!grepl('conlcc',formula)) %>%
  filter(!grepl('stiglcc',formula)) %>%
  filter(!grepl('inthomlcc',formula)) %>%
  filter(!grepl('chronlcc',formula)) %>%
  filter(!grepl('viclcc',formula)) 

viclcc = rq2 %>%
  filter(grepl('viclcc',x)) %>%
  filter(grepl('vic', formula)) %>%
  filter(grepl('+ lcc',formula)) %>%
  filter(!grepl('conlcc',formula)) %>%
  filter(!grepl('stiglcc',formula)) %>%
  filter(!grepl('inthomlcc',formula)) %>%
  filter(!grepl('dislcc',formula)) %>%
  filter(!grepl('chronlcc',formula)) 

inthomlcc = rq2 %>%
  filter(grepl('inthomlcc',x)) %>%
  filter(grepl('inthom', formula)) %>%
  filter(grepl('+ lcc',formula)) %>%
  filter(!grepl('conlcc',formula)) %>%
  filter(!grepl('stiglcc',formula)) %>%
  filter(!grepl('chronlcc',formula)) %>%
  filter(!grepl('dislcc',formula)) %>%
  filter(!grepl('viclcc',formula)) 

conceallcc = rq2 %>%
  filter(grepl('conlcc',x)) %>%
  filter(grepl('conceal', formula)) %>%
  filter(grepl('+ lcc',formula)) %>%
  filter(!grepl('chronlcc',formula)) %>%
  filter(!grepl('stiglcc',formula)) %>%
  filter(!grepl('inthomlcc',formula)) %>%
  filter(!grepl('dislcc',formula)) %>%
  filter(!grepl('viclcc',formula)) 

chronlcc = rq2 %>%
  filter(grepl('chronlcc',x)) %>%
  filter(grepl('chronic', formula)) %>%
  filter(grepl('+ lcc',formula)) %>%
  filter(!grepl('conlcc',formula)) %>%
  filter(!grepl('stiglcc',formula)) %>%
  filter(!grepl('inthomlcc',formula)) %>%
  filter(!grepl('dislcc',formula)) %>%
  filter(!grepl('viclcc',formula)) 

lcc = rbind(stiglcc, evdislcc, viclcc, inthomlcc, conceallcc, chronlcc)

#Main effects
main.lcc = lcc[c(1,33,68,133,193,225),]
main.lcc = main.lcc %>%
  mutate(sig = ifelse(sign(conf.low) == sign(conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronlcc", "conlcc", "dislcc", "stiglcc", "inthomlcc", "viclcc")
                           , labels = c("General", "Identity \nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized \nHomophobia",
                                        "Victimization"))
         ,type = factor(x, levels = c("chronlcc", "conlcc", "dislcc", "stiglcc", "inthomlcc", "viclcc"),
                        labels = c("General", "Internal",
                                   "External","External",
                                   "Internal",
                                   "External")))

main.lcc %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = estimate, y = stress, fill = type)) + 
  scale_shape_manual(values = c(21, 22), labels = c("Non-significant", "Signficiant")) + 
  scale_fill_manual(values=c("darkgreen", "palegreen", "green3"), labels = c("General", "External", "Internal")) +
  labs(x = "Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL,
       title = "LGBTQ+ Community Connectedness: \nMain Effects") +
  guides(shape = guide_legend(nrow = 1, order = 1, override.aes = list(shape= c(21))),
         fill = guide_legend(nrow = 3, override.aes = list(colour = c("darkgreen", "green3", "palegreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = 2, width = 0) +
  geom_point(aes(fill = type, shape = sig), size = 5) +
  scale_x_continuous(limits = c(-.5,.5))+
  my_theme()


#Adjusted effects
all.lcc = lcc %>%
  select(x, estimate, conf.low, conf.high) %>%
  group_by(x) %>%
  dplyr::summarize(m.est = mean(estimate, na.rm = T),
                   m.conf.low = mean(conf.low), 
                   m.conf.high = mean(conf.high),
                   min = min(conf.low),
                   max = max(conf.high)) 


all.lcc = all.lcc %>%
  mutate(sig = ifelse(sign(min) == sign(max), "sig", "ns")
         , stress = factor(x, levels = c("chronlcc", "conlcc", "dislcc", "stiglcc", "inthomlcc", "viclcc")
                           , labels = c("General","Identity \nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized \nHomophobia",
                                        "Victimization")),
         type = factor(all.lcc$x, levels = c("chronlcc", "conlcc", "dislcc", "stiglcc", "inthomlcc", "viclcc"),
                       labels = c("General", "Internal",
                                  "External","External",
                                  "Internal",
                                  "External")))

all.lcc %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = m.est, y = stress, fill = type)) + 
  scale_shape_manual(values = c(21, 22), labels = c("Non-significant", "Signficiant")) + 
  scale_fill_manual(values=c("darkgreen", "palegreen", "green3"), labels = c("General", "External", "Internal")) +
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL,
       title = "LGBTQ+ Community Connectedness: \nAdjusted Effects") +
  guides(shape = guide_legend(nrow = 1, order = 1, override.aes = list(shape= c(21))),
         fill = guide_legend(nrow = 3, override.aes = list(colour = c("darkgreen", "green3", "palegreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0) +
  geom_errorbar(aes(xmin = min, xmax = max), linewidth = 1, width = 0) +
  geom_point(aes(fill = type, shape = sig), size = 5) +
  scale_x_continuous(limits = c(-.5,.5))+
  my_theme()

#Combined
gg.lcc = main.lcc %>%
  select(x, estimate, conf.low, conf.high) 
gg.lcc$min = NA
gg.lcc$max = NA
colnames(gg.lcc) = c("x", "m.est", "m.conf.low", "m.conf.high", "min", "max")
gg.lcc = gg.lcc %>%
  mutate(sig = ifelse(sign(m.conf.low) == sign(m.conf.high), "sig", "ns"), 
         stress = factor(x, levels = c("chronlcc", "conlcc", "dislcc", "stiglcc", "inthomlcc", "viclcc")
                         , labels = c("General","Identity \nConcealment", 
                                      "Discrimination","Stigmatization",
                                      "Internalized \nHomophobia",
                                      "Victimization")),
         type = factor(all.lcc$x, levels = c("chronlcc", "conlcc", "dislcc", "stiglcc", "inthomlcc", "viclcc"),
                       labels = c("General", "Internal",
                                  "External","External",
                                  "Internal",
                                  "External")))
comp.lcc = rbind(all.lcc, gg.lcc)

comp.lcc = comp.lcc %>%
  mutate(stress = factor(x, levels = c("chronlcc", "conlcc", "dislcc", "stiglcc", "inthomlcc", "viclcc")
                         , labels = c("General","Identity \nConcealment",
                                      "Discrimination","Stigmatization",
                                      "Internalized \nHomophobia",
                                      "Victimization")),
         effect = rep(c("Adjusted", "Main"), c(6,6)),
         effect = factor(effect, levels = c("Adjusted", "Main"), labels = c("Adjusted", "Main")))

mod2 <- comp.lcc %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = m.est, y = stress, fill = effect)) + 
  scale_fill_manual(name = "Effect", values=c("springgreen", "darkgreen"), labels = c("Main", 'Adjusted')) +
  scale_shape_manual(name = "Sig", values = c(21, 22), labels = c("Non-significant", "Signficant")) + 
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(nrow = 2), 
         fill = guide_legend(nrow = 2, override.aes = list(colour = c("darkgreen", "springgreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = min, xmax = max), linewidth = 1, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = effect, shape = sig), size = 5, colour = "black", position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.5,.5))+
  my_theme() + 
  ggtitle("LGBTQ+ Community \nConnectedness") +
  geom_hline(aes(yintercept = 5.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 4.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 3.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 2.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 1.5), linetype = "dashed", alpha = .4) +
  theme(plot.title = element_text(face = "bold", size = rel(1.5), hjust = .5))
mod2

#Sexual Identity Centrality
stigsic = rq2 %>%
  filter(grepl('stigsic',x)) %>%
  filter(grepl('feltstig', formula)) %>%
  filter(grepl('+ sic',formula)) %>%
  filter(!grepl('consic',formula)) %>%
  filter(!grepl('chronsic',formula)) %>%
  filter(!grepl('inthomsic',formula)) %>%
  filter(!grepl('dissic',formula)) %>%
  filter(!grepl('vicsic',formula)) 

evdissic = rq2 %>%
  filter(grepl('dissic',x)) %>%
  filter(grepl('evdis', formula)) %>%
  filter(grepl('+ sic',formula)) %>%
  filter(!grepl('consic',formula)) %>%
  filter(!grepl('stigsic',formula)) %>%
  filter(!grepl('inthomsic',formula)) %>%
  filter(!grepl('chronsic',formula)) %>%
  filter(!grepl('vicsic',formula)) 

vicsic = rq2 %>%
  filter(grepl('vicsic',x)) %>%
  filter(grepl('vic', formula)) %>%
  filter(grepl('+ sic',formula)) %>%
  filter(!grepl('consic',formula)) %>%
  filter(!grepl('stigsic',formula)) %>%
  filter(!grepl('inthomsic',formula)) %>%
  filter(!grepl('dissic',formula)) %>%
  filter(!grepl('chronsic',formula)) 

inthomsic = rq2 %>%
  filter(grepl('inthomsic',x)) %>%
  filter(grepl('inthom', formula)) %>%
  filter(grepl('+ sic',formula)) %>%
  filter(!grepl('consic',formula)) %>%
  filter(!grepl('stigsic',formula)) %>%
  filter(!grepl('chronsic',formula)) %>%
  filter(!grepl('dissic',formula)) %>%
  filter(!grepl('vicsic',formula)) 

concealsic = rq2 %>%
  filter(grepl('consic',x)) %>%
  filter(grepl('conceal', formula)) %>%
  filter(grepl('+ sic',formula)) %>%
  filter(!grepl('chronsic',formula)) %>%
  filter(!grepl('stigsic',formula)) %>%
  filter(!grepl('inthomsic',formula)) %>%
  filter(!grepl('dissic',formula)) %>%
  filter(!grepl('vicsic',formula)) 

chronsic = rq2 %>%
  filter(grepl('chronsic',x)) %>%
  filter(grepl('chronic', formula)) %>%
  filter(grepl('+ sic',formula)) %>%
  filter(!grepl('consic',formula)) %>%
  filter(!grepl('stigsic',formula)) %>%
  filter(!grepl('inthomsic',formula)) %>%
  filter(!grepl('dissic',formula)) %>%
  filter(!grepl('vicsic',formula)) 

sic = rbind(stigsic, evdissic, vicsic, inthomsic, concealsic,chronsic)

#Main effects
main.sic = sic[c(1,33,68,133,193, 225),]
main.sic = main.sic %>%
  mutate(sig = ifelse(sign(conf.low) == sign(conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic")
                           , labels = c("General","Identity \nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized \nHomophobia",
                                        "Victimization"))
         ,type = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic"),
                        labels = c("General", "Internal",
                                   "External","External",
                                   "Internal",
                                   "External")))

main.sic %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = estimate, y = stress, fill = type)) + 
  scale_shape_manual(values = c(21, 22), labels = c("Non-significant", "Signficiant")) + 
  scale_fill_manual(values=c("darkgreen", "palegreen", "green3"), labels = c("General", "External", "Internal")) +
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL,
       title = "Sexual Identity Centrality: \nMain Effects") +
  guides(shape = guide_legend(nrow = 1, order = 1, override.aes = list(shape= c(21))),
         fill = guide_legend(nrow = 3, override.aes = list(colour = c("darkgreen", "green3", "palegreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = 2, width = 0) +
  geom_point(aes(fill = type, shape = sig), size = 5) +
  scale_x_continuous(limits = c(-.5,.5))+
  my_theme()

#Adjusted effects
all.sic = sic %>%
  select(x, estimate, conf.low, conf.high) %>%
  group_by(x) %>%
  dplyr::summarize(m.est = mean(estimate, na.rm = T),
                   m.conf.low = mean(conf.low), 
                   m.conf.high = mean(conf.high),
                   min = min(conf.low),
                   max = max(conf.high)) 

all.sic = all.sic %>%
  mutate(sig = ifelse(sign(min) == sign(max), "sig", "ns")
         , stress = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic")
                           , labels = c("General","Identity \nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized \nHomophobia",
                                        "Victimization"))
         ,type = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic"),
                        labels = c("General", "Internal",
                                   "External","External",
                                   "Internal",
                                   "External")))

all.sic %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = m.est, y = stress, fill = type)) + 
  scale_shape_manual(values = c(21, 22), labels = c("Non-significant", "Signficiant")) + 
  scale_fill_manual(values=c("darkgreen", "palegreen", "green3"), labels = c("General", "External", "Internal")) +
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL,
       title = "Sexual Identity Centrality: \nAdjusted Effects") +
  guides(shape = guide_legend(nrow = 1, order = 1, override.aes = list(shape= c(21))),
         fill = guide_legend(nrow = 3, override.aes = list(colour = c("darkgreen", "green3", "palegreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0) +
  geom_errorbar(aes(xmin = min, xmax = max), linewidth = 1, width = 0) +
  geom_point(aes(fill = type, shape = sig), size = 5, colour = "black") +
  scale_x_continuous(limits = c(-.5,.5))+
  my_theme()

#Combined
gg.sic = main.sic %>%
  select(x, estimate, conf.low, conf.high) 
gg.sic$min = NA
gg.sic$max = NA
colnames(gg.sic) = c("x", "m.est", "m.conf.low", "m.conf.high", "min", "max")
gg.sic = gg.sic %>%
  mutate(sig = ifelse(sign(m.conf.low) == sign(m.conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic")
                           , labels = c("General","Identity \nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized \nHomophobia",
                                        "Victimization"))
         ,type = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic"),
                        labels = c("General", "Internal",
                                   "External","External",
                                   "Internal",
                                   "External")))
comp.sic = rbind(all.sic,gg.sic)

comp.sic = comp.sic %>%
  mutate(stress = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic")
                         , labels = c("General","Identity \nConcealment",
                                      "Discrimination","Stigmatization",
                                      "Internalized \nHomophobia",
                                      "Victimization")),
         effect = rep(c("Adjusted", "Main"), c(6,6)),
         effect = factor(effect, levels = c("Adjusted", "Main"), labels = c("Adjusted", "Main")))

mod3 <- comp.sic %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = m.est, y = stress, fill = effect)) + 
  scale_fill_manual(name = "Effect", values=c("springgreen", "darkgreen"), labels = c("Main", 'Adjusted')) +
  scale_shape_manual(name = "Sig", values = c(21, 22), labels = c("Non-significant", "Signficant")) + 
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(nrow = 2), 
         fill = guide_legend(nrow = 2, override.aes = list(colour = c("darkgreen", "springgreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = min, xmax = max), linewidth = 1, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = effect, shape = sig), size = 5, position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.5,.5))+
  my_theme()+ 
  ggtitle("Sexual Identity \nCentrality") +
  geom_hline(aes(yintercept = 5.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 4.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 3.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 2.5), linetype = "dashed", alpha = .4) +
  geom_hline(aes(yintercept = 1.5), linetype = "dashed", alpha = .4) +
  theme(plot.title = element_text(face = "bold", size = rel(1.5), hjust = .5))
mod3

#Combining all moderator graphs here
gg.mods <- ggarrange(mod1, mod2, mod3, 
                     ncol =3, nrow = 1,
                     labels = c("A", "B", "C"),
                     common.legend = T)
gg.mods
ggsave(gg.mods
       , file = "Users/atnissen/Desktop/Figure2.png"
       , width = 17
       , height = 8)


# ############################################################################ #
# Minority stressors and the life satisfaction of sexual minority adults       #
# Created by: Adam Nissen                                                      #
# Date updated: 08.26.2024                                                     #
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
library(lavaan.mi)
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
                  White = c(((1 - mean(young$nonwhite, na.rm = T))*100),
                            ((1 - mean(middle$nonwhite, na.rm = T))*100),
                            ((1 - mean(old$nonwhite, na.rm = T))*100),
                            ((1 - mean(data$nonwhite, na.rm = T))*100)),
                  Black = c(((mean(young$black, na.rm = T))*100),
                            ((mean(middle$black, na.rm = T))*100),
                            ((mean(old$black, na.rm = T))*100),
                            ((mean(data$black, na.rm = T))*100)),
                  Latine = c(((mean(young$latino, na.rm = T))*100),
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
sd(data$ls, na.rm = T)
#Internal consistencies
psych::alpha(select(data, c(swl_1, swl_2, swl_3, swl_4, swl_5)))
omega(select(data, c(swl_1, swl_2, swl_3, swl_4, swl_5)), nfactors = 1)
fa_satisfaction_with_life <- data %>% filter(sexor == 1) %>% select(swl_1, swl_2, swl_3, swl_4, swl_5)
fa.parallel(fa_satisfaction_with_life, fa="fa")  
fa.maximum.likelihood <- fa(fa_satisfaction_with_life, 1, iter = 25, rotate="varimax")
print(fa.maximum.likelihood, digits=2, cut=.3, sort=TRUE)

###General stress
summary(data$chronic)
sd(data$chronic, na.rm = T)
#Internal consistencies
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
# For RQ1 and RQ3
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
# So after further review item 5 has a <.3 factor loading so removing that tiem

MI.IH <- '
IH =~ ih_1 + ih_2 + ih_3 + ih_4
'
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
# CFI = .989, RMSEA = .088
#Checking modification indices
modindices(fit.IH, sort = T)
# Adding a dependency to items 1 and 3

MI.IH <- '
IH =~ ih_1 + ih_2 + ih_3 + ih_4
ih_1 ~~ ih_3
'
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
# CFI = 1.000, RMSEA = .000

ih.score = as.data.frame(lavPredict(fit.IH, assemble = T))
ih.ids = as.data.frame(inspect(fit.IH, "case.idx"))
ih_dat = cbind(ih.ids,ih.score)

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

#General Stress
fa_general_stress <- select(data, c(chronic_1, chronic_2, chronic_3, chronic_4, chronic_5, chronic_6,
                                    chronic_7, chronic_8, chronic_9, chronic_10, chronic_11, chronic_12))
MI.GS <- '
GS =~ chronic_1 + chronic_2 + chronic_3 + chronic_4 + chronic_5 + chronic_6 
    + chronic_7 + chronic_8 + chronic_9 + chronic_10 + chronic_11 + chronic_12
'
syntax.GS <- measEq.syntax(configural.model = MI.GS, 
                            data = data, 
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            missing = "fiml")
cat(as.character(syntax.GS))
summary(syntax.GS)
mod.GS <- as.character(syntax.GS)
cat(mod.GS)
fit.GS <- cfa(mod.GS, data = data, parameterization = "theta")
lavInspect(fit.GS, "cov.lv")
summary(fit.GS)
fitmeasures(fit.GS, c("df", "cfi", "tli", "rmsea", "bic"))
# CFI = .566, RMSEA = .103
#Given the items of the measure, it may not make sense to have a reflective model.
#As such we use the raw sum score for general stress from the Chronic Strains Scale.

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

##### Measurement Invariance Models #####
#### Age #####
# Life Satisfaction
#Configural Model
syntax.LS.config.age <- measEq.syntax(configural.model = MI.LS, 
                               data = data, 
                               parameterization = "theta",
                               ID.fac = "std.lv",
                               ID.cat = "Wu.Estabrook.2016",
                               group = "COHORT",
                               group.equal = "thresholds",
                               missing = "fiml")
cat(as.character(syntax.LS.config.age))
summary(syntax.LS.config.age)
mod.LS.config.age <- as.character(syntax.LS.config.age)
cat(mod.LS.config.age)
fit.LS.config.age <- cfa(mod.LS.config.age, data = data, parameterization = "theta", group = "COHORT")
lavInspect(fit.LS.config.age, "cov.lv")
summary(fit.LS.config.age)
#Metric Model
syntax.LS.metric.age <- measEq.syntax(configural.model = MI.LS, data = data,
                               parameterization = "theta",
                               ID.fac = "std.lv", 
                               ID.cat = "Wu.Estabrook.2016",
                               group = "COHORT", 
                               group.equal = c("thresholds","loadings"),
                               missing = "fiml")

summary(syntax.LS.metric.age)
mod.LS.metric.age <- as.character(syntax.LS.metric.age)
cat(mod.LS.metric.age)
fit.LS.metric.age <- cfa(mod.LS.metric.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.LS.metric.age)

# Scalar model
syntax.LS.scalar.age <- measEq.syntax(configural.model = MI.LS, data = data,
                               parameterization = "theta",
                               ID.fac = "std.lv", 
                               ID.cat = "Wu.Estabrook.2016",
                               group = "COHORT", 
                               group.equal = c("thresholds","loadings",
                                               "intercepts"),
                               missing = "fiml")
summary(syntax.LS.scalar.age)
mod.LS.scalar.age <- as.character(syntax.LS.scalar.age)
cat(mod.LS.scalar.age)
fit.LS.scalar.age <- cfa(mod.LS.scalar.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.LS.scalar.age)

# Model fit indices 
fit.LS.age <- compareFit(fit.LS.config.age, fit.LS.metric.age, fit.LS.scalar.age)
summary(fit.LS.age) 
# Life Satisfaction is invariant across age groups.

#Pulling factor scores across age groups
ls.score.age1 = as.data.frame(lavPredict(fit.LS.scalar.age)[[1]])
ls.score.age2 = as.data.frame(lavPredict(fit.LS.scalar.age)[[2]])
ls.score.age3 = as.data.frame(lavPredict(fit.LS.scalar.age)[[3]])
ls.ids.age1  = as.data.frame(inspect(fit.LS.scalar.age, "case.idx")[[1]])
ls.ids.age2  = as.data.frame(inspect(fit.LS.scalar.age, "case.idx")[[2]])
ls.ids.age3  = as.data.frame(inspect(fit.LS.scalar.age, "case.idx")[[3]])

ls_dat.age1  = cbind(ls.ids.age1,ls.score.age1)
ls_dat.age2  = cbind(ls.ids.age2,ls.score.age2)
ls_dat.age3  = cbind(ls.ids.age3,ls.score.age3)
colnames(ls_dat.age1)[1] = "ID"
colnames(ls_dat.age2)[1] = "ID"
colnames(ls_dat.age3)[1] = "ID"
ls_dat.age1$cohort = 3
ls_dat.age2$cohort = 2
ls_dat.age3$cohort = 1
ls_dat.age = rbind(ls_dat.age1, ls_dat.age2, ls_dat.age3)

#Discrimination
#configural
syntax.DIS.config.age <- measEq.syntax(configural.model = MI.DIS, 
                                      data = data, 
                                      parameterization = "theta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT",
                                      group.equal = "thresholds",
                                      missing = "fiml")
cat(as.character(syntax.DIS.config.age))
summary(syntax.DIS.config.age)
mod.DIS.config.age <- as.character(syntax.DIS.config.age)
cat(mod.DIS.config.age)
fit.DIS.config.age <- cfa(mod.DIS.config.age, data = data, parameterization = "theta", group = "COHORT")
lavInspect(fit.DIS.config.age, "cov.lv")
summary(fit.DIS.config.age)
#metric model 
syntax.DIS.metric.age <- measEq.syntax(configural.model = MI.DIS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT", 
                                      group.equal = c("thresholds","loadings"),
                                      missing = "fiml")

summary(syntax.DIS.metric.age)
mod.DIS.metric.age <- as.character(syntax.DIS.metric.age)
cat(mod.DIS.metric.age)
fit.DIS.metric.age <- cfa(mod.DIS.metric.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.DIS.metric.age)
# Scalar model
syntax.DIS.scalar.age <- measEq.syntax(configural.model = MI.DIS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT", 
                                      group.equal = c("thresholds","loadings",
                                                      "intercepts"),
                                      missing = "fiml")
summary(syntax.DIS.scalar.age)
mod.DIS.scalar.age <- as.character(syntax.DIS.scalar.age)
cat(mod.DIS.scalar.age)
fit.DIS.scalar.age <- cfa(mod.DIS.scalar.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.DIS.scalar.age)
# Model fit indices 
fit.DIS.age <- compareFit(fit.DIS.config.age, fit.DIS.metric.age, fit.DIS.scalar.age) 
summary(fit.DIS.age) 

#from metric to scalar there is a decrease in cfi of -.012
#allowing for scalar metric invariance
lavTestScore(fit.DIS.scalar.age)
parTable(fit.DIS.scalar.age)
#seems that allowing groups to differ on item 3 may work

#Partial Scalar Invariance Model
syntax.DIS.scapart.age <- measEq.syntax(configural.model = MI.DIS, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "COHORT", 
                                       group.equal = c("thresholds","loadings",
                                                       "intercepts"),
                                       missing = "fiml",
                                       group.partial = "dis_3_r ~1")

summary(syntax.DIS.scapart.age)
mod.DIS.scapart.age <- as.character(syntax.DIS.scapart.age)
cat(mod.DIS.scapart.age)
fit.DIS.scapart.age <- cfa(mod.DIS.scapart.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.DIS.scapart.age)
#now seeing fit differences
fit.DIS.part.age <- compareFit(fit.DIS.config.age, fit.DIS.metric.age, fit.DIS.scapart.age) 
summary(fit.DIS.part.age) 
#partial scalar invariance met
dis.score.age1 = as.data.frame(lavPredict(fit.DIS.scapart.age)[[1]])
dis.score.age2 = as.data.frame(lavPredict(fit.DIS.scapart.age)[[2]])
dis.score.age3 = as.data.frame(lavPredict(fit.DIS.scapart.age)[[3]])
dis.ids.age1  = as.data.frame(inspect(fit.DIS.scapart.age, "case.idx")[[1]])
dis.ids.age2  = as.data.frame(inspect(fit.DIS.scapart.age, "case.idx")[[2]])
dis.ids.age3  = as.data.frame(inspect(fit.DIS.scapart.age, "case.idx")[[3]])

dis_dat.age1  = cbind(dis.ids.age1,dis.score.age1)
dis_dat.age2  = cbind(dis.ids.age2,dis.score.age2)
dis_dat.age3  = cbind(dis.ids.age3,dis.score.age3)
colnames(dis_dat.age1)[1] = "ID"
colnames(dis_dat.age2)[1] = "ID"
colnames(dis_dat.age3)[1] = "ID"
dis_dat.age1$cohort = 3
dis_dat.age2$cohort = 2
dis_dat.age3$cohort = 1
dis_dat.age = rbind(dis_dat.age1, dis_dat.age2, dis_dat.age3)

# Stigmatization
#Configural model
syntax.FS.config.age <- measEq.syntax(configural.model = MI.FS, 
                                      data = data, 
                                      parameterization = "theta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT",
                                      group.equal = "thresholds",
                                      missing = "fiml")
cat(as.character(syntax.FS.config.age))
summary(syntax.FS.config.age)
mod.FS.config.age <- as.character(syntax.FS.config.age)
cat(mod.FS.config.age)
fit.FS.config.age <- cfa(mod.FS.config.age, data = data, parameterization = "theta", group = "COHORT")
lavInspect(fit.FS.config.age, "cov.lv")
summary(fit.FS.config.age)
#Metric Model
syntax.FS.metric.age <- measEq.syntax(configural.model = MI.FS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT", 
                                      group.equal = c("thresholds","loadings"),
                                      missing = "fiml")

summary(syntax.FS.metric.age)
mod.FS.metric.age <- as.character(syntax.FS.metric.age)
cat(mod.FS.metric.age)
fit.FS.metric.age <- cfa(mod.FS.metric.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.FS.metric.age)
# Scalar model
syntax.FS.scalar.age <- measEq.syntax(configural.model = MI.FS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT", 
                                      group.equal = c("thresholds","loadings",
                                                      "intercepts"),
                                      missing = "fiml")

summary(syntax.FS.scalar.age)
mod.FS.scalar.age <- as.character(syntax.FS.scalar.age)
cat(mod.FS.scalar.age)
fit.FS.scalar.age <- cfa(mod.FS.scalar.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.FS.scalar.age)

# Model fit indices 
fit.FS.age <- compareFit(fit.FS.config.age, fit.FS.metric.age, fit.FS.scalar.age) 
summary(fit.FS.age) 

# From configural to metric there is a jump of .049 in RMSEA likely because of
# small degrees of freedom 
# we will allow for metric partial invariance
lavTestScore(fit.FS.metric.age)
parTable(fit.FS.metric.age)
#seems to be the the factor loading of the item 1 are not equal across groups
#we will free this parameter in a new model

#Partial Metric Invariance Model
syntax.FS.metpart.age <- measEq.syntax(configural.model = MI.FS, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "COHORT", 
                                       group.equal = c("thresholds","loadings"),
                                       missing = "fiml", group.partial = "FS =~ fs_1")

summary(syntax.FS.metpart.age)
mod.FS.metpart.age <- as.character(syntax.FS.metpart.age)
cat(mod.FS.metpart.age)
fit.FS.metpart.age <- cfa(mod.FS.metpart.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.FS.metpart.age)

#Scalar
syntax.FS.scapart.age <- measEq.syntax(configural.model = MI.FS, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "COHORT", 
                                       group.equal = c("thresholds","loadings",
                                                       "intercepts"),
                                       missing = "fiml", 
                                       group.partial = c("FS =~ fs_1",
                                                         "FS ~ FS_1"))
summary(syntax.FS.scapart.age)
mod.FS.scapart.age <- as.character(syntax.FS.scapart.age)
cat(mod.FS.scapart.age)
fit.FS.scapart.age <- cfa(mod.FS.scapart.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.FS.scapart.age)
#new comparison
fit.FS.part.age <- compareFit(fit.FS.config.age, fit.FS.metpart.age, fit.FS.scapart.age) 
summary(fit.FS.part.age) 

fs.score.age1 = as.data.frame(lavPredict(fit.FS.scapart.age)[[1]])
fs.score.age2 = as.data.frame(lavPredict(fit.FS.scapart.age)[[2]])
fs.score.age3 = as.data.frame(lavPredict(fit.FS.scapart.age)[[3]])
fs.ids.age1  = as.data.frame(inspect(fit.FS.scapart.age, "case.idx")[[1]])
fs.ids.age2  = as.data.frame(inspect(fit.FS.scapart.age, "case.idx")[[2]])
fs.ids.age3  = as.data.frame(inspect(fit.FS.scapart.age, "case.idx")[[3]])

fs_dat.age1  = cbind(fs.ids.age1,fs.score.age1)
fs_dat.age2  = cbind(fs.ids.age2,fs.score.age2)
fs_dat.age3  = cbind(fs.ids.age3,fs.score.age3)
colnames(fs_dat.age1)[1] = "ID"
colnames(fs_dat.age2)[1] = "ID"
colnames(fs_dat.age3)[1] = "ID"
fs_dat.age1$cohort = 3
fs_dat.age2$cohort = 2
fs_dat.age3$cohort = 1
fs_dat.age = rbind(fs_dat.age1, fs_dat.age2, fs_dat.age3)

# Victimization
syntax.VIC.config.age <- measEq.syntax(configural.model = MI.VIC, 
                                      data = data, 
                                      parameterization = "theta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT",
                                      group.equal = "thresholds",
                                      missing = "fiml")
cat(as.character(syntax.VIC.config.age))
summary(syntax.VIC.config.age)
mod.VIC.config.age <- as.character(syntax.VIC.config.age)
cat(mod.VIC.config.age)
fit.VIC.config.age <- cfa(mod.VIC.config.age, data = data, parameterization = "theta", group = "COHORT")
lavInspect(fit.VIC.config.age, "cov.lv")
summary(fit.VIC.config.age)
#metric model
syntax.VIC.metric.age <- measEq.syntax(configural.model = MI.VIC, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT", 
                                      group.equal = c("thresholds","loadings"),
                                      missing = "fiml")

summary(syntax.VIC.metric.age)
mod.VIC.metric.age <- as.character(syntax.VIC.metric.age)
cat(mod.VIC.metric.age)
fit.VIC.metric.age <- cfa(mod.VIC.metric.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.VIC.metric.age)

# Scalar model
syntax.VIC.scalar.age <- measEq.syntax(configural.model = MI.VIC, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT", 
                                      group.equal = c("thresholds","loadings",
                                                      "intercepts"),
                                      missing = "fiml")

summary(syntax.VIC.scalar.age)
mod.VIC.scalar.age <- as.character(syntax.VIC.scalar.age)
cat(mod.VIC.scalar.age)
fit.VIC.scalar.age <- cfa(mod.VIC.scalar.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.VIC.scalar.age)
# Model fit indices 
fit.VIC.age <- compareFit(fit.VIC.config.age, fit.VIC.metric.age, fit.VIC.scalar.age) 
summary(fit.VIC.age) 

#scalar invariance not meet as rmsea difference is |.036| and cfi is |.049| 
#so allowing for scalar invariance so checking what needs to be freed
lavTestScore(fit.VIC.scalar.age)
parTable(fit.VIC.scalar.age)
# Item 2 has highest chi-square, so freeing that parameter across groups

#Partial Scalar Invariance Model
syntax.VIC.scapart.age <- measEq.syntax(configural.model = MI.VIC, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "COHORT", 
                                       group.equal = c("thresholds","loadings",
                                                       "intercepts"),
                                       missing = "fiml",
                                       group.partial = c("vic_2 ~ 1"))

summary(syntax.VIC.scapart.age)
mod.VIC.scapart.age <- as.character(syntax.VIC.scapart.age)
cat(mod.VIC.scapart.age)
fit.VIC.scapart.age <- cfa(mod.VIC.scapart.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.VIC.scalar.age)

#checking whether freeing allowed for invariance
fit.VIC.part.age <- compareFit(fit.VIC.config.age, fit.VIC.metric.age, fit.VIC.scapart.age) 
summary(fit.VIC.part.age) 
#partial scalar invariance met
vic.score.age1 = as.data.frame(lavPredict(fit.VIC.scapart.age)[[1]])
vic.score.age2 = as.data.frame(lavPredict(fit.VIC.scapart.age)[[2]])
vic.score.age3 = as.data.frame(lavPredict(fit.VIC.scapart.age)[[3]])
vic.ids.age1  = as.data.frame(inspect(fit.VIC.scapart.age, "case.idx")[[1]])
vic.ids.age2  = as.data.frame(inspect(fit.VIC.scapart.age, "case.idx")[[2]])
vic.ids.age3  = as.data.frame(inspect(fit.VIC.scapart.age, "case.idx")[[3]])

vic_dat.age1  = cbind(vic.ids.age1,vic.score.age1)
vic_dat.age2  = cbind(vic.ids.age2,vic.score.age2)
vic_dat.age3  = cbind(vic.ids.age3,vic.score.age3)
colnames(vic_dat.age1)[1] = "ID"
colnames(vic_dat.age2)[1] = "ID"
colnames(vic_dat.age3)[1] = "ID"
vic_dat.age1$cohort = 3
vic_dat.age2$cohort = 2
vic_dat.age3$cohort = 1
vic_dat.age = rbind(vic_dat.age1, vic_dat.age2, vic_dat.age3)

#Internalized Homophobia
syntax.IH.config.age <- measEq.syntax(configural.model = MI.IH, 
                                      data = data, 
                                      parameterization = "theta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT",
                                      group.equal = "thresholds",
                                      missing = "fiml")
cat(as.character(syntax.IH.config.age))
summary(syntax.IH.config.age)
mod.IH.config.age <- as.character(syntax.IH.config.age)
cat(mod.IH.config.age)
fit.IH.config.age <- cfa(mod.IH.config.age, data = data, parameterization = "theta", group = "COHORT")
lavInspect(fit.IH.config.age, "cov.lv")
summary(fit.IH.config.age)
#metric model
syntax.IH.metric.age <- measEq.syntax(configural.model = MI.IH, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT", 
                                      group.equal = c("thresholds","loadings"),
                                      missing = "fiml")
summary(syntax.IH.metric.age)
mod.IH.metric.age <- as.character(syntax.IH.metric.age)
cat(mod.IH.metric.age)
fit.IH.metric.age <- cfa(mod.IH.metric.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.IH.metric.age)
# Scalar model
syntax.IH.scalar.age <- measEq.syntax(configural.model = MI.IH, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT", 
                                      group.equal = c("thresholds","loadings",
                                                      "intercepts"),
                                      missing = "fiml")
summary(syntax.IH.scalar.age)
mod.IH.scalar.age <- as.character(syntax.IH.scalar.age)
cat(mod.IH.scalar.age)
fit.IH.scalar.age <- cfa(mod.IH.scalar.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.IH.scalar.age)
# Model fit indices 
fit.IH.age <- compareFit(fit.IH.config.age, fit.IH.metric.age, fit.IH.scalar.age) 
summary(fit.IH.age) 
#metric invariance not met as change in rmsea is |.025| likely because of low degrees of freedom
lavTestScore(fit.IH.metric.age)
parTable(fit.IH.metric.age)
# There is nothing that seems to be super changing here so we will allow this but note as a limitation
# But there is a large jump in RMSEA for scalar invariance
lavTestScore(fit.IH.scalar.age)
parTable(fit.IH.scalar.age)
# after testing and exmining fits we will free item 1 and 3 intercepts
# scalar
syntax.IH.scapart.age <- measEq.syntax(configural.model = MI.IH, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "COHORT", 
                                       group.equal = c("thresholds","loadings",
                                                       "intercepts"),
                                       missing = "fiml", group.partial = c("ih_1 ~ 1",
                                                                           "ih_3 ~ 1"))
summary(syntax.IH.scapart.age)
mod.IH.scapart.age <- as.character(syntax.IH.scapart.age)
cat(mod.IH.scapart.age)
fit.IH.scapart.age <- cfa(mod.IH.scapart.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.IH.scapart.age)
#comparing models
fit.IH.part.age <- compareFit(fit.IH.config.age, fit.IH.metric.age, fit.IH.scapart.age) 
summary(fit.IH.part.age) 

ih.score.age1 = as.data.frame(lavPredict(fit.IH.scapart.age)[[1]])
ih.score.age2 = as.data.frame(lavPredict(fit.IH.scapart.age)[[2]])
ih.score.age3 = as.data.frame(lavPredict(fit.IH.scapart.age)[[3]])
ih.ids.age1  = as.data.frame(inspect(fit.IH.scapart.age, "case.idx")[[1]])
ih.ids.age2  = as.data.frame(inspect(fit.IH.scapart.age, "case.idx")[[2]])
ih.ids.age3  = as.data.frame(inspect(fit.IH.scapart.age, "case.idx")[[3]])

ih_dat.age1  = cbind(ih.ids.age1,ih.score.age1)
ih_dat.age2  = cbind(ih.ids.age2,ih.score.age2)
ih_dat.age3  = cbind(ih.ids.age3,ih.score.age3)
colnames(ih_dat.age1)[1] = "ID"
colnames(ih_dat.age2)[1] = "ID"
colnames(ih_dat.age3)[1] = "ID"
ih_dat.age1$cohort = 3
ih_dat.age2$cohort = 2
ih_dat.age3$cohort = 1
ih_dat.age = rbind(ih_dat.age1, ih_dat.age2, ih_dat.age3)

#Identity Concealment
syntax.CON.config.age <- measEq.syntax(configural.model = MI.CON, 
                                      data = data, 
                                      parameterization = "theta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT",
                                      group.equal = "thresholds",
                                      missing = "fiml")
cat(as.character(syntax.CON.config.age))
summary(syntax.CON.config.age)
mod.CON.config.age <- as.character(syntax.CON.config.age)
cat(mod.CON.config.age)
fit.CON.config.age <- cfa(mod.CON.config.age, data = data, parameterization = "theta", group = "COHORT")
lavInspect(fit.CON.config.age, "cov.lv")
summary(fit.CON.config.age)
#metric model
syntax.CON.metric.age <- measEq.syntax(configural.model = MI.CON, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT", 
                                      group.equal = c("thresholds","loadings"),
                                      missing = "fiml")

summary(syntax.CON.metric.age)
mod.CON.metric.age <- as.character(syntax.CON.metric.age)
cat(mod.CON.metric.age)
fit.CON.metric.age <- cfa(mod.CON.metric.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.CON.metric.age)

# Scalar model
syntax.CON.scalar.age <- measEq.syntax(configural.model = MI.CON, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "COHORT", 
                                      group.equal = c("thresholds","loadings",
                                                      "intercepts"),
                                      missing = "fiml")

summary(syntax.CON.scalar.age)
mod.CON.scalar.age <- as.character(syntax.CON.scalar.age)
cat(mod.CON.scalar.age)
fit.CON.scalar.age <- cfa(mod.CON.scalar.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.CON.scalar.age)

# Model fit indices 
fit.CON.age <- compareFit(fit.CON.config.age, fit.CON.metric.age, fit.CON.scalar.age) 
summary(fit.CON.age) 

#metric invariance not met as change in rmsea is |.039| and cfi is |.017|
#allowing for partial invariance so checking which parameter needs to be freed
lavTestScore(fit.CON.metric.age)
parTable(fit.CON.metric.age)
#suggests that item 2 factor loading needs to be freed
syntax.CON.metpart.age <- measEq.syntax(configural.model = MI.CON, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "COHORT", 
                                       group.equal = c("thresholds","loadings"),
                                       missing = "fiml",
                                       group.partial = "CON =~ conceal_2_r")
                                                        
summary(syntax.CON.metpart.age)
mod.CON.metpart.age <- as.character(syntax.CON.metpart.age)
cat(mod.CON.metpart.age)
fit.CON.metpart.age <- cfa(mod.CON.metpart.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.CON.metpart.age)

# checking to see if freeing item 2 resulted in partial invariance
fit.CON.part.age <- compareFit(fit.CON.config.age, fit.CON.metpart.age) 
summary(fit.CON.part.age) 
#rmsea still not invariant (change is |.016|) but will allow it
# will live with fit and continue to partial scalar invariance

syntax.CON.scapart.age <- measEq.syntax(configural.model = MI.CON, data = data,
                                        parameterization = "theta",
                                        ID.fac = "std.lv", 
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "COHORT", 
                                        group.equal = c("thresholds","loadings", 
                                                        "intercepts"),
                                        missing = "fiml",
                                        group.partial = c("CON =~ conceal_2_r",
                                                          "conceal_2_r ~ 1"))

summary(syntax.CON.scapart.age)
mod.CON.scapart.age <- as.character(syntax.CON.scapart.age)
cat(mod.CON.scapart.age)
fit.CON.scapart.age <- cfa(mod.CON.scapart.age, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.CON.scapart.age)

fit.CON.part.age <- compareFit(fit.CON.config.age, fit.CON.metpart.age, fit.CON.scapart.age) 
summary(fit.CON.part.age) 
#scalar is still not invariant so need to interpret with caution

con.score.age1 = as.data.frame(lavPredict(fit.CON.scapart.age)[[1]])
con.score.age2 = as.data.frame(lavPredict(fit.CON.scapart.age)[[2]])
con.score.age3 = as.data.frame(lavPredict(fit.CON.scapart.age)[[3]])
con.ids.age1  = as.data.frame(inspect(fit.CON.scapart.age, "case.idx")[[1]])
con.ids.age2  = as.data.frame(inspect(fit.CON.scapart.age, "case.idx")[[2]])
con.ids.age3  = as.data.frame(inspect(fit.CON.scapart.age, "case.idx")[[3]])

con_dat.age1  = cbind(con.ids.age1,con.score.age1)
con_dat.age2  = cbind(con.ids.age2,con.score.age2)
con_dat.age3  = cbind(con.ids.age3,con.score.age3)
colnames(con_dat.age1)[1] = "ID"
colnames(con_dat.age2)[1] = "ID"
colnames(con_dat.age3)[1] = "ID"
con_dat.age1$cohort = 3
con_dat.age2$cohort = 2
con_dat.age3$cohort = 1
con_dat.age = rbind(con_dat.age1, con_dat.age2, con_dat.age3)

#### Sexual Identity ####
# Life Satisfaction
syntax.LS.config.sexor <- measEq.syntax(configural.model = MI.LS, 
                                      data = data, 
                                      parameterization = "theta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "sexor",
                                      group.equal = "thresholds",
                                      missing = "fiml")
cat(as.character(syntax.LS.config.sexor))
summary(syntax.LS.config.sexor)
mod.LS.config.sexor <- as.character(syntax.LS.config.sexor)
cat(mod.LS.config.sexor)
fit.LS.config.sexor <- cfa(mod.LS.config.sexor, data = data, parameterization = "theta", group = "sexor")
lavInspect(fit.LS.config.sexor, "cov.lv")
summary(fit.LS.config.sexor)
#Metric Model
syntax.LS.metric.sexor <- measEq.syntax(configural.model = MI.LS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "sexor", 
                                      group.equal = c("thresholds","loadings"),
                                      missing = "fiml")

summary(syntax.LS.metric.sexor)
mod.LS.metric.sexor <- as.character(syntax.LS.metric.sexor)
cat(mod.LS.metric.sexor)
fit.LS.metric.sexor <- cfa(mod.LS.metric.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.LS.metric.sexor)

# Scalar model
syntax.LS.scalar.sexor <- measEq.syntax(configural.model = MI.LS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "sexor", 
                                      group.equal = c("thresholds","loadings",
                                                      "intercepts"),
                                      missing = "fiml")
mod.LS.scalar.sexor <- as.character(syntax.LS.scalar.sexor)
cat(mod.LS.scalar.sexor)
fit.LS.scalar.sexor <- cfa(mod.LS.scalar.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.LS.scalar.sexor)

# Model fit indices 
fit.LS.sexor <- compareFit(fit.LS.config.sexor, fit.LS.metric.sexor, fit.LS.scalar.sexor) 
summary(fit.LS.sexor) 

#metric invariance not met as change in rmsea is |.022|
#allowing for partial invariance so checking which parameter needs to be freed
lavTestScore(fit.LS.metric.sexor)
parTable(fit.LS.metric.sexor)
#factor loading of item 3 has highest chi-square so freeing that parameter
syntax.LS.metpart.sexor <- measEq.syntax(configural.model = MI.LS, data = data,
                                        parameterization = "theta",
                                        ID.fac = "std.lv", 
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "sexor", 
                                        group.equal = c("thresholds","loadings"),
                                        missing = "fiml", group.partial = "LS =~ swl_3")

summary(syntax.LS.metpart.sexor)
mod.LS.metpart.sexor <- as.character(syntax.LS.metpart.sexor)
cat(mod.LS.metpart.sexor)
fit.LS.metpart.sexor <- cfa(mod.LS.metpart.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.LS.metpart.sexor)

#Scalar model
syntax.LS.scalar.sexor <- measEq.syntax(configural.model = MI.LS, data = data,
                                        parameterization = "theta",
                                        ID.fac = "std.lv", 
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "sexor", 
                                        group.equal = c("thresholds","loadings",
                                                        "intercepts"),
                                        missing = "fiml",
                                        group.partial = "LS =~ swl_3")
mod.LS.scalar.sexor <- as.character(syntax.LS.scalar.sexor)
cat(mod.LS.scalar.sexor)
fit.LS.scalar.sexor <- cfa(mod.LS.scalar.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.LS.scalar.sexor)

fit.LS.sexor <- compareFit(fit.LS.config.sexor, fit.LS.metpart.sexor, fit.LS.scalar.sexor) 
summary(fit.LS.sexor) 
# fit is still not desirable but we will live with the fit an note as a limitation
# upon further looks, it seems for gay/lesbian folks, there a two-factor solution versus a one factor solution is preferred
# hence the misfit. so we will move forward with the analyses but note this as a limitation

ls.score.sexor1 = as.data.frame(lavPredict(fit.LS.scalar.sexor)[[1]])
ls.score.sexor2 = as.data.frame(lavPredict(fit.LS.scalar.sexor)[[2]])
ls.score.sexor3 = as.data.frame(lavPredict(fit.LS.scalar.sexor)[[3]])
ls.ids.sexor1  = as.data.frame(inspect(fit.LS.scalar.sexor, "case.idx")[[1]])
ls.ids.sexor2  = as.data.frame(inspect(fit.LS.scalar.sexor, "case.idx")[[2]])
ls.ids.sexor3  = as.data.frame(inspect(fit.LS.scalar.sexor, "case.idx")[[3]])

ls_dat.sexor1  = cbind(ls.ids.sexor1,ls.score.sexor1)
ls_dat.sexor2  = cbind(ls.ids.sexor2,ls.score.sexor2)
ls_dat.sexor3  = cbind(ls.ids.sexor3,ls.score.sexor3)
colnames(ls_dat.sexor1)[1] = "ID"
colnames(ls_dat.sexor2)[1] = "ID"
colnames(ls_dat.sexor3)[1] = "ID"
ls_dat.sexor1$sexor = 1
ls_dat.sexor2$sexor = 2
ls_dat.sexor3$sexor = 3
ls_dat.sexor = rbind(ls_dat.sexor1, ls_dat.sexor2, ls_dat.sexor3)

#Discrimination
syntax.DIS.config.sexor <- measEq.syntax(configural.model = MI.DIS, 
                                         data = data, 
                                         parameterization = "theta",
                                         ID.fac = "std.lv",
                                         ID.cat = "Wu.Estabrook.2016",
                                         group = "sexor",
                                         group.equal = "thresholds",
                                         missing = "fiml")
cat(as.character(syntax.DIS.config.sexor))
summary(syntax.DIS.config.sexor)
mod.DIS.config.sexor <- as.character(syntax.DIS.config.sexor)
cat(mod.DIS.config.sexor)
fit.DIS.config.sexor <- cfa(mod.DIS.config.sexor, data = data, parameterization = "theta", group = "sexor")
lavInspect(fit.DIS.config.sexor, "cov.lv")
summary(fit.DIS.config.sexor)
#Metric model
syntax.DIS.metric.sexor <- measEq.syntax(configural.model = MI.DIS, data = data,
                                         parameterization = "theta",
                                         ID.fac = "std.lv", 
                                         ID.cat = "Wu.Estabrook.2016",
                                         group = "sexor", 
                                         group.equal = c("thresholds","loadings"),
                                         missing = "fiml")

summary(syntax.DIS.metric.sexor)
mod.DIS.metric.sexor <- as.character(syntax.DIS.metric.sexor)
cat(mod.DIS.metric.sexor)
fit.DIS.metric.sexor <- cfa(mod.DIS.metric.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.DIS.metric.sexor)

# Scalar model
syntax.DIS.scalar.sexor <- measEq.syntax(configural.model = MI.DIS, data = data,
                                         parameterization = "theta",
                                         ID.fac = "std.lv", 
                                         ID.cat = "Wu.Estabrook.2016",
                                         group = "sexor", 
                                         group.equal = c("thresholds","loadings",
                                                         "intercepts"),
                                         missing = "fiml")

summary(syntax.DIS.scalar.sexor)
mod.DIS.scalar.sexor <- as.character(syntax.DIS.scalar.sexor)
cat(mod.DIS.scalar.sexor)
fit.DIS.scalar.sexor <- cfa(mod.DIS.scalar.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.DIS.scalar.sexor)

# Model fit indices 
fit.DIS.sexor <- compareFit(fit.DIS.config.sexor, fit.DIS.metric.sexor, fit.DIS.scalar.sexor) 
summary(fit.DIS.sexor) 
#measurement invariance met

dis.score.sexor1 = as.data.frame(lavPredict(fit.DIS.scalar.sexor)[[1]])
dis.score.sexor2 = as.data.frame(lavPredict(fit.DIS.scalar.sexor)[[2]])
dis.score.sexor3 = as.data.frame(lavPredict(fit.DIS.scalar.sexor)[[3]])
dis.ids.sexor1  = as.data.frame(inspect(fit.DIS.scalar.sexor, "case.idx")[[1]])
dis.ids.sexor2  = as.data.frame(inspect(fit.DIS.scalar.sexor, "case.idx")[[2]])
dis.ids.sexor3  = as.data.frame(inspect(fit.DIS.scalar.sexor, "case.idx")[[3]])

dis_dat.sexor1  = cbind(dis.ids.sexor1,dis.score.sexor1)
dis_dat.sexor2  = cbind(dis.ids.sexor2,dis.score.sexor2)
dis_dat.sexor3  = cbind(dis.ids.sexor3,dis.score.sexor3)
colnames(dis_dat.sexor1)[1] = "ID"
colnames(dis_dat.sexor2)[1] = "ID"
colnames(dis_dat.sexor3)[1] = "ID"
dis_dat.sexor1$sexor = 1
dis_dat.sexor2$sexor = 2
dis_dat.sexor3$sexor = 3
dis_dat.sexor = rbind(dis_dat.sexor1, dis_dat.sexor2, dis_dat.sexor3)

# Stigmatization
syntax.FS.config.sexor <- measEq.syntax(configural.model = MI.FS, 
                                      data = data, 
                                      parameterization = "theta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "sexor",
                                      group.equal = "thresholds",
                                      missing = "fiml")
cat(as.character(syntax.FS.config.sexor))
summary(syntax.FS.config.sexor)
mod.FS.config.sexor <- as.character(syntax.FS.config.sexor)
cat(mod.FS.config.sexor)
fit.FS.config.sexor <- cfa(mod.FS.config.sexor, data = data, parameterization = "theta", group = "sexor")
lavInspect(fit.FS.config.sexor, "cov.lv")
summary(fit.FS.config.sexor)
#Metric model
syntax.FS.metric.sexor <- measEq.syntax(configural.model = MI.FS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "sexor", 
                                      group.equal = c("thresholds","loadings"),
                                      missing = "fiml")

summary(syntax.FS.metric.sexor)
mod.FS.metric.sexor <- as.character(syntax.FS.metric.sexor)
cat(mod.FS.metric.sexor)
fit.FS.metric.sexor <- cfa(mod.FS.metric.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.FS.metric.sexor)

# Scalar model
syntax.FS.scalar.sexor <- measEq.syntax(configural.model = MI.FS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "sexor", 
                                      group.equal = c("thresholds","loadings",
                                                      "intercepts"),
                                      missing = "fiml")

summary(syntax.FS.scalar.sexor)
mod.FS.scalar.sexor <- as.character(syntax.FS.scalar.sexor)
cat(mod.FS.scalar.sexor)
fit.FS.scalar.sexor <- cfa(mod.FS.scalar.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.FS.scalar.sexor)

# Model fit indices 
fit.FS.sexor <- compareFit(fit.FS.config.sexor, fit.FS.metric.sexor, fit.FS.scalar.sexor) 
summary(fit.FS.sexor) 
#metric invariance not met as rmsea is |.031|
#allowing for partial invariance so checking which parameter needs to be freed
lavTestScore(fit.FS.metric.sexor)
parTable(fit.FS.metric.sexor)
# As with life satisfaction, there's not any one item really causing misfit. It probbaly is
# just 
# item 3 factor loading has highest chi-square, so freeing that parameter
syntax.FS.metpart.sexor <- measEq.syntax(configural.model = MI.FS, data = data,
                                        parameterization = "theta",
                                        ID.fac = "std.lv", 
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "sexor", 
                                        group.equal = c("thresholds","loadings"),
                                        missing = "fiml",
                                        group.partial = "FS =~   fs_3")

summary(syntax.FS.metpart.sexor)
mod.FS.metpart.sexor <- as.character(syntax.FS.metpart.sexor)
cat(mod.FS.metpart.sexor)
fit.FS.metpart.sexor <- cfa(mod.FS.metpart.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.FS.metpart.sexor)

# checking to see if partial invariance met
fit.FS.part.sexor <- compareFit(fit.FS.config.sexor, fit.FS.metpart.sexor) 
summary(fit.FS.part.sexor) 
#partial metric invariance met
syntax.FS.scapart.sexor <- measEq.syntax(configural.model = MI.FS, data = data,
                                         parameterization = "theta",
                                         ID.fac = "std.lv", 
                                         ID.cat = "Wu.Estabrook.2016",
                                         group = "sexor", 
                                         group.equal = c("thresholds","loadings",
                                                         "intercepts"),
                                         missing = "fiml", group.partial = c("FS =~   fs_3",
                                                                             "fs_3 ~ 1"))

summary(syntax.FS.scapart.sexor)
mod.FS.scapart.sexor <- as.character(syntax.FS.scapart.sexor)
cat(mod.FS.scapart.sexor)
fit.FS.scapart.sexor <- cfa(mod.FS.scapart.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.FS.scapart.sexor)

fit.FS.part.sexor <- compareFit(fit.FS.config.sexor, fit.FS.metpart.sexor, fit.FS.scapart.sexor) 
summary(fit.FS.part.sexor) 
#RMSEA jumps to .015 but this is a model with three items so RMSEA is more sensitive to that

fs.score.sexor1 = as.data.frame(lavPredict(fit.FS.scapart.sexor)[[1]])
fs.score.sexor2 = as.data.frame(lavPredict(fit.FS.scapart.sexor)[[2]])
fs.score.sexor3 = as.data.frame(lavPredict(fit.FS.scapart.sexor)[[3]])
fs.ids.sexor1  = as.data.frame(inspect(fit.FS.scapart.sexor, "case.idx")[[1]])
fs.ids.sexor2  = as.data.frame(inspect(fit.FS.scapart.sexor, "case.idx")[[2]])
fs.ids.sexor3  = as.data.frame(inspect(fit.FS.scapart.sexor, "case.idx")[[3]])

fs_dat.sexor1  = cbind(fs.ids.sexor1,fs.score.sexor1)
fs_dat.sexor2  = cbind(fs.ids.sexor2,fs.score.sexor2)
fs_dat.sexor3  = cbind(fs.ids.sexor3,fs.score.sexor3)
colnames(fs_dat.sexor1)[1] = "ID"
colnames(fs_dat.sexor2)[1] = "ID"
colnames(fs_dat.sexor3)[1] = "ID"
fs_dat.sexor1$sexor = 1
fs_dat.sexor2$sexor = 2
fs_dat.sexor3$sexor = 3
fs_dat.sexor = rbind(fs_dat.sexor1, fs_dat.sexor2, fs_dat.sexor3)

# Victimization
syntax.VIC.config.sexor <- measEq.syntax(configural.model = MI.VIC, 
                                       data = data, 
                                       parameterization = "theta",
                                       ID.fac = "std.lv",
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "sexor",
                                       group.equal = "thresholds",
                                       missing = "fiml")
cat(as.character(syntax.VIC.config.sexor))
summary(syntax.VIC.config.sexor)
mod.VIC.config.sexor <- as.character(syntax.VIC.config.sexor)
cat(mod.VIC.config.sexor)
fit.VIC.config.sexor <- cfa(mod.VIC.config.sexor, data = data, parameterization = "theta", group = "sexor")
lavInspect(fit.VIC.config.sexor, "cov.lv")
summary(fit.VIC.config.sexor)
#Metric Model
syntax.VIC.metric.sexor <- measEq.syntax(configural.model = MI.VIC, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "sexor", 
                                       group.equal = c("thresholds","loadings"),
                                       missing = "fiml")

summary(syntax.VIC.metric.sexor)
mod.VIC.metric.sexor <- as.character(syntax.VIC.metric.sexor)
cat(mod.VIC.metric.sexor)
fit.VIC.metric.sexor <- cfa(mod.VIC.metric.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.VIC.metric.sexor)

# Scalar model
syntax.VIC.scalar.sexor <- measEq.syntax(configural.model = MI.VIC, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "sexor", 
                                       group.equal = c("thresholds","loadings",
                                                       "intercepts"),
                                       missing = "fiml")

summary(syntax.VIC.scalar.sexor)
mod.VIC.scalar.sexor <- as.character(syntax.VIC.scalar.sexor)
cat(mod.VIC.scalar.sexor)
fit.VIC.scalar.sexor <- cfa(mod.VIC.scalar.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.VIC.scalar.sexor)

# Model fit indices 
fit.VIC.sexor <- compareFit(fit.VIC.config.sexor, fit.VIC.metric.sexor, fit.VIC.scalar.sexor) 
summary(fit.VIC.sexor) 
#measurement invariance met
vic.score.sexor1 = as.data.frame(lavPredict(fit.VIC.scalar.sexor)[[1]])
vic.score.sexor2 = as.data.frame(lavPredict(fit.VIC.scalar.sexor)[[2]])
vic.score.sexor3 = as.data.frame(lavPredict(fit.VIC.scalar.sexor)[[3]])
vic.ids.sexor1  = as.data.frame(inspect(fit.VIC.scalar.sexor, "case.idx")[[1]])
vic.ids.sexor2  = as.data.frame(inspect(fit.VIC.scalar.sexor, "case.idx")[[2]])
vic.ids.sexor3  = as.data.frame(inspect(fit.VIC.scalar.sexor, "case.idx")[[3]])

vic_dat.sexor1  = cbind(vic.ids.sexor1,vic.score.sexor1)
vic_dat.sexor2  = cbind(vic.ids.sexor2,vic.score.sexor2)
vic_dat.sexor3  = cbind(vic.ids.sexor3,vic.score.sexor3)
colnames(vic_dat.sexor1)[1] = "ID"
colnames(vic_dat.sexor2)[1] = "ID"
colnames(vic_dat.sexor3)[1] = "ID"
vic_dat.sexor1$sexor = 1
vic_dat.sexor2$sexor = 2
vic_dat.sexor3$sexor = 3
vic_dat.sexor = rbind(vic_dat.sexor1, vic_dat.sexor2, vic_dat.sexor3)

#Internalized Homophobia
syntax.IH.config.sexor <- measEq.syntax(configural.model = MI.IH, 
                                      data = data, 
                                      parameterization = "theta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "sexor",
                                      group.equal = "thresholds",
                                      missing = "fiml")
cat(as.character(syntax.IH.config.sexor))
summary(syntax.IH.config.sexor)
mod.IH.config.sexor <- as.character(syntax.IH.config.sexor)
cat(mod.IH.config.sexor)
fit.IH.config.sexor <- cfa(mod.IH.config.sexor, data = data, parameterization = "theta", group = "sexor")
summary(fit.IH.config.sexor)
#Metric Model
syntax.IH.metric.sexor <- measEq.syntax(configural.model = MI.IH, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "sexor", 
                                      group.equal = c("thresholds","loadings"),
                                      missing = "fiml")

summary(syntax.IH.metric.sexor)
mod.IH.metric.sexor <- as.character(syntax.IH.metric.sexor)
cat(mod.IH.metric.sexor)
fit.IH.metric.sexor <- cfa(mod.IH.metric.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.IH.metric.sexor)

# Scalar model
syntax.IH.scalar.sexor <- measEq.syntax(configural.model = MI.IH, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "sexor", 
                                      group.equal = c("thresholds","loadings",
                                                      "intercepts"),
                                      missing = "fiml")

summary(syntax.IH.scalar.sexor)
mod.IH.scalar.sexor <- as.character(syntax.IH.scalar.sexor)
cat(mod.IH.scalar.sexor)
fit.IH.scalar.sexor <- cfa(mod.IH.scalar.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.IH.scalar.sexor)

# Model fit indices 
fit.IH.sexor <- compareFit(fit.IH.config.sexor, fit.IH.metric.sexor, fit.IH.scalar.sexor) 
summary(fit.IH.sexor) 
#metric invariance not met
lavTestScore(fit.IH.metric.sexor)
parTable(fit.IH.metric.sexor)
# Freeing the factor loading of ih_1
syntax.IH.metpart.sexor <- measEq.syntax(configural.model = MI.IH, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "sexor", 
                                       group.equal = c("thresholds","loadings"),
                                       missing = "fiml", group.partial = c("ih_1 =~ 1"))
summary(syntax.IH.metpart.sexor)
mod.IH.metpart.sexor <- as.character(syntax.IH.metpart.sexor)
cat(mod.IH.metpart.sexor)
fit.IH.metpart.sexor <- cfa(mod.IH.metpart.sexor, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.IH.metpart.sexor)
#comparing models
fit.IH.part.sexor <- compareFit(fit.IH.config.sexor, fit.IH.metpart.sexor) 
summary(fit.IH.part.sexor) 
#still not quite invariant
lavTestScore(fit.IH.metpart.sexor)
parTable(fit.IH.metpart.sexor)
#does not seem further items will improve fit so we will move forward but interpret with caution
syntax.IH.scapart.sexor <- measEq.syntax(configural.model = MI.IH, data = data,
                                         parameterization = "theta",
                                         ID.fac = "std.lv", 
                                         ID.cat = "Wu.Estabrook.2016",
                                         group = "sexor", 
                                         group.equal = c("thresholds","loadings",
                                                         "intercepts"),
                                         missing = "fiml", group.partial = c("ih_1 =~ 1",
                                                                             "ih_1 ~ 1"))
summary(syntax.IH.scapart.sexor)
mod.IH.scapart.sexor <- as.character(syntax.IH.scapart.sexor)
cat(mod.IH.scapart.sexor)
fit.IH.scapart.sexor <- cfa(mod.IH.scapart.sexor, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.IH.scapart.sexor)
#comparing models
fit.IH.part.sexor <- compareFit(fit.IH.config.sexor, fit.IH.metpart.sexor,fit.IH.scapart.sexor) 
summary(fit.IH.part.sexor) 
#Still not great but already invariant so will move forward

ih.score.sexor1 = as.data.frame(lavPredict(fit.IH.scapart.sexor)[[1]])
ih.score.sexor2 = as.data.frame(lavPredict(fit.IH.scapart.sexor)[[2]])
ih.score.sexor3 = as.data.frame(lavPredict(fit.IH.scapart.sexor)[[3]])
ih.ids.sexor1  = as.data.frame(inspect(fit.IH.scapart.sexor, "case.idx")[[1]])
ih.ids.sexor2  = as.data.frame(inspect(fit.IH.scapart.sexor, "case.idx")[[2]])
ih.ids.sexor3  = as.data.frame(inspect(fit.IH.scapart.sexor, "case.idx")[[3]])

ih_dat.sexor1  = cbind(ih.ids.sexor1,ih.score.sexor1)
ih_dat.sexor2  = cbind(ih.ids.sexor2,ih.score.sexor2)
ih_dat.sexor3  = cbind(ih.ids.sexor3,ih.score.sexor3)
colnames(ih_dat.sexor1)[1] = "ID"
colnames(ih_dat.sexor2)[1] = "ID"
colnames(ih_dat.sexor3)[1] = "ID"
ih_dat.sexor1$sexor = 1
ih_dat.sexor2$sexor = 2
ih_dat.sexor3$sexor = 3
ih_dat.sexor = rbind(ih_dat.sexor1, ih_dat.sexor2, ih_dat.sexor3)

#Identity Concealment
syntax.CON.config.sexor <- measEq.syntax(configural.model = MI.CON, 
                                       data = data, 
                                       parameterization = "theta",
                                       ID.fac = "std.lv",
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "sexor",
                                       group.equal = "thresholds",
                                       missing = "fiml")
cat(as.character(syntax.CON.config.sexor))
summary(syntax.CON.config.sexor)
mod.CON.config.sexor <- as.character(syntax.CON.config.sexor)
cat(mod.CON.config.sexor)
fit.CON.config.sexor <- cfa(mod.CON.config.sexor, data = data, parameterization = "theta", group = "sexor")
lavInspect(fit.CON.config.sexor, "cov.lv")
summary(fit.CON.config.sexor)
#Metric model
syntax.CON.metric.sexor <- measEq.syntax(configural.model = MI.CON, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "sexor", 
                                       group.equal = c("thresholds","loadings"),
                                       missing = "fiml")

summary(syntax.CON.metric.sexor)
mod.CON.metric.sexor <- as.character(syntax.CON.metric.sexor)
cat(mod.CON.metric.sexor)
fit.CON.metric.sexor <- cfa(mod.CON.metric.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.CON.metric.sexor)

# Scalar model
syntax.CON.scalar.sexor <- measEq.syntax(configural.model = MI.CON, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "sexor", 
                                       group.equal = c("thresholds","loadings",
                                                       "intercepts"),
                                       missing = "fiml")

summary(syntax.CON.scalar.sexor)
mod.CON.scalar.sexor <- as.character(syntax.CON.scalar.sexor)
cat(mod.CON.scalar.sexor)
fit.CON.scalar.sexor <- cfa(mod.CON.scalar.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.CON.scalar.sexor)

# Model fit indices 
fit.CON.sexor <- compareFit(fit.CON.config.sexor, fit.CON.metric.sexor, fit.CON.scalar.sexor) 
summary(fit.CON.sexor) 
#metric invariance not met as change in rmsea is |.018| and cfi is |.017|
#allowing for partial invariance so checking which parameter needs to be freed
lavTestScore(fit.CON.metric.sexor)
parTable(fit.CON.metric.sexor)

#Partial Metric model
syntax.CON.metpart.sexor <- measEq.syntax(configural.model = MI.CON, data = data,
                                         parameterization = "theta",
                                         ID.fac = "std.lv", 
                                         ID.cat = "Wu.Estabrook.2016",
                                         group = "sexor", 
                                         group.equal = c("thresholds","loadings"),
                                         missing = "fiml",
                                         group.partial = "CON =~ conceal_3_r")

summary(syntax.CON.metpart.sexor)
mod.CON.metpart.sexor <- as.character(syntax.CON.metpart.sexor)
cat(mod.CON.metpart.sexor)
fit.CON.metpart.sexor <- cfa(mod.CON.metpart.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.CON.metpart.sexor)
# Model fit indices 
fit.CON.sexor <- compareFit(fit.CON.config.sexor, fit.CON.metpart.sexor) 
summary(fit.CON.sexor) 
#partial metric invariance met

#Scalar model
syntax.CON.scapart.sexor <- measEq.syntax(configural.model = MI.CON, data = data,
                                          parameterization = "theta",
                                          ID.fac = "std.lv", 
                                          ID.cat = "Wu.Estabrook.2016",
                                          group = "sexor", 
                                          group.equal = c("thresholds","loadings",
                                                          "intercepts"),
                                          missing = "fiml",
                                          group.partial = c("CON =~ conceal_3_r",
                                                            "conceal_3_r ~1"))

summary(syntax.CON.scapart.sexor)
mod.CON.scapart.sexor <- as.character(syntax.CON.scapart.sexor)
cat(mod.CON.scapart.sexor)
fit.CON.scapart.sexor <- cfa(mod.CON.scapart.sexor, data = data, parameterization = "theta", group = "sexor") 
summary(fit.CON.scapart.sexor)

fit.CON.sexor <- compareFit(fit.CON.config.sexor, fit.CON.metpart.sexor, fit.CON.scapart.sexor) 
summary(fit.CON.sexor) 
lavTestScore(fit.CON.scapart.sexor)
parTable(fit.CON.scapart.sexor)
#for to scalar not great but its partial invariance so move forward with it

con.score.sexor1 = as.data.frame(lavPredict(fit.CON.scapart.sexor)[[1]])
con.score.sexor2 = as.data.frame(lavPredict(fit.CON.scapart.sexor)[[2]])
con.score.sexor3 = as.data.frame(lavPredict(fit.CON.scapart.sexor)[[3]])
con.ids.sexor1  = as.data.frame(inspect(fit.CON.scapart.sexor, "case.idx")[[1]])
con.ids.sexor2  = as.data.frame(inspect(fit.CON.scapart.sexor, "case.idx")[[2]])
con.ids.sexor3  = as.data.frame(inspect(fit.CON.scapart.sexor, "case.idx")[[3]])

con_dat.sexor1  = cbind(con.ids.sexor1,con.score.sexor1)
con_dat.sexor2  = cbind(con.ids.sexor2,con.score.sexor2)
con_dat.sexor3  = cbind(con.ids.sexor3,con.score.sexor3)
colnames(con_dat.sexor1)[1] = "ID"
colnames(con_dat.sexor2)[1] = "ID"
colnames(con_dat.sexor3)[1] = "ID"
con_dat.sexor1$sexor = 1
con_dat.sexor2$sexor = 2
con_dat.sexor3$sexor = 3
con_dat.sexor = rbind(con_dat.sexor1, con_dat.sexor2, con_dat.sexor3)

####Race / Ethnicity####
# Life Satisfaction
syntax.LS.config.race <- measEq.syntax(configural.model = MI.LS, 
                                      data = data, 
                                      parameterization = "theta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "race",
                                      group.equal = "thresholds",
                                      missing = "fiml")
cat(as.character(syntax.LS.config.race))
summary(syntax.LS.config.race)
mod.LS.config.race <- as.character(syntax.LS.config.race)
cat(mod.LS.config.race)
fit.LS.config.race <- cfa(mod.LS.config.race, data = data, parameterization = "theta", group = "race")
lavInspect(fit.LS.config.race, "cov.lv")
summary(fit.LS.config.race)
#metric model
syntax.LS.metric.race <- measEq.syntax(configural.model = MI.LS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "race", 
                                      group.equal = c("thresholds","loadings"),
                                      missing = "fiml")

summary(syntax.LS.metric.race)
mod.LS.metric.race <- as.character(syntax.LS.metric.race)
cat(mod.LS.metric.race)
fit.LS.metric.race <- cfa(mod.LS.metric.race, data = data, parameterization = "theta", group = "race") 
summary(fit.LS.metric.race)

# Scalar model
syntax.LS.scalar.race <- measEq.syntax(configural.model = MI.LS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "race", 
                                      group.equal = c("thresholds","loadings",
                                                      "intercepts"),
                                      missing = "fiml")

summary(syntax.LS.scalar.race)
mod.LS.scalar.race <- as.character(syntax.LS.scalar.race)
cat(mod.LS.scalar.race)
fit.LS.scalar.race <- cfa(mod.LS.scalar.race, data = data, parameterization = "theta", group = "race") 
summary(fit.LS.scalar.race)

# Model fit indices 
fit.LS.race <- compareFit(fit.LS.config.race, fit.LS.metric.race, fit.LS.scalar.race) 
summary(fit.LS.race) 
#metric invariance not met as change in rmsea is |.017|
#allowing for partial invariance so checking which parameter needs to be freed
lavTestScore(fit.LS.metric.race)
parTable(fit.LS.metric.race)
# item 3 factor loading had highest chi-square value so freeing that parameter
#partial metric model
syntax.LS.metpart.race <- measEq.syntax(configural.model = MI.LS, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "race", 
                                       group.equal = c("thresholds","loadings"),
                                       missing = "fiml")

summary(syntax.LS.metpart.race)
mod.LS.metpart.race <- as.character(syntax.LS.metpart.race)
cat(mod.LS.metpart.race)
fit.LS.metpart.race <- cfa(mod.LS.metpart.race, data = data, parameterization = "theta", group = "race") 
summary(fit.LS.metpart.race)
#Scalar model
syntax.LS.scapart.race <- measEq.syntax(configural.model = MI.LS, data = data,
                                        parameterization = "theta",
                                        ID.fac = "std.lv", 
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "race", 
                                        group.equal = c("thresholds","loadings",
                                                        "intercepts"),
                                        missing = "fiml")

summary(syntax.LS.scapart.race)
mod.LS.scapart.race <- as.character(syntax.LS.scapart.race)
cat(mod.LS.scapart.race)
fit.LS.scapart.race <- cfa(mod.LS.scapart.race, data = data, parameterization = "theta", group = "race") 
summary(fit.LS.scapart.race)
# checking i partial invariance met
fit.LS.part.race <- compareFit(fit.LS.config.race, fit.LS.metpart.race, fit.LS.scapart.race) 
summary(fit.LS.part.race)
#metric invariance is still not met as change in rmsea is |.015|
#does not do much to RMSEA so just going to keep the model as note as a limitation

ls.score.race1 = as.data.frame(lavPredict(fit.LS.scalar.race)[[1]])
ls.score.race2 = as.data.frame(lavPredict(fit.LS.scalar.race)[[2]])
ls.score.race3 = as.data.frame(lavPredict(fit.LS.scalar.race)[[3]])
ls.ids.race1  = as.data.frame(inspect(fit.LS.scalar.race, "case.idx")[[1]])
ls.ids.race2  = as.data.frame(inspect(fit.LS.scalar.race, "case.idx")[[2]])
ls.ids.race3  = as.data.frame(inspect(fit.LS.scalar.race, "case.idx")[[3]])

ls_dat.race1  = cbind(ls.ids.race1,ls.score.race1)
ls_dat.race2  = cbind(ls.ids.race2,ls.score.race2)
ls_dat.race3  = cbind(ls.ids.race3,ls.score.race3)
colnames(ls_dat.race1)[1] = "ID"
colnames(ls_dat.race2)[1] = "ID"
colnames(ls_dat.race3)[1] = "ID"
ls_dat.race1$race = 1
ls_dat.race2$race = 3
ls_dat.race3$race = 2
ls_dat.race = rbind(ls_dat.race1, ls_dat.race2, ls_dat.race3)

#Discrimination
syntax.DIS.config.race <- measEq.syntax(configural.model = MI.DIS, 
                                        data = data, 
                                        parameterization = "theta",
                                        ID.fac = "std.lv",
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "race",
                                        group.equal = "thresholds",
                                        missing = "fiml")
cat(as.character(syntax.DIS.config.race))
summary(syntax.DIS.config.race)
mod.DIS.config.race <- as.character(syntax.DIS.config.race)
cat(mod.DIS.config.race)
fit.DIS.config.race <- cfa(mod.DIS.config.race, data = data, parameterization = "theta", group = "race")
lavInspect(fit.DIS.config.race, "cov.lv")
summary(fit.DIS.config.race)

#metric model
syntax.DIS.metric.race <- measEq.syntax(configural.model = MI.DIS, data = data,
                                        parameterization = "theta",
                                        ID.fac = "std.lv", 
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "race", 
                                        group.equal = c("thresholds","loadings"),
                                        missing = "fiml")

summary(syntax.DIS.metric.race)
mod.DIS.metric.race <- as.character(syntax.DIS.metric.race)
cat(mod.DIS.metric.race)
fit.DIS.metric.race <- cfa(mod.DIS.metric.race, data = data, parameterization = "theta", group = "race") 
summary(fit.DIS.metric.race)
lavTestScore(fit.DIS.metric.race)
parTable(fit.DIS.metric.race)
# Scalar model
syntax.DIS.scalar.race <- measEq.syntax(configural.model = MI.DIS, data = data,
                                        parameterization = "theta",
                                        ID.fac = "std.lv", 
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "race", 
                                        group.equal = c("thresholds","loadings",
                                                        "intercepts"),
                                        missing = "fiml")

summary(syntax.DIS.scalar.race)
mod.DIS.scalar.race <- as.character(syntax.DIS.scalar.race)
cat(mod.DIS.scalar.race)
fit.DIS.scalar.race <- cfa(mod.DIS.scalar.race, data = data, parameterization = "theta", group = "race") 
summary(fit.DIS.scalar.race)
# Model fit indices 
fit.DIS.race <- compareFit(fit.DIS.config.race, fit.DIS.metric.race, fit.DIS.scalar.race) 
summary(fit.DIS.race)
#Measurement invariance met

dis.score.race1 = as.data.frame(lavPredict(fit.DIS.metric.race)[[1]])
dis.score.race2 = as.data.frame(lavPredict(fit.DIS.metric.race)[[2]])
dis.score.race3 = as.data.frame(lavPredict(fit.DIS.metric.race)[[3]])
dis.ids.race1  = as.data.frame(inspect(fit.DIS.metric.race, "case.idx")[[1]])
dis.ids.race2  = as.data.frame(inspect(fit.DIS.metric.race, "case.idx")[[2]])
dis.ids.race3  = as.data.frame(inspect(fit.DIS.metric.race, "case.idx")[[3]])

dis_dat.race1  = cbind(dis.ids.race1,dis.score.race1)
dis_dat.race2  = cbind(dis.ids.race2,dis.score.race2)
dis_dat.race3  = cbind(dis.ids.race3,dis.score.race3)
colnames(dis_dat.race1)[1] = "ID"
colnames(dis_dat.race2)[1] = "ID"
colnames(dis_dat.race3)[1] = "ID"
dis_dat.race1$race = 1
dis_dat.race2$race = 3
dis_dat.race3$race = 2
dis_dat.race = rbind(dis_dat.race1, dis_dat.race2, dis_dat.race3)

# Stigmatization
syntax.FS.config.race <- measEq.syntax(configural.model = MI.FS, 
                                      data = data, 
                                      parameterization = "theta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "race",
                                      group.equal = "thresholds",
                                      missing = "fiml")
cat(as.character(syntax.FS.config.race))
summary(syntax.FS.config.race)
mod.FS.config.race <- as.character(syntax.FS.config.race)
cat(mod.FS.config.race)
fit.FS.config.race <- cfa(mod.FS.config.race, data = data, parameterization = "theta", group = "race")
lavInspect(fit.FS.config.race, "cov.lv")
summary(fit.FS.config.race)
#Metric Model
syntax.FS.metric.race <- measEq.syntax(configural.model = MI.FS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "race", 
                                      group.equal = c("thresholds","loadings"),
                                      missing = "fiml")

summary(syntax.FS.metric.race)
mod.FS.metric.race <- as.character(syntax.FS.metric.race)
cat(mod.FS.metric.race)
fit.FS.metric.race <- cfa(mod.FS.metric.race, data = data, parameterization = "theta", group = "race") 
summary(fit.FS.metric.race)

# Scalar model
syntax.FS.scalar.race <- measEq.syntax(configural.model = MI.FS, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "race", 
                                      group.equal = c("thresholds","loadings",
                                                      "intercepts"),
                                      missing = "fiml")

summary(syntax.FS.scalar.race)
mod.FS.scalar.race <- as.character(syntax.FS.scalar.race)
cat(mod.FS.scalar.race)
fit.FS.scalar.race <- cfa(mod.FS.scalar.race, data = data, parameterization = "theta", group = "race") 
summary(fit.FS.scalar.race)

# Model fit indices 
fit.FS.race <- compareFit(fit.FS.config.race, fit.FS.metric.race, fit.FS.scalar.race) 
summary(fit.FS.race) 
#measurement invariance met

fs.score.race1 = as.data.frame(lavPredict(fit.FS.scalar.race)[[1]])
fs.score.race2 = as.data.frame(lavPredict(fit.FS.scalar.race)[[2]])
fs.score.race3 = as.data.frame(lavPredict(fit.FS.scalar.race)[[3]])
fs.ids.race1  = as.data.frame(inspect(fit.FS.scalar.race, "case.idx")[[1]])
fs.ids.race2  = as.data.frame(inspect(fit.FS.scalar.race, "case.idx")[[2]])
fs.ids.race3  = as.data.frame(inspect(fit.FS.scalar.race, "case.idx")[[3]])

fs_dat.race1  = cbind(fs.ids.race1,fs.score.race1)
fs_dat.race2  = cbind(fs.ids.race2,fs.score.race2)
fs_dat.race3  = cbind(fs.ids.race3,fs.score.race3)
colnames(fs_dat.race1)[1] = "ID"
colnames(fs_dat.race2)[1] = "ID"
colnames(fs_dat.race3)[1] = "ID"
fs_dat.race1$race = 1
fs_dat.race2$race = 3
fs_dat.race3$race = 2
fs_dat.race = rbind(fs_dat.race1, fs_dat.race2, fs_dat.race3)

# Victimization
syntax.VIC.config.race <- measEq.syntax(configural.model = MI.VIC, 
                                       data = data, 
                                       parameterization = "theta",
                                       ID.fac = "std.lv",
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "race",
                                       group.equal = "thresholds",
                                       missing = "fiml")
cat(as.character(syntax.VIC.config.race))
summary(syntax.VIC.config.race)
mod.VIC.config.race <- as.character(syntax.VIC.config.race)
cat(mod.VIC.config.race)
fit.VIC.config.race <- cfa(mod.VIC.config.race, data = data, parameterization = "theta", group = "race")
lavInspect(fit.VIC.config.race, "cov.lv")
summary(fit.VIC.config.race)
#Metric Model
syntax.VIC.metric.race <- measEq.syntax(configural.model = MI.VIC, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "race", 
                                       group.equal = c("thresholds","loadings"),
                                       missing = "fiml")

summary(syntax.VIC.metric.race)
mod.VIC.metric.race <- as.character(syntax.VIC.metric.race)
cat(mod.VIC.metric.race)
fit.VIC.metric.race <- cfa(mod.VIC.metric.race, data = data, parameterization = "theta", group = "race") 
summary(fit.VIC.metric.race)

# Scalar model
syntax.VIC.scalar.race <- measEq.syntax(configural.model = MI.VIC, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "race", 
                                       group.equal = c("thresholds","loadings",
                                                       "intercepts"),
                                       missing = "fiml")

summary(syntax.VIC.scalar.race)
mod.VIC.scalar.race <- as.character(syntax.VIC.scalar.race)
cat(mod.VIC.scalar.race)
fit.VIC.scalar.race <- cfa(mod.VIC.scalar.race, data = data, parameterization = "theta", group = "race") 
summary(fit.VIC.scalar.race)

# Model fit indices 
fit.VIC.race <- compareFit(fit.VIC.config.race, fit.VIC.metric.race, fit.VIC.scalar.race) 
summary(fit.VIC.race) 
#scalar invariance not met as change in cfi is |.012|
#allowing for partial invariance so checking which parameter needs to be freed
lavTestScore(fit.VIC.scalar.race)
parTable(fit.VIC.scalar.race)
#item 2 intercept has highest chi-square so freeing that parameter
# Partial Scalar model
syntax.VIC.scapart.race <- measEq.syntax(configural.model = MI.VIC, data = data,
                                        parameterization = "theta",
                                        ID.fac = "std.lv", 
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "race", 
                                        group.equal = c("thresholds","loadings",
                                                        "intercepts"),
                                        missing = "fiml",
                                        group.partial = "vic_2 ~1")

summary(syntax.VIC.scapart.race)
mod.VIC.scapart.race <- as.character(syntax.VIC.scapart.race)
cat(mod.VIC.scapart.race)
fit.VIC.scapart.race <- cfa(mod.VIC.scapart.race, data = data, parameterization = "theta", group = "race") 
summary(fit.VIC.scapart.race)

# checking partial invariance
fit.VIC.part.race <- compareFit(fit.VIC.config.race, fit.VIC.metric.race, fit.VIC.scapart.race) 
summary(fit.VIC.part.race) 
#partial scalar invariance met
vic.score.race1 = as.data.frame(lavPredict(fit.VIC.scapart.race)[[1]])
vic.score.race2 = as.data.frame(lavPredict(fit.VIC.scapart.race)[[2]])
vic.score.race3 = as.data.frame(lavPredict(fit.VIC.scapart.race)[[3]])
vic.ids.race1  = as.data.frame(inspect(fit.VIC.scapart.race, "case.idx")[[1]])
vic.ids.race2  = as.data.frame(inspect(fit.VIC.scapart.race, "case.idx")[[2]])
vic.ids.race3  = as.data.frame(inspect(fit.VIC.scapart.race, "case.idx")[[3]])

vic_dat.race1  = cbind(vic.ids.race1,vic.score.race1)
vic_dat.race2  = cbind(vic.ids.race2,vic.score.race2)
vic_dat.race3  = cbind(vic.ids.race3,vic.score.race3)
colnames(vic_dat.race1)[1] = "ID"
colnames(vic_dat.race2)[1] = "ID"
colnames(vic_dat.race3)[1] = "ID"
vic_dat.race1$race = 1
vic_dat.race2$race = 3
vic_dat.race3$race = 2
vic_dat.race = rbind(vic_dat.race1, vic_dat.race2, vic_dat.race3)

#Internalized Homophobia
syntax.IH.config.race <- measEq.syntax(configural.model = MI.IH, 
                                      data = data, 
                                      parameterization = "theta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "race",
                                      group.equal = "thresholds",
                                      missing = "fiml")
cat(as.character(syntax.IH.config.race))
summary(syntax.IH.config.race)
mod.IH.config.race <- as.character(syntax.IH.config.race)
cat(mod.IH.config.race)
fit.IH.config.race <- cfa(mod.IH.config.race, data = data, parameterization = "theta", group = "race")
lavInspect(fit.IH.config.race, "cov.lv")
summary(fit.IH.config.race)
#Metric model
syntax.IH.metric.race <- measEq.syntax(configural.model = MI.IH, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "race", 
                                      group.equal = c("thresholds","loadings"),
                                      missing = "fiml")

summary(syntax.IH.metric.race)
mod.IH.metric.race <- as.character(syntax.IH.metric.race)
cat(mod.IH.metric.race)
fit.IH.metric.race <- cfa(mod.IH.metric.race, data = data, parameterization = "theta", group = "race") 
summary(fit.IH.metric.race)

# Scalar model
syntax.IH.scalar.race <- measEq.syntax(configural.model = MI.IH, data = data,
                                      parameterization = "theta",
                                      ID.fac = "std.lv", 
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "race", 
                                      group.equal = c("thresholds","loadings",
                                                      "intercepts"),
                                      missing = "fiml")

summary(syntax.IH.scalar.race)
mod.IH.scalar.race <- as.character(syntax.IH.scalar.race)
cat(mod.IH.scalar.race)
fit.IH.scalar.race <- cfa(mod.IH.scalar.race, data = data, parameterization = "theta", group = "race") 
summary(fit.IH.scalar.race)

# Model fit indices 
fit.IH.race <- compareFit(fit.IH.config.race, fit.IH.metric.race, fit.IH.scalar.race) 
summary(fit.IH.race) 
#scalar invariance not met as change in rmsea is |.037| 
#allowing for partial invariance so checking which parameter needs to be freed
lavTestScore(fit.IH.scalar.race)
parTable(fit.IH.scalar.race)

#Partial scalar model
syntax.IH.scapart.race <- measEq.syntax(configural.model = MI.IH, data = data,
                                         parameterization = "theta",
                                         ID.fac = "std.lv", 
                                         ID.cat = "Wu.Estabrook.2016",
                                         group = "race", 
                                         group.equal = c("thresholds","loadings",
                                                         "intercepts"),
                                         missing = "fiml", group.partial = c("ih_1 ~ 1",
                                                                             "ih_3 ~ 1"))
summary(syntax.IH.scapart.race)
mod.IH.scapart.race <- as.character(syntax.IH.scapart.race)
cat(mod.IH.scapart.race)
fit.IH.scapart.race <- cfa(mod.IH.scapart.race, data = data, parameterization = "theta", group = "COHORT") 
summary(fit.IH.scapart.race)
#comparing models
fit.IH.part.race <- compareFit(fit.IH.config.race, fit.IH.metric.race,fit.IH.scapart.race) 
summary(fit.IH.part.race) 
#Still not great but nothing else seems to improve fit so we will move forward and note limitations

ih.score.race1 = as.data.frame(lavPredict(fit.IH.scapart.race)[[1]])
ih.score.race2 = as.data.frame(lavPredict(fit.IH.scapart.race)[[2]])
ih.score.race3 = as.data.frame(lavPredict(fit.IH.scapart.race)[[3]])
ih.ids.race1  = as.data.frame(inspect(fit.IH.scapart.race, "case.idx")[[1]])
ih.ids.race2  = as.data.frame(inspect(fit.IH.scapart.race, "case.idx")[[2]])
ih.ids.race3  = as.data.frame(inspect(fit.IH.scapart.race, "case.idx")[[3]])

ih_dat.race1  = cbind(ih.ids.race1,ih.score.race1)
ih_dat.race2  = cbind(ih.ids.race2,ih.score.race2)
ih_dat.race3  = cbind(ih.ids.race3,ih.score.race3)
colnames(ih_dat.race1)[1] = "ID"
colnames(ih_dat.race2)[1] = "ID"
colnames(ih_dat.race3)[1] = "ID"
ih_dat.race1$race = 1
ih_dat.race2$race = 3
ih_dat.race3$race = 2
ih_dat.race = rbind(ih_dat.race1, ih_dat.race2, ih_dat.race3)

#Identity Concealment
syntax.CON.config.race <- measEq.syntax(configural.model = MI.CON, 
                                       data = data, 
                                       parameterization = "theta",
                                       ID.fac = "std.lv",
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "race",
                                       group.equal = "thresholds",
                                       missing = "fiml")
cat(as.character(syntax.CON.config.race))
summary(syntax.CON.config.race)
mod.CON.config.race <- as.character(syntax.CON.config.race)
cat(mod.CON.config.race)
fit.CON.config.race <- cfa(mod.CON.config.race, data = data, parameterization = "theta", group = "race")
summary(fit.CON.config.race)
#metric model
syntax.CON.metric.race <- measEq.syntax(configural.model = MI.CON, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "race", 
                                       group.equal = c("thresholds","loadings"),
                                       missing = "fiml")

summary(syntax.CON.metric.race)
mod.CON.metric.race <- as.character(syntax.CON.metric.race)
cat(mod.CON.metric.race)
fit.CON.metric.race <- cfa(mod.CON.metric.race, data = data, parameterization = "theta", group = "race") 
summary(fit.CON.metric.race)

# Scalar model
syntax.CON.scalar.race <- measEq.syntax(configural.model = MI.CON, data = data,
                                       parameterization = "theta",
                                       ID.fac = "std.lv", 
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "race", 
                                       group.equal = c("thresholds","loadings",
                                                       "intercepts"),
                                       missing = "fiml")

summary(syntax.CON.scalar.race)
mod.CON.scalar.race <- as.character(syntax.CON.scalar.race)
cat(mod.CON.scalar.race)
fit.CON.scalar.race <- cfa(mod.CON.scalar.race, data = data, parameterization = "theta", group = "race") 
summary(fit.CON.scalar.race)

# Model fit indices 
fit.CON.race <- compareFit(fit.CON.config.race, fit.CON.metric.race, fit.CON.scalar.race) 
summary(fit.CON.race) 
#metric invariance not met as change in rmsea is |.028|
#allowing for partial invariance so checking which parameter needs to be freed
lavTestScore(fit.CON.metric.race)
parTable(fit.CON.metric.race)
#no one item seems to be contributing the most to misfit, so we will allow the misfit
#but note this as a limitation

con.score.race1 = as.data.frame(lavPredict(fit.CON.scalar.race)[[1]])
con.score.race2 = as.data.frame(lavPredict(fit.CON.scalar.race)[[2]])
con.score.race3 = as.data.frame(lavPredict(fit.CON.scalar.race)[[3]])
con.ids.race1  = as.data.frame(inspect(fit.CON.scalar.race, "case.idx")[[1]])
con.ids.race2  = as.data.frame(inspect(fit.CON.scalar.race, "case.idx")[[2]])
con.ids.race3  = as.data.frame(inspect(fit.CON.scalar.race, "case.idx")[[3]])

con_dat.race1  = cbind(con.ids.race1,con.score.race1)
con_dat.race2  = cbind(con.ids.race2,con.score.race2)
con_dat.race3  = cbind(con.ids.race3,con.score.race3)
colnames(con_dat.race1)[1] = "ID"
colnames(con_dat.race2)[1] = "ID"
colnames(con_dat.race3)[1] = "ID"
con_dat.race1$race = 1
con_dat.race2$race = 3
con_dat.race3$race = 2
con_dat.race = rbind(con_dat.race1, con_dat.race2, con_dat.race3)

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
  mutate(across(CHR, ~scale(.)[,1])) %>%
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

### RQ3. Group differences in minority stressors impacts on life satisfaction ####

#Age Groups
chr_dat.age <- chr_dat %>% select(ID, CHR, cohort)
list_dat.age = list(ls_dat.age,chr_dat.age,dis_dat.age,vic_dat.age,fs_dat.age,
               ih_dat.age, con_dat.age)

spec_data.age = list_dat.age %>% purrr::reduce(full_join,by = c("ID", "cohort")) 

colnames(spec_data.age)[2] = c("ls")
colnames(spec_data.age)[4:9] = c("chronic","evdis", "vic", "feltstig", "inthom", "conceal") 
                              
rq3.cohort <- setup(data = spec_data.age,      
             y = "ls",            
             x = c("feltstig", "evdis", "vic", "inthom", "conceal", "chronic"),       
             model = c("lm"),
             controls = c("feltstig", "evdis", "vic", "inthom", "conceal", "chronic"),
             subsets = list(cohort = c("1", "2", "3")))                 
results.cohort <- specr(rq3.cohort)
data.cohort = results.cohort[["data"]]
data.cohort$sig = ifelse(data.cohort$p.value < .05, "sig", "ns")

# Sexual Identity
chr_dat.sexor <- chr_dat %>% select(ID, CHR, sexor)
list_dat.sexor = list(ls_dat.sexor,chr_dat.sexor,dis_dat.sexor,vic_dat.sexor,fs_dat.sexor,
                    ih_dat.sexor, con_dat.sexor)

spec_data.sexor = list_dat.sexor %>% purrr::reduce(full_join,by = c("ID", "sexor")) 

colnames(spec_data.sexor)[2] = c("ls")
colnames(spec_data.sexor)[4:9] = c("chronic","evdis", "vic", "feltstig", "inthom", "conceal") 

rq3.sexor <- setup(data = spec_data.sexor,      
                   y = "ls",            
                   x = c("feltstig", "evdis", "vic", "inthom", "conceal", "chronic"),       
                   model = c("lm"),
                   controls = c("feltstig", "evdis", "vic", "inthom", "conceal", "chronic"),
                   subsets = list(sexor = c("1", "2", "3")))                 
results.sexor <- specr(rq3.sexor)
data.sexor = results.sexor[["data"]]
data.sexor$sig = ifelse(data.sexor$p.value < .05, "sig", "ns")

# Race / Ethnicity
chr_dat.race <- chr_dat %>% select(ID, CHR, race)
list_dat.race = list(ls_dat.race,chr_dat.race,dis_dat.race,vic_dat.race,fs_dat.race,
                      ih_dat.race, con_dat.race)

spec_data.race = list_dat.race %>% purrr::reduce(full_join,by = c("ID", "race")) 

colnames(spec_data.race)[2] = c("ls")
colnames(spec_data.race)[4:9] = c("chronic","evdis", "vic", "feltstig", "inthom", "conceal") 

rq3.race <- setup(data = spec_data.race,      
                  y = "ls",            
                  x = c("feltstig", "evdis", "vic", "inthom", "conceal", "chronic"),       
                  model = c("lm"),
                  controls = c("feltstig", "evdis", "vic", "inthom", "conceal", "chronic"),
                  subsets = list(race = c("1", "2", "3")))                 
results.race <- specr(rq3.race)
data.race = results.race[["data"]]
data.race$sig = ifelse(data.race$p.value < .05, "sig", "ns")

#####Combining and Visualizing Results####
#####RQ1#####
#Main Effects
main.rq1 = data.rq1[data.rq1$controls == "no covariates",] %>%
  select(x, estimate, conf.low, conf.high, p.value)

#Now getting effects from all models from specification curve
all.rq1 = data.rq1 %>%
      select(x, estimate, conf.low, conf.high) %>%
      group_by(x) %>%
      dplyr::summarize(m.est = mean(estimate, na.rm = T),
                       min.rng = range(estimate)[[1]],
                       max.rng = range(estimate)[[2]],
                       m.conf.low = mean(conf.low), 
                       m.conf.high = mean(conf.high),
                       min.ci = min(conf.low),
                       max.ci = max(conf.high)) 
all.rq1 

# Checking change in R2 following including all minority stressors
gen <- data.rq1 %>% 
  filter(controls == "no covariates") %>%
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
# Minority stressors explain about 3% additional variance in life satisfaction above and beyond
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

#Graphing the results
main.rq1 = main.rq1 %>%
  mutate(sig = ifelse(sign(conf.low) == sign(conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic")
                           , labels = c("General", "Sexual Identity\nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Victimization")),
         type = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic"),
                       labels = c("General", "Internal",
                                  "External","External",
                                  "Internal",
                                  "External")))

ord <- c("Sexual Identity\nConcealment", "Internalized\nHomophobia", "Victimization"
         , "Stigmatization", "Discrimination",  "General")

gg.rq1 = main.rq1 %>%
  select(x, estimate, conf.low, conf.high) 
gg.rq1$min.ci = NA
gg.rq1$max.ci = NA
colnames(gg.rq1) = c("x", "m.est", "m.conf.low", "m.conf.high", "min.ci", "max.ci")
gg.rq1 = gg.rq1 %>%
  mutate(sig = ifelse(sign(m.conf.low) == sign(m.conf.high), "sig", "ns"), 
         stress = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic")
                         , labels = c("General", "Sexual Identity\nConcealment",
                                      "Discrimination","Stigmatization",
                                      "Internalized\nHomophobia",
                                      "Victimization")),
         type = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic"),
                      labels = c("General", "Internal",
                                 "External","External",
                                 "Internal",
                                 "External")))
comp.rq1 = all.rq1 %>%
           select(-min.rng, -max.rng) %>%
           rbind(gg.rq1)

comp.rq1 = comp.rq1 %>%
  mutate(effect = rep(c("Adjusted", "Main"), c(6,6)),
         effect = factor(effect, levels = c("Adjusted", "Main"), labels = c("Adjusted", "Main")))

fig1 <- comp.rq1 %>%
  mutate(stress = factor(stress, ord)) %>%
  ggplot(aes(x = m.est, y = stress, fill = effect)) + 
  scale_shape_manual(name = "Sig", values = c(21, 22), labels = c("Significant", "Non-significant")) + 
  scale_fill_manual(name = "Effect", values=c("red4", "darksalmon"), labels = c("Main", "Adjusted")) +
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(nrow = 2, override.aes = list(shape= c(22,21))), 
         fill = guide_legend(nrow = 2, override.aes = list(colour = c("darksalmon", "red4")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = min.ci, xmax = max.ci), linewidth = 1, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = effect, shape = sig), size = 5, colour = "black", position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.75,.1), breaks = c(-.75, -.5,-.25, 0)) +
  geom_hline(aes(yintercept = 5.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 4.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 3.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 2.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 1.5), linetype = "solid", alpha = .2) +
  my_theme()
fig1

ggsave(fig1
       , file = "/Users/atnissen/Desktop/Figure2.png"
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
                   min.rng = range(estimate)[[1]],
                   max.rng = range(estimate)[[2]],
                   m.conf.low = mean(conf.low), 
                   m.conf.high = mean(conf.high),
                   min.ci = min(conf.low),
                   max.ci = max(conf.high)) 

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
                   min.rng = range(estimate)[[1]],
                   max.rng = range(estimate)[[2]],
                   m.conf.low = mean(conf.low), 
                   m.conf.high = mean(conf.high),
                   min.ci = min(conf.low),
                   max.ci = max(conf.high)) 


#Main effects
main.pss = pss[c(1,33,68,133,193, 225),]
main.pss = main.pss %>%
  mutate(sig = ifelse(sign(conf.low) == sign(conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss")
                           , labels = c("General","Sexual Identity\nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Victimization")),
         type = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss"),
                       labels = c("General", "Internal",
                                  "External","External",
                                  "Internal",
                                  "External"))) %>%
  select(x,term,estimate, p.value, conf.low, conf.high, sig, stress, type)

gg.pss = main.pss %>%
  select(x, estimate, conf.low, conf.high) 
gg.pss$min.ci = NA
gg.pss$max.ci = NA
colnames(gg.pss) = c("x", "m.est", "m.conf.low", "m.conf.high", "min.ci", "max.ci")
gg.pss = gg.pss %>%
  mutate(sig = ifelse(sign(m.conf.low) == sign(m.conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss")
                           , labels = c("General","Sexual Identity\nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Victimization"))
         ,type = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss"),
                        labels = c("General", "Internal",
                                   "External","External",
                                   "Internal",
                                   "External")))

comp.pss = all.pss %>%
  select(-min.rng, -max.rng) %>%
  mutate(sig = ifelse(sign(min.ci) == sign(max.ci), "sig", "ns")
         , stress = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss")
                           , labels = c("General","Sexual Identity\nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Victimization"))
         ,type = factor(x, levels = c("chronpss", "conpss", "dispss", "stigpss", "inthompss", "vicpss"),
                        labels = c("General", "Internal",
                                   "External","External",
                                   "Internal",
                                   "External"))) %>%
  rbind(gg.pss)

comp.pss = comp.pss %>%
  mutate(effect = rep(c("Adjusted", "Main"), c(6,6)),
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
  guides(shape = guide_legend(nrow = 2), fill = guide_legend(nrow = 2, override.aes = list(colour = c("springgreen", "darkgreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = min.ci, xmax = max.ci), linewidth = 1, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = effect, shape = sig), size = 5, position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.25,.25))+
  my_theme() + 
  ggtitle("Perceived Social\nSupport") +
  geom_hline(aes(yintercept = 5.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 4.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 3.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 2.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 1.5), linetype = "solid", alpha = .2) +
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
                                    "External"))) %>%
  select(x,term,estimate, p.value, conf.low, conf.high, sig, stress, type)

all.lcc = all.lcc %>%
  mutate(sig = ifelse(sign(min) == sign(max), "sig", "ns")
         , stress = factor(x, levels = c("chronlcc", "conlcc", "dislcc", "stiglcc", "inthomlcc", "viclcc")
                           , labels = c("General","Sexual Identity\nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Victimization")),
         type = factor(x, levels = c("chronlcc", "conlcc", "dislcc", "stiglcc", "inthomlcc", "viclcc"),
                      labels = c("General", "Internal",
                                 "External","External",
                                 "Internal",
                                 "External")))

#Combined
gg.lcc = main.lcc %>%
  select(x, estimate, conf.low, conf.high) 
gg.lcc$min.ci = NA
gg.lcc$max.ci = NA
colnames(gg.lcc) = c("x", "m.est", "m.conf.low", "m.conf.high", "min.ci", "max.ci")
gg.lcc = gg.lcc %>%
  mutate(sig = ifelse(sign(m.conf.low) == sign(m.conf.high), "sig", "ns"), 
         stress = factor(x, levels = c("chronlcc", "conlcc", "dislcc", "stiglcc", "inthomlcc", "viclcc")
                          , labels = c("General","Sexual Identity\nConcealment", 
                                       "Discrimination","Stigmatization",
                                       "Internalized\nHomophobia",
                                       "Victimization")),
         type = factor(x, levels = c("chronlcc", "conlcc", "dislcc", "stiglcc", "inthomlcc", "viclcc"),
                       labels = c("General", "Internal",
                                  "External","External",
                                  "Internal",
                                  "External")))
comp.lcc = all.lcc %>%
           select(-min.rng, -max.rng) %>%
           rename(min.ci = min, max.ci = max) %>%
           rbind(gg.lcc)

comp.lcc = comp.lcc %>%
  mutate(effect = rep(c("Adjusted", "Main"), c(6,6)),
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
         fill = guide_legend(nrow = 2, override.aes = list(colour = c("springgreen", "darkgreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = min.ci, xmax = max.ci), linewidth = 1, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = effect, shape = sig), size = 5, colour = "black", position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.25,.25))+
  my_theme() + 
  ggtitle("LGBTQ+ Community \nConnectedness") +
  geom_hline(aes(yintercept = 5.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 4.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 3.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 2.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 1.5), linetype = "solid", alpha = .2) +
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
                         , labels = c("General","Sexual Identity\nConcealment",
                                      "Discrimination","Stigmatization",
                                      "Internalized\nHomophobia",
                                      "Victimization"))
       ,type = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic"),
                      labels = c("General", "Internal",
                                 "External","External",
                                 "Internal",
                                 "External"))) %>%
  select(x,term,estimate, p.value, conf.low, conf.high, sig, stress, type)

#Adjusted effects
all.sic = sic %>%
  select(x, estimate, conf.low, conf.high) %>%
  group_by(x) %>%
  dplyr::summarize(m.est = mean(estimate, na.rm = T),
                   min.rng = range(estimate)[[1]],
                   max.rng = range(estimate)[[2]],
                   m.conf.low = mean(conf.low), 
                   m.conf.high = mean(conf.high),
                   min.ci = min(conf.low),
                   max.ci = max(conf.high)) 

all.sic = all.sic %>%
  mutate(sig = ifelse(sign(min.ci) == sign(max.ci), "sig", "ns")
          , stress = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic")
                            , labels = c("General","Sexual Identity\nConcealment",
                                         "Discrimination","Stigmatization",
                                         "Internalized\nHomophobia",
                                         "Victimization"))
         ,type = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic"),
                        labels = c("General", "Internal",
                                   "External","External",
                                   "Internal",
                                   "External")))

#Combined
gg.sic = main.sic %>%
  select(x, estimate, conf.low, conf.high) 
gg.sic$min.ci = NA
gg.sic$max.ci = NA
colnames(gg.sic) = c("x", "m.est", "m.conf.low", "m.conf.high", "min.ci", "max.ci")
gg.sic = gg.sic %>%
  mutate(sig = ifelse(sign(m.conf.low) == sign(m.conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic")
                           , labels = c("General","Sexual Identity\nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Victimization"))
         ,type = factor(x, levels = c("chronsic", "consic", "dissic", "stigsic", "inthomsic", "vicsic"),
                        labels = c("General", "Internal",
                                   "External","External",
                                   "Internal",
                                   "External")))
comp.sic = all.sic %>%
           select(-min.rng, -max.rng) %>%
           rbind(gg.sic)

comp.sic = comp.sic %>%
  mutate(effect = rep(c("Adjusted", "Main"), c(6,6)),
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
         fill = guide_legend(nrow = 2, override.aes = list(colour = c("springgreen", "darkgreen")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = min.ci, xmax = max.ci), linewidth = 1, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = effect, shape = sig), size = 5, position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.25,.25))+
  my_theme()+ 
  ggtitle("Sexual Identity \nCentrality") +
  geom_hline(aes(yintercept = 5.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 4.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 3.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 2.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 1.5), linetype = "solid", alpha = .2) +
  theme(plot.title = element_text(face = "bold", size = rel(1.5), hjust = .5))
mod3

#Combining all moderator graphs here
gg.mods <- ggarrange(mod1, mod2, mod3, 
                  ncol =3, nrow = 1,
                  labels = c("A", "B", "C"),
                  common.legend = T)

ggsave(gg.mods
       , file = "/Users/atnissen/Desktop/FigureS1.png"
       , width = 17
       , height = 8)

#####RQ3######
#Age Groups
#Main effects
main.cohort = data.cohort[data.cohort$controls == "no covariates",] %>%
  select(x, subsets, estimate, conf.low, conf.high, p.value)

main.cohort$cohort = ifelse(main.cohort$subsets == "1", "Younger",
                            ifelse(main.cohort$subsets == "2", "Middle", "Older"))

main.cohort = main.cohort %>%
  filter(subsets !="all") %>%
  mutate(sig = ifelse(sign(conf.low) == sign(conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronic", "evdis", "feltstig", "inthom", "conceal", "vic")
                           , labels = c("General", 
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Sexual Identity\nConcealment",
                                        "Victimization"))
         , cohort = factor(subsets, levels = c("1", "2", "3"),
                           labels = c("Younger", "Middle", "Older")))

ord.age <- c("Older", "Middle", "Younger")

age1 <- main.cohort %>%
  mutate(stress = factor(stress, ord)
         , cohort = factor(cohort, ord.age)) %>%
  ggplot(aes(x = estimate, y = stress, group = cohort)) + 
  scale_fill_manual(name = "Age\nGroup", values=c("coral1", "coral3", "coral4"), labels = c("Younger","Middle","Older")) +
  scale_shape_manual(name = "Sig", values = c(22, 21), labels = c("Significant", "Non-significant")) + 
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(nrow = 2), 
         fill = guide_legend(nrow = 2,ncol = 2, order =1, override.aes = list(colour = c("coral4", "coral3", "coral")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = cohort, shape = sig), size = 5, position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.75,.25))+
  my_theme() +
  ggtitle("Main Effects") + 
  geom_hline(aes(yintercept = 5.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 4.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 3.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 2.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 1.5), linetype = "solid", alpha = .2) +
  annotate(geom = "segment", x = -.70, xend = -.70, y = 5.7, yend = 6.3, linewidth = 1) +
  annotate(geom = "segment", x = -.70, xend = -.68, y = 5.7, yend = 5.7, linewidth = 1) +
  annotate(geom = "segment", x = -.70, xend = -.68, y = 6.3, yend = 6.3, linewidth = 1) +
  annotate(geom = "text", label = "*", x = -.73, y = 5.9, size = 12) +
  annotate(geom = "segment", x = -.50, xend = -.50, y = 5, yend = 5.3, linewidth = 1) +
  annotate(geom = "segment", x = -.50, xend = -.48, y = 5.3, yend = 5.3, linewidth = 1) +
  annotate(geom = "segment", x = -.50, xend = -.48, y = 5, yend = 5, linewidth = 1) +
  annotate(geom = "text", label = "*", x = -.53, y = 5.05, size = 12) +
  theme(plot.title = element_text(face = "bold", size = rel(1.5), hjust = .5)
  )

#Adjusted effects
all.cohort = data.cohort %>%
  select(x, subsets,estimate, conf.low, conf.high) %>%
  group_by(x,subsets) %>%
  dplyr::summarize(m.est = mean(estimate, na.rm = T),
                   min.rng = range(estimate)[[1]],
                   max.rng = range(estimate)[[2]],
                   m.conf.low = mean(conf.low), 
                   m.conf.high = mean(conf.high),
                   min.ci = min(conf.low),
                   max.ci = max(conf.high)) %>%
  filter(!grepl('all',subsets))

all.cohort$cohort = ifelse(all.cohort$subsets == "1", "Young",
                           ifelse(all.cohort$subsets == "2", "Middle", "Old"))

all.cohort = all.cohort %>%
  mutate(sig = ifelse(sign(min.ci) == sign(max.ci), "sig", "ns")
         , stress = factor(x, levels = c("chronic", "evdis", "feltstig", "inthom", "conceal", "vic")
                           , labels = c("General", 
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Sexual Identity\nConcealment",
                                        "Victimization"))
         , cohort = factor(subsets, levels = c("1", "2", "3"),
                           labels = c("Younger", "Middle", "Older")))
age2 <- all.cohort %>%
  mutate(stress = factor(stress, ord)
         , cohort = factor(cohort, ord.age)) %>%
  ggplot(aes(x = m.est, y = stress, group = cohort)) + 
  scale_fill_manual(name = "Age\nGroup", values=c("coral1", "coral3", "coral4"), labels = c("Younger", "Middle",  "Older")) +
  scale_shape_manual(name = "Sig", values = c(21, 22), labels = c("Significant", "Non-significant")) + 
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(nrow = 2, override.aes = list(shape = c(22,21))), 
         fill = guide_legend(nrow = 2, ncol = 2, order = 1, override.aes = list(colour = c("coral4", "coral3", "coral")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = min.ci, xmax = max.ci), linewidth = 1, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = cohort, shape = sig), size = 5, position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.75,.25))+
  my_theme() +
  ggtitle("Adjusted Effects") +
  geom_hline(aes(yintercept = 5.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 4.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 3.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 2.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 1.5), linetype = "solid", alpha = .2) +
  theme(plot.title = element_text(face = "bold", size = rel(1.5), hjust = .5))

#Combined
age.legend <- get_legend(age2)
age.all <- ggarrange(age1, age2,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1, legend.grob = age.legend)

ggsave(age.all
       , file = "/Users/atnissen/Desktop/FigureS2.png"
       , width = 17
       , height = 8)

#Sexual Identity
#Main effects
main.sexor = data.sexor[data.sexor$controls == "no covariates",] %>%
  select(x, subsets, estimate, conf.low, conf.high, p.value)

main.sexor$sexor = ifelse(main.sexor$subsets == "1", "Gay/Lesbian",
                          ifelse(main.sexor$subsets == "2", "Bisexual", "Other SM\nIdentity"))
main.sexor = main.sexor %>%
  filter(subsets !="all") %>%
  mutate(sig = ifelse(sign(conf.low) == sign(conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic")
                           , labels = c("General", "Sexual Identity\nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Victimization"))
         , sexor = factor(subsets, levels = c("1", "2", "3"),
                          labels = c("Gay/Lesbian", "Bisexual", "Other SM\nIdentity")))

ord.sexor <- c("Other SM\nIdentity", "Bisexual", "Gay/Lesbian")

sexor1 <- main.sexor %>%
  mutate(stress = factor(stress, ord)
         , sexor = factor(sexor, ord.sexor)) %>%
  ggplot(aes(x = estimate, y = stress, group = sexor)) + 
  scale_fill_manual(name = "Sexual\nIdentity", values=c("plum1", "orchid", "darkviolet"), labels = c("Gay/Lesbian", "Bisexual", "Other SM\nIdentity")) +
  scale_shape_manual(name = "Sig", values = c(21, 22), labels = c("Significant", "Non-significant")) + 
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(nrow = 2, override.aes = list(shape = c(22, 21))), 
         fill = guide_legend(nrow = 2, ncol = 2, order = 1, override.aes = list(colour = c("darkviolet", "orchid", "plum1")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = sexor, shape = sig), size = 5, position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.75,.25))+
  my_theme() +
  ggtitle("Main Effects") +
  geom_hline(aes(yintercept = 5.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 4.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 3.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 2.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 1.5), linetype = "solid", alpha = .2) +
  theme(plot.title = element_text(face = "bold", size = rel(1.5), hjust = .5))

#Adjusted effects
all.sexor = data.sexor %>%
  select(x, subsets,estimate, conf.low, conf.high) %>%
  group_by(x,subsets) %>%
  dplyr::summarize(m.est = mean(estimate, na.rm = T),
                   min.rng = range(estimate)[[1]],
                   max.rng = range(estimate)[[2]],
                   m.conf.low = mean(conf.low), 
                   m.conf.high = mean(conf.high),
                   min.ci = min(conf.low),
                   max.ci = max(conf.high)) %>%
  filter(!grepl('all',subsets))

all.sexor$sexor = ifelse(all.sexor$subsets == "1", "Gay/Lesbian",
                  ifelse(all.sexor$subsets == "2", "Bisexual", "Other SM\nIdentity"))

all.sexor = all.sexor %>%
  mutate(sig = ifelse(sign(min.ci) == sign(max.ci), "sig", "ns")
         , stress = factor(x, levels = c("chronic", "conceal", "evdis", "feltstig", "inthom", "vic")
                           , labels = c("General", "Sexual Identity\nConcealment",
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Victimization"))
         , sexor = factor(subsets, levels = c("1", "2", "3"),
                          labels = c("Gay/Lesbian", "Bisexual", "Other SM\nIdentity")))

sexor2 <- all.sexor %>%
  mutate(stress = factor(stress, ord)
         , sexor = factor(sexor, ord.sexor)) %>%
  ggplot(aes(x = m.est, y = stress, group = sexor)) + 
  scale_fill_manual(name = "Sexual\nIdentity", values=c("plum1", "orchid", "darkviolet"), labels = c("Gay/Lesbian", "Bisexual", "Other SM\nIdentity")) +
  scale_shape_manual(name = "Sig", values = c(21, 22), labels = c("Significant", "Non-significant")) + 
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(nrow = 2, override.aes = list(shape = c(22, 21))), 
         fill = guide_legend(nrow = 2, ncol = 2, order = 1, override.aes = list(colour = c("darkviolet", "orchid", "plum1")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = min.ci, xmax = max.ci), linewidth = 1, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = sexor, shape = sig), size = 5, position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.75,.25))+
  my_theme() +
  ggtitle("Adjusted Effects") +
  geom_hline(aes(yintercept = 5.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 4.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 3.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 2.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 1.5), linetype = "solid", alpha = .2) +
  theme(plot.title = element_text(face = "bold", size = rel(1.5), hjust = .5))

#Combined
sexor.all <-ggarrange(sexor1, sexor2, 
                      labels = c("A", "B"),
                      ncol = 2, nrow = 1, 
                      common.legend = T)

ggsave(sexor.all
       , file = "/Users/atnissen/Desktop/FigureS3.png"
       , width = 17
       , height = 8)

#Race/Ethnicity
#Main effects
main.race = data.race[data.race$controls == "no covariates",] %>%
  select(x, subsets, estimate, conf.low, conf.high, p.value)

main.race$race = ifelse(main.race$subsets == "1", "White",
                        ifelse(main.race$subsets == "2", "Black", "Latine"))
main.race = main.race %>%
  filter(subsets !="all") %>%
  mutate(sig = ifelse(sign(conf.low) == sign(conf.high), "sig", "ns")
         , stress = factor(x, levels = c("chronic", "evdis", "feltstig", "inthom", "conceal", "vic")
                           , labels = c("General", 
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Sexual Identity\nConcealment",
                                        "Victimization"))
         , race = factor(subsets, levels = c("1", "2", "3"),
                         labels = c("White", "Black", "Latine")))

ord.race <- c("Latine", "Black", "White")

race1 <- main.race %>%
  mutate(stress = factor(stress, ord)
         , race = factor(race, ord.race)) %>%
  ggplot(aes(x = estimate, y = stress, group = race)) + 
  scale_fill_manual(name = "Race", values=c("cyan", "skyblue", "blue"), labels = c("White", "Black", "Latine")) +
  scale_shape_manual(name = "Sig", values = c(21, 22), labels = c("Significant", "Non-significant")) + 
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(nrow = 2, override.aes = list(shape = c(22,21))), 
         fill = guide_legend(nrow = 2, ncol = 2, order = 1, override.aes = list(colour = c("blue", "skyblue", "cyan")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = race, shape = sig), size = 5, position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.75,.5))+
  my_theme() +
  ggtitle("Main Effects") + 
  geom_hline(aes(yintercept = 5.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 4.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 3.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 2.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 1.5), linetype = "solid", alpha = .2) +
  annotate(geom = "segment", x = -.70, xend = -.70, y = 6, yend = 6.3, linewidth = 1) +
  annotate(geom = "segment", x = -.70, xend = -.68, y = 6.3, yend = 6.3, linewidth = 1) +
  annotate(geom = "segment", x = -.70, xend = -.68, y = 6, yend = 6, linewidth = 1) +
  annotate(geom = "text", label = "*", x = -.73, y = 6.05, size = 12) +
  annotate(geom = "segment", x = -.50, xend = -.50, y = 2.7, yend = 3.3, linewidth = 1) +
  annotate(geom = "segment", x = -.50, xend = -.48, y = 3.3, yend = 3.3, linewidth = 1) +
  annotate(geom = "segment", x = -.50, xend = -.48, y = 2.7, yend = 2.7, linewidth = 1) +
  annotate(geom = "text", label = "*", x = -.53, y = 2.9, size = 12) +
  theme(plot.title = element_text(face = "bold", size = rel(1.5), hjust = .5))

#Adjusted effects
all.race = data.race %>%
  filter(!grepl('all',subsets)) %>%
  select(x, subsets,estimate, conf.low, conf.high) %>%
  group_by(x,subsets) %>%
  dplyr::summarize(m.est = mean(estimate, na.rm = T),
                   min.rng = range(estimate)[[1]],
                   max.rng = range(estimate)[[2]],
                   m.conf.low = mean(conf.low), 
                   m.conf.high = mean(conf.high),
                   min.ci = min(conf.low),
                   max.ci = max(conf.high)) %>%
  filter(!grepl('all',subsets))


all.race$race = ifelse(all.race$subsets == "1", "White",
                         ifelse(all.race$subsets == "2", "Black", "Latine"))

all.race = all.race %>%
  mutate(sig = ifelse(sign(min.ci) == sign(max.ci), "sig", "ns")
         , stress = factor(x, levels = c("chronic", "evdis", "feltstig", "inthom", "conceal", "vic")
                           , labels = c("General",
                                        "Discrimination","Stigmatization",
                                        "Internalized\nHomophobia",
                                        "Sexual Identity\nConcealment",
                                        "Victimization"))
         , race = factor(subsets, levels = c("1", "2", "3"),
                           labels = c("White", "Black", "Latine")))

race2 <- all.race %>%
  mutate(stress = factor(stress, ord)
         , race = factor(race, ord.race)) %>%
  ggplot(aes(x = m.est, y = stress, group = race)) + 
  scale_fill_manual(name = "Race", values=c("cyan", "skyblue", "blue"), labels = c("White", "Black", "Latine")) +
  scale_shape_manual(name = "Sig", values = c(21, 22), labels = c("Significant", "Non-significant")) + 
  labs(x = "Standardized Effect Size",
       y = "Stressor",
       fill = NULL,
       shape = NULL) +
  guides(shape = guide_legend(nrow = 2, override.aes = list(shape = c(22,21))), 
         fill = guide_legend(nrow = 2, ncol = 2, order = 1, override.aes = list(colour = c("blue", "skyblue", "cyan")))) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_errorbar(aes(xmin = m.conf.low, xmax = m.conf.high), linewidth = 2, width = 0, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = min.ci, xmax = max.ci), linewidth = 1, width = 0, position = position_dodge(width = 0.9)) +
  geom_point(aes(fill = race, shape = sig), size = 5, position = position_dodge(width = 0.9)) +
  scale_x_continuous(limits = c(-.75,.5))+
  my_theme() +
  ggtitle("Adjusted Effects") +
  geom_hline(aes(yintercept = 5.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 4.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 3.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 2.5), linetype = "solid", alpha = .2) +
  geom_hline(aes(yintercept = 1.5), linetype = "solid", alpha = .2) +
  theme(plot.title = element_text(face = "bold", size = rel(1.5), hjust = .5))

#Combined
race.all <- ggarrange(race1, race2, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1, 
          common.legend = T)

ggsave(race.all
       , file = "/Users/atnissen/Desktop/FigureS4.png"
       , width = 17
       , height = 8)

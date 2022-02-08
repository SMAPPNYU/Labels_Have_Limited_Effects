
########################################################################################################################################################
### Title: News Credibility Labels Improve News Diets, Reduce Misperceptions, and Increase Media Trust
### Authors: Kevin Aslett, Andrew Guess, Jonathan Nagler, Richard Bonneaua, and Joshua A. Tucker
### Purpose of code: Produce figures and tables of analyses that are displayed in the supplementary materials and methods

##### Data In:
# (1) ./Data/Clean_NewsGuard_Digital_Trace_Data.csv
# (2) ./Data/Clean_NewsGuard_Survey_Study.csv
# (3) './Data/NYUU0017_w1_OUTPUT.csv'
# (4) './Data/NYUU0017_w2_OUTPUT.csv'
# (5) ./Data/NewsGuard_Ratings.csv
# (6) ./Data/pulsebyday.csv

##### Data Out: NONE

########################################################################################################################################################

#Load libraries:
library(dplyr)
library(xtable)
library(AER)
library(glmnet)
library(estimatr)
library(huxtable)
library(magrittr)
library(texreg)
library(ivpack)
library(ggplot2)
library(haven)
library(dplyr)
library(tidyverse)
library(ivdesc)
library(pwr)

#Read in survey data with digital trace data:
Pulse_data <- read_csv('./Data/Clean_NewsGuard_Digital_Trace_Data.csv',
                       col_types = cols(
                         .default= col_character(),
                         Treated = col_double(),
                         Complied=col_double(),
                         Total_DL =  col_double(),
                         Inverse_DL = col_double(),
                         Total_Science_Misinfo  =  col_double(),
                         Social_Media_Use =  col_double(),
                         income_score =  col_double(),
                         abs_part =  col_double(),
                         mean_cons =  col_double(),
                         Average_domain_NewsG_Score_post= col_double(),
                         Average_domain_NewsG_Score= col_double(),
                         Prop_Unreliable_NewsG_Score_post= col_double(),
                         Prop_Unreliable_NewsG_Score= col_double(),
                         Prop_Reliable_NewsG_Score_post = col_double(),
                         Prop_Reliable_NewsG_Score = col_double(),
                         Count_Unreliable_NewsG_Score_post = col_double(),
                         Count_Unreliable_NewsG_Score = col_double(),
                         Count_Reliable_NewsG_Score_post = col_double(),
                         Count_Reliable_NewsG_Score = col_double(),
                         Prop_Unreliable_NewsG_Score_dv = col_double(),
                         Average_domain_NewsG_Score_dv = col_double(),
                         Prop_Reliable_NewsG_Score_dv = col_double(),
                         Count_Unreliable_NewsG_Score_dv = col_double(),
                         Count_Reliable_NewsG_Score_dv = col_double(),
                         gender_dummy_fem=col_double(),
                         educ_score=col_double(),
                         Age=col_double(),
                         Age_Sq=col_double(),
                         party_score=col_double(),
                         race_white=col_double(),
                         ideo_score=col_double(),
                         Trust_Media_w1=col_double(),
                         trust_news=col_double(),
                         trust_news_sm=col_double(),
                         cons_news_n=col_double(),
                         cons_cable=col_double(),
                         cons_print=col_double(),
                         cons_public=col_double(), 
                         cons_talk=col_double(),
                         cons_desk=col_double(),
                         cons_mobile=col_double(),
                         Safari_dummy=col_double(),
                         log_news=col_double()
                       ))



#Read in survey data without digital trace data:

data_frame_1 <- read_csv('./Data/Clean_NewsGuard_Survey_Study.csv',
                         col_types = cols(
                           .default= col_character(),
                           Treated = col_double(),
                           Pulse_Dummy=col_double(),
                           Complied=col_double(),
                           Total_DL =  col_double(),
                           Inverse_DL = col_double(),
                           income_score =  col_double(),
                           Total_Science_Misinfo  =  col_double(),
                           Social_Media_Use =  col_double(),
                           SMP4310=col_double(),
                           SMP4310_w2=col_double(),
                           SMP4326=col_double(),
                           SMP4326_w2=col_double(),
                           Trust_inst_w2=col_double(),
                           Trust_inst_w1=col_double(),
                           BLM_Misinfo_Index_w2=col_double(),
                           Covid_Misinfo_Index_w2=col_double(),
                           BLM_info_Index_w2=col_double(),
                           Covid_info_Index_w2=col_double(),
                           Pol_cyn_2=col_double(),
                           Pol_cyn_1=col_double(),
                           Fox_Trust_2=col_double(),
                           Fox_Trust_1=col_double(),
                           CNN_Trust_2=col_double(),
                           CNN_Trust_1=col_double(),
                           ABC_Trust_2=col_double(),
                           ABC_Trust_1=col_double(),
                           NBC_Trust_2=col_double(),
                           NBC_Trust_1=col_double(),
                           CBS_Trust_2=col_double(),
                           CBS_Trust_1=col_double(),
                           aff_pol_w2=col_double(),
                           aff_pol_w1=col_double(),
                           Trust_Media_w2=col_double(),
                           Treated=col_double(),
                           Trust_Media_w1=col_double(),
                           gender_dummy_fem=col_double(),
                           educ_score=col_double(),
                           Age=col_double(),
                           Age_Sq=col_double(),
                           party_score=col_double(),
                           race_white=col_double(),
                           ideo_score=col_double(),
                           Trust_Media_w1=col_double(),
                           trust_news=col_double(),
                           trust_news_sm=col_double(),
                           cons_news_n=col_double(),
                           cons_cable=col_double(),
                           cons_print=col_double(),
                           cons_public=col_double(), 
                           cons_talk=col_double(),
                           cons_desk=col_double(),
                           cons_mobile=col_double(),
                           Safari_dummy=col_double()
                         ))

data_frame_1 <- data_frame_1[order(data_frame_1$visa1),]

#Table 1: Produce Descriptive statistics for sample by treatment and control groups:

Summary_stats <- data_frame_1 %>% mutate(Groups = ifelse(Treated == 1 & !is.na(Pulse_Dummy),'Treated and we have digital trace data','Treated and we do not have digital trace data'))
Summary_stats <- Summary_stats %>% mutate(Groups = ifelse(Treated == 0 & !is.na(Pulse_Dummy),'Control and we have digital trace data',Groups))
Summary_stats <- Summary_stats %>% mutate(Groups = ifelse(Treated == 0 & is.na(Pulse_Dummy),'Control and we do not have digital trace data',Groups))

data_groups <- Summary_stats %>% group_by(Groups) %>% mutate(Mean_Age = mean(Age,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Mean_DL = mean(Total_DL,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Mean_income = mean(income_score,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Prop_Female = mean(gender_dummy_fem,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Prop_White = mean(race_white,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Mean_educ = mean(educ_score,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Mean_ideo = mean(ideo_score,na.rm=T))

mean_data_groups <- data_groups %>% select(Groups,
                                           Mean_Age,
                                           Mean_DL,
                                           Mean_income,
                                           Prop_Female,
                                           Prop_White,
                                           Mean_educ,
                                           Mean_ideo)

count_groups <- mean_data_groups %>% group_by(Groups) %>% count()
mean_data_groups <- unique(mean_data_groups)

mean_data_groups <- merge(count_groups,mean_data_groups,by='Groups') 

#Create Xtable Object:
xt <- xtable(mean_data_groups,
             digits=2,
             align=c(
               "p{1cm}|","|p{4cm}|",
               "p{1cm}|","p{1cm}|",
               "p{1.5cm}|","p{1cm}|",
               "p{1.5cm}|","p{1.5cm}|",
               "p{1.5cm}|","p{1.5cm}|"))

#Name Columns:
names(xt) <- c('Group','Observ.','Age','Dig. Lit.','Income','Gender (Prop. Female)','Race (Prop. White)','Education','Ideology' )

#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity),file='./Tables/Table_2.txt')




#Table 2: Descriptive statistics for sample by treatment and control groups by attrition:

#Read in Data:
Wave_1_Data <- read.csv('./Data/NYUU0017_w1_OUTPUT.csv')
Wave_2_Data <- read.csv('./Data/NYUU0017_w2_OUTPUT.csv')

#Change the columnnames that were the same in wave 1 and wave 2
colnames(Wave_2_Data)[89:114] <- c('operating_system_w2',
                                   'web_browser_w2',
                                   'device_w2',
                                   'birthyr_w2',
                                   'gender_w2',
                                   'race_w2',
                                   'educ_w2',
                                   'marstat_w2',
                                   'employ_w2',
                                   'faminc_new_w2',
                                   'pid3_w2',
                                   'pid7_w2',
                                   'presvote16post_w2',
                                   'inputzip_w2',
                                   'inputstate_w2',
                                   'region_w2',
                                   'votereg_w2',
                                   'ideo5_w2',
                                   'newsint_w2',
                                   'religpew_w2',
                                   'pew_churatd_w2',
                                   'pew_bornagain_w2',
                                   'pew_religimp_w2',
                                   'pew_prayer_w2',
                                   'startime_w2',
                                   'endtime_w2')

colnames(Wave_2_Data)[1:2] <- c('caseid_w2',
                                'weight_w2')  
colnames(Wave_2_Data)[5] <- c('visa1_w2')                           

#Merge based on caseid in Wave 1 survey and caseid_w1 in Wave 2 Survey
Waves <- merge(Wave_1_Data,Wave_2_Data,by.x='caseid',by.y='caseid_w1',all='T')

Attrition <- Waves %>% select(caseid,caseid_w2)
Attrition <- Attrition %>% mutate(Both_Waves = ifelse(is.na(caseid_w2),0,1)) %>% select(caseid,Both_Waves)
Attrition_Data <- merge(Wave_1_Data,Attrition,by='caseid')


# Create Treatment Variable
Attrition_Data$plugin_treat <- as.character(Attrition_Data$plugin_treat)
Attrition_Data$Treated <- ifelse(Attrition_Data$plugin_treat == 'Offered plug-in',1,0)


#Age
Attrition_Data <- Attrition_Data %>% mutate(Age = 2020 - birthyr)

#Education
Attrition_Data$educ <- factor(Attrition_Data$educ, levels = c('No HS',
                                                              'High school graduate',
                                                              'Some college',
                                                              '2-year',
                                                              '4-year',
                                                              'Post-grad'))
Attrition_Data$educ_score <- as.numeric(Attrition_Data$educ)

#Income
Attrition_Data$faminc_new  <- factor(Attrition_Data$faminc_new , levels = c("Less than $10,000",
                                                                            "$10,000 - $19,999",
                                                                            "$20,000 - $29,999",
                                                                            "$30,000 - $39,999",
                                                                            "$40,000 - $49,999",
                                                                            "$50,000 - $59,999",
                                                                            "$60,000 - $69,999",
                                                                            "$70,000 - $79,999",
                                                                            "$80,000 - $89,999",
                                                                            "$90,000 - $99,999",
                                                                            "$100,000 - $119,999",
                                                                            "$120,000 - $149,999",
                                                                            "$150,000 - $199,999",
                                                                            "$200,000 - $249,999",
                                                                            "$250,000 - $349,999",
                                                                            "$350,000 - $499,999",
                                                                            "$500,000 or more"))
Attrition_Data$income_score  <- as.numeric(Attrition_Data$faminc_new)

#Digital Literacy
##Create digital literacy variable:

#Run for loop to set the columns 18-23 as factors with the same levels
dl_matrix <- matrix(nrow=nrow(Attrition_Data))
for(i in 18:23){
  Attrition_Data[,i]  <- factor(Attrition_Data[,i], levels = c('No Understanding',
                                                               '2',
                                                               '3',
                                                               '4',
                                                               'Full Understanding'))
  
  dl_matrix <- cbind(dl_matrix,Attrition_Data[,i])
}
#Remove the first row in the dataset (Consists solely of NAs)
dl_matrix <- dl_matrix[,-1]

#Run for loop to set the columns 25-26 as factors with the same levels
for(i in c(25,26)){
  Attrition_Data[,i]  <- factor(Attrition_Data[,i], levels = c('Strongly Disagree',
                                                               '-3',
                                                               '-2',
                                                               '-1',
                                                               '0',
                                                               '1',
                                                               '2',
                                                               '3',
                                                               'Strongly Agree'))
  dl_matrix <- cbind(dl_matrix,Attrition_Data[,i])
}

#Run for loop to set the columns 24-27 as factors with the same levels
for(i in c(24,27)){
  Attrition_Data[,i]  <- factor(Attrition_Data[,i], levels = c('Strongly Agree',
                                                               '3',
                                                               '2',
                                                               '1',
                                                               '0',
                                                               '-1',
                                                               '-2',
                                                               '-3',
                                                               'Strongly Disagree'))
  dl_matrix <- cbind(dl_matrix,Attrition_Data[,i])
}

#We take the matrix with the all of the answers to our digital literacy question and convert the matrix into 
dl_matrix <- data.frame(dl_matrix)

#Rename the columns to correspond with each digital literacy question:
colnames(dl_matrix) <- c('DL_1','DL_2','DL_3','DL_4','DL_5','DL_6','DL_7','DL_8','DL_9','DL_10')

#Sum the values assigned from each digital literacy question:
dl_matrix <- dl_matrix %>% mutate(Total_DL = DL_1 + DL_2 + DL_3 + DL_4 + DL_5 + DL_6 + DL_7 + DL_8 + DL_9 + DL_10)
dl_matrix <- dl_matrix %>% select(Total_DL)


Attrition_Data <- cbind(Attrition_Data,dl_matrix)




#Gender
Attrition_Data$gender <- as.character(Attrition_Data$gender)
Attrition_Data <- Attrition_Data %>% mutate(gender_dummy_fem = ifelse(gender == 'Female',1,0))

#Race
Attrition_Data <- Attrition_Data %>% mutate(race_white = ifelse(race == 'White',1,0))

#Ideology
Attrition_Data$SMP8201 <- factor(Attrition_Data$SMP8201, levels = c('Very liberal',
                                                                    'Somewhat liberal',
                                                                    'Slightly liberal',
                                                                    'Moderate; middle of the road',
                                                                    'Slightly conservative',
                                                                    'Somewhat conservative',
                                                                    'Very conservative'))


Attrition_Data$ideo_score <- as.numeric(Attrition_Data$SMP8201) - 4


#Calculate statistics for treatment group by attrition
Attrition_Data <- Attrition_Data %>% mutate(Groups = ifelse(Treated == 1 & Both_Waves == 0,'Treated and did not take the Wave 2 survey','Treated and took both surveys'))
Attrition_Data <- Attrition_Data %>% mutate(Groups = ifelse(Treated == 0 & Both_Waves == 0,'Control and did not take the Wave 2 survey',Groups))
Attrition_Data <- Attrition_Data %>% mutate(Groups = ifelse(Treated == 0 & Both_Waves == 1,'Control and took both surveys',Groups))

data_groups <- Attrition_Data %>% group_by(Groups) %>% mutate(Mean_Age = mean(Age,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Mean_DL = mean(Total_DL,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Mean_income = mean(income_score,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Prop_Female = mean(gender_dummy_fem,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Prop_White = mean(race_white,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Mean_educ = mean(educ_score,na.rm=T))
data_groups <- data_groups %>% group_by(Groups) %>% mutate(Mean_ideo = mean(ideo_score,na.rm=T))

mean_data_groups <- data_groups %>% select(Groups,
                                           Mean_Age,
                                           Mean_DL,
                                           Mean_income,
                                           Prop_Female,
                                           Prop_White,
                                           Mean_educ,
                                           Mean_ideo)

count_groups <- mean_data_groups %>% group_by(Groups) %>% count()
mean_data_groups <- unique(mean_data_groups)

mean_data_groups <- merge(count_groups,mean_data_groups,by='Groups') 



#Create Xtable Object:
xt <- xtable(mean_data_groups,
             digits=2,
             align=c(
               "p{1cm}|","|p{4cm}|",
               "p{1cm}|","p{1cm}|",
               "p{1.5cm}|","p{1cm}|",
               "p{1.5cm}|","p{1.5cm}|",
               "p{1.5cm}|","p{1.5cm}|"))

#Name Columns:
names(xt) <- c('Group','Observ.','Age','Dig. Lit.','Income','Gender (Prop. Female)','Race (Prop. White)','Education','Ideology' )

#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity),file='./Tables/Table_1.txt')


#Figure 1 and 2:


#Create Democrat and Republican dummy variable and make all variables proportions of maximum and minimum:

data_frame_1$Age <- (data_frame_1$Age-min(data_frame_1$Age))/(max(data_frame_1$Age)-min(data_frame_1$Age))
data_frame_1$gender_dummy_fem <- (data_frame_1$gender_dummy_fem-min(data_frame_1$gender_dummy_fem))/(max(data_frame_1$gender_dummy_fem)-min(data_frame_1$gender_dummy_fem))
data_frame_1$race_white <- (data_frame_1$race_white-min(data_frame_1$race_white))/(max(data_frame_1$race_white)-min(data_frame_1$race_white))
data_frame_1$educ_score <- (data_frame_1$educ_score-min(data_frame_1$educ_score))/(max(data_frame_1$educ_score)-min(data_frame_1$educ_score))
data_frame_1$dem_dummy <- ifelse(data_frame_1$party_score < 4,1,0)
data_frame_1$rep_dummy <- ifelse(data_frame_1$party_score > 4,1,0)
data_frame_1$Total_DL <- (data_frame_1$Total_DL-min(data_frame_1$Total_DL,na.rm=T))/(max(data_frame_1$Total_DL,na.rm=T)-min(data_frame_1$Total_DL,na.rm=T))
data_frame_1$income_score <- (data_frame_1$income_score-min(data_frame_1$income_score,na.rm=T))/(max(data_frame_1$income_score,na.rm=T)-min(data_frame_1$income_score,na.rm=T))


#Create Democrat and Republican dummy variable and make all variables proportions of maximum and minimum:

Pulse_data$Age <- (Pulse_data$Age-min(Pulse_data$Age))/(max(Pulse_data$Age)-min(Pulse_data$Age))
Pulse_data$gender_dummy_fem <- (Pulse_data$gender_dummy_fem-min(Pulse_data$gender_dummy_fem))/(max(Pulse_data$gender_dummy_fem)-min(Pulse_data$gender_dummy_fem))
Pulse_data$race_white <- (Pulse_data$race_white-min(Pulse_data$race_white))/(max(Pulse_data$race_white)-min(Pulse_data$race_white))
Pulse_data$educ_score <- (Pulse_data$educ_score-min(Pulse_data$educ_score))/(max(Pulse_data$educ_score)-min(Pulse_data$educ_score))
Pulse_data$dem_dummy <- ifelse(Pulse_data$party_score < 4,1,0)
Pulse_data$rep_dummy <- ifelse(Pulse_data$party_score > 4,1,0)
Pulse_data$Total_DL <- (Pulse_data$Total_DL-min(Pulse_data$Total_DL,na.rm=T))/(max(Pulse_data$Total_DL,na.rm=T)-min(Pulse_data$Total_DL,na.rm=T))
Pulse_data$income_score <- (Pulse_data$income_score-min(Pulse_data$income_score,na.rm=T))/(max(Pulse_data$income_score,na.rm=T)-min(Pulse_data$income_score,na.rm=T))
Pulse_data$Prop_Unreliable_NewsG_Score <- (Pulse_data$Prop_Unreliable_NewsG_Score-min(Pulse_data$Prop_Unreliable_NewsG_Score,na.rm=T))/(max(Pulse_data$Prop_Unreliable_NewsG_Score,na.rm=T)-min(Pulse_data$Prop_Unreliable_NewsG_Score,na.rm=T))
Pulse_data$Average_domain_NewsG_Score <- (Pulse_data$Average_domain_NewsG_Score-min(Pulse_data$Average_domain_NewsG_Score,na.rm=T))/(max(Pulse_data$Average_domain_NewsG_Score,na.rm=T)-min(Pulse_data$Average_domain_NewsG_Score,na.rm=T))



#Estimate means along various dimensions for respondents who would install the NewsGuard web browser extension if and only if they are assigned to receive it (“compliers”) and those who would not under any circumstances (“never-takers”) computed following the procedure in Marbach and Hangartner (2020).
#All respondents:

#Only select variabels needed:
dat <- data_frame_1 %>% 
  select(Total_DL,race_white,income_score,dem_dummy,rep_dummy,educ_score,gender_dummy_fem,Age,Treated,Complied) %>% 
  gather(var,val,-Treated,-Complied)
dat <- na.omit(dat)

#Set pre-registred seed
set.seed(938)


#Bootstrap
res <- dat %>% group_by(var) %>% do(m=ivdesc(X=.$val,D=.$Complied,Z=.$Treated, bootn=2000, boot=TRUE))
res_1 <- res
res <- unnest(res)

#Pull statistics from bootstrapping:
resB <- res %>% 
  select(var,group,mu,mu_se)



#Create confidence intervals:
res <- resB %>% 
  mutate(lo=mu+mu_se*qnorm(0.025), 
         hi=mu+mu_se*qnorm(0.975))

# Figure 1 
##########



#Variables
flabso <- c("Total_DL","race_white","income_score","dem_dummy",
            "rep_dummy","educ_score","gender_dummy_fem","Age")



#Samples
glabso <- rev(c("sample", "co", "nt", "at"))
res <- ungroup(res) %>% 
  mutate(var=factor(var, levels=flabso),
         group=factor(group, levels=glabso))



#Set labels for types of samples
xlabs <- c("co"="Complier", "nt"="Never-taker", 
           "at"="Always-taker", sample="Sample")


#We do not include the always-taker group, because it is rare:
res <- res %>% filter(group!='at')


#Set placement of confidence interval lines:
res$x <- c(0.95,0.85,0.75,1.5,1.4,1.3,2.0,1.9,1.8,2.5,2.4,2.3,3.0,2.9,2.8,3.5,3.4,3.3,4.0,3.9,3.8,4.5,4.4,4.3)


#Figure 1: Profile of all compliers and never-takers

ggplot(data = res, aes(x = x, y = mu)) +
  geom_point(aes(color = group),size=3) +
  geom_linerange(aes(min = lo, 
                     max = hi, 
                     color = group),size=1.5) +
  scale_color_manual(labels=xlabs,values=c('red','blue','purple'), name = "Type") +
  ylab("\n Mean Value") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 18),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(0,1) +
  scale_x_continuous(" \n",breaks=c(0.9,1.4,1.9,2.4,2.9,3.4,3.9,4.4),labels=c("Age",
                                                                              "Democrat",
                                                                              "Education*",
                                                                              "Gender (Female)",
                                                                              "Income*",
                                                                              "Race (White)",
                                                                              "Republican",
                                                                              "Digital Literacy*"),limits=c(0.5,5.0)) +
  coord_flip()


ggsave("./Figures/fig_1.png", width=11)



#Estimate means along various dimensions for respondents who would install the NewsGuard web browser extension if and only if they are assigned to receive it (“compliers”) and those who would not under any circumstances (“never-takers”) computed following the procedure in Marbach and Hangartner (2020).
#Only Those with whom we have digital trace data:




#Only select variabels needed:
dat <- Pulse_data %>% 
  select(Total_DL,race_white,income_score,dem_dummy,rep_dummy,educ_score,gender_dummy_fem,Age,Prop_Unreliable_NewsG_Score,Average_domain_NewsG_Score,Treated,Complied) %>% 
  gather(var,val,-Treated,-Complied)
dat <- na.omit(dat)


#Set pre-registred seed
set.seed(938)
#Bootstrap
res <- dat %>% group_by(var) %>% do(m=ivdesc(X=.$val,D=.$Complied,Z=.$Treated, bootn=2000, boot=TRUE))
res_1 <- res

res <- unnest(res)

#Pull statistics from bootstrapping:
res <- res %>% 
  select(var,group,mu,mu_se)


#Create confidence intervals:
res <- res %>% 
  mutate(lo=mu+mu_se*qnorm(0.025), 
         hi=mu+mu_se*qnorm(0.975))

#Variables
flabso <- c("Total_DL","race_white","income_score","dem_dummy",
            "rep_dummy","educ_score","gender_dummy_fem","Age",
            "Prop_Unreliable_NewsG_Score","Average_domain_NewsG_Score")

#Samples
glabso <- rev(c("sample", "co", "nt", "at"))
res <- ungroup(res) %>% 
  mutate(var=factor(var, levels=flabso),
         group=factor(group, levels=glabso))


#Set labels for types of samples
xlabs <- c("co"="Complier", "nt"="Never-taker", 
           "at"="Always-taker", sample="Sample")

#We do not include the always-taker group, because it is rare:
res <- res %>% filter(group!='at')

#Set placement of confidence interval lines:
res$x <- c(0.95,0.85,0.75,1.5,1.4,1.3,2.0,1.9,1.8,2.5,2.4,2.3,3.0,2.9,2.8,3.5,3.4,3.3,4.0,3.9,3.8,4.5,4.4,4.3,5.0,4.9,4.8,5.5,5.4,5.3)


#Figure 2: Profile of compliers and never-takers with whom we collect web-browsing data

ggplot(data = res, aes(x = x, y = mu)) +
  geom_point(aes(color = group),size=3) +
  geom_linerange(aes(min = lo, 
                     max = hi, 
                     color = group),size=1.5) +
  scale_color_manual(labels=xlabs,values=c('red','blue','purple'), name = "Type") +
  ylab("\n Mean Value") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=14),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14)) +
  ylim(0,1) +
  scale_x_continuous(" \n",breaks=c(0.9,1.4,1.9,2.4,2.9,3.4,3.9,4.4,4.9,5.4),labels=rev(c("Digital Literacy*",
                                                                                          "Republican",
                                                                                          "Race (White)",
                                                                                          "Proportion of News Diet\nthat is Unreliable*",
                                                                                          "Income",
                                                                                          "Gender",
                                                                                          "Education",
                                                                                          "Democrat",
                                                                                          "Reliability Score\nof News Diet",
                                                                                          "Age")),limits=c(0.5,6.0)) +
  coord_flip()


ggsave("./Figures/fig_2.png", width=11)





#Figure 3: Histogram of NewsGuard Scores - Data Cannot be shared

#NG_Rating <- read.csv('./Data/NewsGuard_Ratings.csv')

#png(file="./Figures/fig_3.png",
#    width=450, height=450)
#hist(NG_Rating$Score,col='grey',main=NULL,xlab='NewsGuard Score',ylab='Number of News Domains')
#dev.off()




######### Run ITT Models and Covariate-Adjusted Complier Average Causal Effects (CACE) models using both the stronger and weaker compliance checks:

### Test Hypotheses 1, 2, and 3:
#(H1) We test whether in-feed source reliability labels shift downstream news and information consumption from unreliable sources known for publishing misleading or false content to more reliable sources
#(H2) We test whether it increases trust in mainstream media and reliable sources (H2), and  
#(H3) mitigate phenomena associated with democratic dysfunction (affective polarization and political cynicism). 

#Read in survey data with digital trace data:
Pulse_data <- read_csv('./Data/Clean_NewsGuard_Digital_Trace_Data.csv',
                       col_types = cols(
                         .default= col_character(),
                         BLM_info_Index_w2 = col_double(),
                         Covid_info_Index_w2 = col_double(),
                         BLM_Misinfo_Index_w2 = col_double(),
                         Covid_Misinfo_Index_w2 = col_double(),
                         Trust_Media_w2 = col_double(),
                         SMP4310_w2 = col_double(),
                         SMP4310 = col_double(),
                         Treated = col_double(),
                         Complied=col_double(),
                         Total_DL =  col_double(),
                         Total_Science_Misinfo  =  col_double(),
                         Social_Media_Use =  col_double(),
                         income_score =  col_double(),
                         abs_part =  col_double(),
                         aff_pol_w2 = col_double(),
                         aff_pol_w1 = col_double(),
                         mean_cons =  col_double(),
                         Average_domain_NewsG_Score_post= col_double(),
                         Average_domain_NewsG_Score= col_double(),
                         Prop_Unreliable_NewsG_Score_post= col_double(),
                         Prop_Unreliable_NewsG_Score= col_double(),
                         Prop_Reliable_NewsG_Score_post = col_double(),
                         Prop_Reliable_NewsG_Score = col_double(),
                         Count_Unreliable_NewsG_Score_post = col_double(),
                         Count_Unreliable_NewsG_Score = col_double(),
                         Count_Reliable_NewsG_Score_post = col_double(),
                         Count_Reliable_NewsG_Score = col_double(),
                         Prop_Unreliable_NewsG_Score_dv = col_double(),
                         Average_domain_NewsG_Score_dv = col_double(),
                         Prop_Reliable_NewsG_Score_dv = col_double(),
                         Count_Unreliable_NewsG_Score_dv = col_double(),
                         Count_Reliable_NewsG_Score_dv = col_double(),
                         Average_domain_NewsG_Score_post_dur= col_double(),
                         Average_domain_NewsG_Score_dur= col_double(),
                         Prop_Unreliable_NewsG_Score_post_dur= col_double(),
                         Prop_Unreliable_NewsG_Score_dur= col_double(),
                         Prop_Reliable_NewsG_Score_post_dur = col_double(),
                         Prop_Reliable_NewsG_Score_dur = col_double(),
                         Count_Unreliable_NewsG_Score_post_dur = col_double(),
                         Count_Unreliable_NewsG_Score_dur = col_double(),
                         Count_Reliable_NewsG_Score_post_dur = col_double(),
                         Count_Reliable_NewsG_Score_dur = col_double(),
                         Prop_Unreliable_NewsG_Score_dv_dur = col_double(),
                         Average_domain_NewsG_Score_dv_dur = col_double(),
                         Prop_Reliable_NewsG_Score_dv_dur = col_double(),
                         Count_Unreliable_NewsG_Score_dv_dur = col_double(),
                         Count_Reliable_NewsG_Score_dv_dur = col_double(),
                         Prop_Unreliable_NewsG_Score_dv_SM_ref = col_double(),
                         Average_domain_NewsG_Score_dv_SM_ref = col_double(),
                         Prop_Reliable_NewsG_Score_dv_SM_ref = col_double(),
                         Count_Unreliable_NewsG_Score_dv_SM_ref = col_double(),
                         Count_Reliable_NewsG_Score_dv_SM_ref = col_double(),
                         Average_domain_NewsG_Score_post_SM_ref= col_double(),
                         Average_domain_NewsG_Score_SM_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_post_SM_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_SM_ref= col_double(),
                         Prop_Reliable_NewsG_Score_post_SM_ref = col_double(),
                         Prop_Reliable_NewsG_Score_SM_ref = col_double(),
                         Count_Unreliable_NewsG_Score_post_SM_ref = col_double(),
                         Count_Unreliable_NewsG_Score_SM_ref = col_double(),
                         Count_Reliable_NewsG_Score_post_SM_ref = col_double(),
                         Count_Reliable_NewsG_Score_SM_ref = col_double(),
                         Prop_Unreliable_NewsG_Score_dv_All_ref = col_double(),
                         Average_domain_NewsG_Score_dv_All_ref = col_double(),
                         Prop_Reliable_NewsG_Score_dv_All_ref = col_double(),
                         Count_Unreliable_NewsG_Score_dv_All_ref = col_double(),
                         Count_Reliable_NewsG_Score_dv_All_ref = col_double(),
                         Average_domain_NewsG_Score_post_All_ref= col_double(),
                         Average_domain_NewsG_Score_All_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_post_All_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_All_ref= col_double(),
                         Prop_Reliable_NewsG_Score_post_All_ref = col_double(),
                         Prop_Reliable_NewsG_Score_All_ref = col_double(),
                         Count_Unreliable_NewsG_Score_post_All_ref = col_double(),
                         Count_Unreliable_NewsG_Score_All_ref = col_double(),
                         Count_Reliable_NewsG_Score_post_All_ref = col_double(),
                         Count_Reliable_NewsG_Score_All_ref = col_double(),
                         gender_dummy_fem=col_double(),
                         educ_score=col_double(),
                         Age=col_double(),
                         Age_Sq=col_double(),
                         SMP4326_w2=col_double(),
                         SMP4326=col_double(),
                         Trust_inst_w2=col_double(),
                         Trust_inst_w1=col_double(),
                         Pol_cyn_2=col_double(),
                         Pol_cyn_1=col_double(),
                         Fox_Trust_2=col_double(),
                         Fox_Trust_1=col_double(),
                         CNN_Trust_2=col_double(),
                         CNN_Trust_1=col_double(),
                         ABC_Trust_2=col_double(),
                         ABC_Trust_1=col_double(),
                         NBC_Trust_2=col_double(),
                         NBC_Trust_1=col_double(),
                         CBS_Trust_2=col_double(),
                         CBS_Trust_1=col_double(),
                         party_score=col_double(),
                         race_white=col_double(),
                         ideo_score=col_double(),
                         Trust_Media_w1=col_double(),
                         trust_news=col_double(),
                         trust_news_sm=col_double(),
                         cons_news_n=col_double(),
                         cons_cable=col_double(),
                         cons_print=col_double(),
                         cons_public=col_double(), 
                         cons_talk=col_double(),
                         cons_desk=col_double(),
                         cons_mobile=col_double(),
                         Safari_dummy=col_double(),
                         log_news=col_double(),
                         Safari_dummy=col_double(),
                         IE_dummy=col_double(),
                         Chrome_dummy=col_double(),
                         Firefox_dummy=col_double(),
                         Social_Media_Use=col_double()))



#Read in survey data without digital trace data:

data_frame_1 <- read_csv('./Data/Clean_NewsGuard_Survey_Study.csv',
                         col_types = cols(
                           .default= col_character(),
                           Treated = col_double(),
                           Pulse_Dummy=col_double(),
                           Complied=col_double(),
                           Total_DL =  col_double(),
                           Inverse_DL = col_double(),
                           income_score =  col_double(),
                           Total_Science_Misinfo  =  col_double(),
                           Social_Media_Use =  col_double(),
                           mean_cons =  col_double(),
                           SMP4310=col_double(),
                           SMP4310_w2=col_double(),
                           SMP4326=col_double(),
                           SMP4326_w2=col_double(),
                           Trust_inst_w2=col_double(),
                           Trust_inst_w1=col_double(),
                           BLM_Misinfo_Index_w2=col_double(),
                           Covid_Misinfo_Index_w2=col_double(),
                           BLM_info_Index_w2=col_double(),
                           Covid_info_Index_w2=col_double(),
                           Pol_cyn_2=col_double(),
                           Pol_cyn_1=col_double(),
                           Fox_Trust_2=col_double(),
                           Fox_Trust_1=col_double(),
                           CNN_Trust_2=col_double(),
                           CNN_Trust_1=col_double(),
                           ABC_Trust_2=col_double(),
                           ABC_Trust_1=col_double(),
                           NBC_Trust_2=col_double(),
                           NBC_Trust_1=col_double(),
                           CBS_Trust_2=col_double(),
                           CBS_Trust_1=col_double(),
                           aff_pol_w2=col_double(),
                           aff_pol_w1=col_double(),
                           Trust_Media_w2=col_double(),
                           Treated=col_double(),
                           Trust_Media_w1=col_double(),
                           gender_dummy_fem=col_double(),
                           educ_score=col_double(),
                           Age=col_double(),
                           Age_Sq=col_double(),
                           party_score=col_double(),
                           race_white=col_double(),
                           ideo_score=col_double(),
                           Trust_Media_w1=col_double(),
                           trust_news=col_double(),
                           trust_news_sm=col_double(),
                           cons_news_n=col_double(),
                           cons_cable=col_double(),
                           cons_print=col_double(),
                           cons_public=col_double(), 
                           cons_talk=col_double(),
                           cons_desk=col_double(),
                           cons_mobile=col_double(),
                           Safari_dummy=col_double(),
                           IE_dummy=col_double(),
                           Chrome_dummy=col_double(),
                           Firefox_dummy=col_double(),
                           Social_Media_Use=col_double()
                         ))

#Set order of dataset:
data_frame_1 <- data_frame_1[order(data_frame_1$visa1),]


#Create a function using glmnet lasso that chooses the variables to use:
Lasso <- function(data_for_analysis) {
  set.seed(938)
  lasso_select <- cv.glmnet(x=data.matrix(data_for_analysis[,-1]),
                            y=as.vector(data_for_analysis[,1]),
                            alpha=1)
  
  coef.out <- coef(lasso_select, exact = TRUE)
  indices <- which(coef.out != 0)
  names_of_columns <- c(rownames(coef.out)[indices],colnames(data_for_analysis)[1])
  names_of_columns_3 <- names_of_columns[!names_of_columns %in% "(Intercept)"]
  return(names_of_columns_3)
}

#Clean Data Function:

Clean <- function(data_for_analysis) {
  #Replace Infinite values in data with NA
  data_for_analysis <- do.call(data.frame,                      
                               lapply(data_for_analysis,
                                      function(x) replace(x, is.infinite(x), NA)))
  #Remove NA values:
  data_for_analysis <- na.omit(data_for_analysis)
  
  return(data_for_analysis)
}




################## Behavioral Measures (Covariate-Adjusted Models) ##############################################



#List of possible variables for inclusion:
list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "log_news",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")




#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv','Prop_Unreliable_NewsG_Score'),
                               c('Prop_Reliable_NewsG_Score_dv','Prop_Reliable_NewsG_Score'),
                               c('Count_Unreliable_NewsG_Score_dv','Count_Unreliable_NewsG_Score'),
                               c('Count_Reliable_NewsG_Score_dv','Count_Reliable_NewsG_Score'),
                               c('Average_domain_NewsG_Score_dv','Average_domain_NewsG_Score'),
                               c('Prop_Unreliable_NewsG_Score_post','Prop_Unreliable_NewsG_Score'),
                               c('Prop_Reliable_NewsG_Score_post','Prop_Reliable_NewsG_Score'),
                               c('Count_Unreliable_NewsG_Score_post','Count_Unreliable_NewsG_Score'),
                               c('Count_Reliable_NewsG_Score_post','Count_Reliable_NewsG_Score'),
                               c('Average_domain_NewsG_Score_post','Average_domain_NewsG_Score'))

#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_3.txt',
                   './Tables/Table_4.txt',
                   './Tables/Table_5.txt',
                   './Tables/Table_6.txt',
                   './Tables/Table_7.txt',
                   './Tables/Table_8.txt',
                   './Tables/Table_9.txt',
                   './Tables/Table_10.txt',
                   './Tables/Table_11.txt',
                   './Tables/Table_12.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         "log_news",
                         'Prop_Unreliable_NewsG_Score',
                         'Prop_Reliable_NewsG_Score',
                         'Count_Unreliable_NewsG_Score',
                         'Count_Reliable_NewsG_Score',
                         'Average_domain_NewsG_Score',
                         'compliance_check_1',
                         'Complied',
                         "IE_dummy",
                         "Chrome_dummy",
                         "Firefox_dummy",
                         "Social_Media_Use")

top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         "Log of news viewed",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Treatment',
                         'Treatment',
                         "Internet Explorer Browser",
                         "Chrome Browser",
                         "Firefox Browser",
                         "Social Media use")

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:

for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c(list_variables_to_run_1[[i]],list_possible_covariates)
  
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #names_of_columns
  
  names_of_columns <- c('Treated',names_of_columns)
  
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  data_for_analysis <- Pulse_data[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  #names of the variables:
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}


################## Attitudinal Measures (Covariate-Adjusted Models) ##############################################

list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")




#Create Sets of variables to use in models:
list_variables_to_run_2 = list(c('BLM_Misinfo_Index_w2'),
                               c('BLM_info_Index_w2'),
                               c('Covid_Misinfo_Index_w2'),
                               c('Covid_info_Index_w2'),
                               c('Trust_Media_w2'),
                               c('aff_pol_w2','aff_pol_w1'),
                               c('SMP4326_w2','SMP4326'),
                               c('SMP4310_w2','SMP4310'),
                               c('Trust_inst_w2','Trust_inst_w1'),
                               c('CBS_Trust_2','CBS_Trust_1'),
                               c('ABC_Trust_2','ABC_Trust_1'),
                               c('NBC_Trust_2','NBC_Trust_1'),
                               c('CNN_Trust_2','CNN_Trust_1'),
                               c('Fox_Trust_2','Fox_Trust_1'))

#Create list of Titles:
Titles <- c('Testing Effect of Intervention on Belief in Misinformation about the Black Lives Matter Movement with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Belief in True Information about the Black Lives Matter Movement with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Belief in Misinformation about Covid-19 with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Belief in True Information about Covid-19 with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in Media with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Affective Polarization with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Whether They Believe “Fake News is a Problem” with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Whether They Believe “Fake News is a Problem in the Mainstream Media” with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in Institutions with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in CBS with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in ABC with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in NBC with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in CNN with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in Fox News with Covariate-Adjusted Models (HC2 Robust standard errors)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_13.txt',
                   './Tables/Table_14.txt',
                   './Tables/Table_15.txt',
                   './Tables/Table_16.txt',
                   './Tables/Table_17.txt',
                   './Tables/Table_18.txt',
                   './Tables/Table_19.txt',
                   './Tables/Table_20.txt',
                   './Tables/Table_21.txt',
                   './Tables/Table_22.txt',
                   './Tables/Table_23.txt',
                   './Tables/Table_24.txt',
                   './Tables/Table_25.txt',
                   './Tables/Table_26.txt')

#Create crosswalk for variables and display names:
#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         'SMP4310',
                         'Treatment:Moderator',
                         "IE_dummy",
                         "Chrome_dummy",
                         "Firefox_dummy",
                         "Social_Media_Use",
                         'aff_pol_w1',
                         'SMP4326',
                         'SMP4310',
                         'Trust_inst_w1',
                         'CBS_Trust_1',
                         'ABC_Trust_1',
                         'NBC_Trust_1',
                         'CNN_Trust_1',
                         'Fox_Trust_1')

top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         'Pre-Treatment Value',
                         'Treatment*Moderator',
                         "Internet Explorer Browser",
                         "Chrome Browser",
                         "Firefox Browser",
                         "Social Media use",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value')

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 13-26:

for(i in 1:length(list_variables_to_run_2)){
  list_possible_covariates_for_use <- c(list_variables_to_run_2[[i]],list_possible_covariates)
  
  data_for_analysis <- data_frame_1 %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns)]
  data_for_analysis <- data_frame_1[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_2[[i]][1], " ~ .")
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #CACE Model 1  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_2)]
  data_for_analysis <- data_frame_1[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_2[[i]][1], '~ . - Treated | . - compliance_check_1')
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]
  data_for_analysis <- data_frame_1[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_2[[i]][1], '~  . - Treated | . - Complied')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  #names of the variables:
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}


################## Behavioral Measures (Covariate-Adjusted Models) ##############################################

#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv'),
                               c('Prop_Reliable_NewsG_Score_dv'),
                               c('Count_Unreliable_NewsG_Score_dv'),
                               c('Count_Reliable_NewsG_Score_dv'),
                               c('Average_domain_NewsG_Score_dv'),
                               c('Prop_Unreliable_NewsG_Score_post'),
                               c('Prop_Reliable_NewsG_Score_post'),
                               c('Count_Unreliable_NewsG_Score_post'),
                               c('Count_Reliable_NewsG_Score_post'),
                               c('Average_domain_NewsG_Score_post'))





#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Unadjusted Models (HC2 Robust standard errors) (Before July 1st) ',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Unadjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Unadjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Unadjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Unadjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Unadjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Unadjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Unadjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Unadjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Unadjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_27.txt',
                   './Tables/Table_28.txt',
                   './Tables/Table_29.txt',
                   './Tables/Table_30.txt',
                   './Tables/Table_31.txt',
                   './Tables/Table_32.txt',
                   './Tables/Table_33.txt',
                   './Tables/Table_34.txt',
                   './Tables/Table_35.txt',
                   './Tables/Table_36.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c('Treated',
                         'Prop_Unreliable_NewsG_Score',
                         'Prop_Reliable_NewsG_Score',
                         'Count_Unreliable_NewsG_Score',
                         'Count_Reliable_NewsG_Score',
                         'Average_domain_NewsG_Score')

top_attributes_html <- c('Treatment',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value')


names(top_attributes_html) <- top_attribute_names

#Run For loop to produce Tables 27-36:



for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c('Treated',list_variables_to_run_1[[i]])
  
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  names_of_columns_2 <- c('Treated',list_possible_covariates_for_use,'compliance_check_1')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  
  names_of_columns_3 <- c('Treated',list_possible_covariates_for_use,'Complied')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  
  #names of the variables:
  
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}


################## Attitudinal Measures (Covariate-Unadjusted Models) ##############################################

#Create list of Titles:
Titles <- c('Testing Effect of Intervention on Belief in Misinformation about the Black Lives Matter Movement with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Belief in True Information about the Black Lives Matter Movement with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Belief in Misinformation about Covid-19 with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Belief in True Information about Covid-19 with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in Media with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Affective Polarization with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Whether They Believe “Fake News is a Problem” with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Whether They Believe “Fake News is a Problem in the Mainstream Media” with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in Institutions with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in CBS with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in ABC with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in NBC with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in CNN with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Trust in Fox News with Covariate-Unadjusted Models (HC2 Robust standard errors)')


#Create Sets of variables to use in models:
list_variables_to_run_2 = list(c('BLM_Misinfo_Index_w2'),
                               c('BLM_info_Index_w2'),
                               c('Covid_Misinfo_Index_w2'),
                               c('Covid_info_Index_w2'),
                               c('Trust_Media_w2'),
                               c('aff_pol_w2'),
                               c('SMP4326_w2'),
                               c('SMP4310_w2'),
                               c('Trust_inst_w2'),
                               c('CBS_Trust_2'),
                               c('ABC_Trust_2'),
                               c('NBC_Trust_2'),
                               c('CNN_Trust_2'),
                               c('Fox_Trust_2'))


#Create list of filenames:
Tables_title <-  c('./Tables/Table_37.txt',
                   './Tables/Table_38.txt',
                   './Tables/Table_39.txt',
                   './Tables/Table_40.txt',
                   './Tables/Table_41.txt',
                   './Tables/Table_42.txt',
                   './Tables/Table_43.txt',
                   './Tables/Table_44.txt',
                   './Tables/Table_45.txt',
                   './Tables/Table_46.txt',
                   './Tables/Table_47.txt',
                   './Tables/Table_48.txt',
                   './Tables/Table_49.txt',
                   './Tables/Table_50.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         'aff_pol_w1',
                         'SMP4326',
                         'SMP4310',
                         'Trust_inst_w1',
                         'CBS_Trust_1',
                         'ABC_Trust_1',
                         'NBC_Trust_1',
                         'CNN_Trust_1',
                         'Fox_Trust_1')


top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value')


names(top_attributes_html) <- top_attribute_names

#Run For loop to produce Tables 37-50:

for(i in 1:length(list_variables_to_run_2)){
  list_possible_covariates_for_use <- c('Treated',list_variables_to_run_2[[i]])
  data_for_analysis <- data_frame_1 %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_2[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(ITT_Model)
  
  
  names_of_columns_2 <- c('Treated',list_possible_covariates_for_use,'compliance_check_1')
  
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_2)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_2[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  
  names_of_columns_3 <- c('Treated',list_possible_covariates_for_use,'Complied')
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_2[[i]][1], '~  . - Treated | . - Complied')
  
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  #names of the variables:
  
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}

###########################################   Effect of Moderators on Affective Polarization - Adjusted Covariate Models   ###############################################

list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")




#Create Sets of variables to use in models:
list_variables_to_run_4 <- list(c('aff_pol_w2'))


#Create list of Titles:
Titles <- c('Testing Effect of Intervention Initial level of Affective Polarization Moderator on Effect on Affective Polarization (Covariate-Adjusted)')

#Create list of filenames:
Tables_title <-  c('./Tables/Table_51.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         'SMP4310',
                         'Treatment:Moderator',
                         "IE_dummy",
                         "Chrome_dummy",
                         "Firefox_dummy",
                         "Social_Media_Use")


top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         'Pre-Treatment Value',
                         'Treatment*Moderator',
                         "Internet Explorer Browser",
                         "Chrome Browser",
                         "Firefox Browser",
                         "Social Media use")


names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 53-55:
for(i in 1:length(list_variables_to_run_4)){
  list_possible_covariates_for_use <- c(list_variables_to_run_4[[i]],list_possible_covariates)
  
  data_for_analysis <- data_frame_1 %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #names_of_columns
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  
  
  #Affective Polarization  
  
  names_of_columns_M2 <- c('aff_pol_w1',names_of_columns)
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M2)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "aff_pol_w1")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_AP <- lm_robust(as.formula(f), data = data_for_analysis)
  
  ####MATCHING WITH VARIABLES####
  #Match names_of_columns_3 with actual names
  Variable_Names <- names(lm_adj_MT_AP$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(lm_adj_MT_AP),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Affective Polarization"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}

###########################################   Effect of Moderators on Trust In Media - Adjusted Covariate Models   ###############################################

list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")


#Create Sets of variables to use in models:
list_variables_to_run_4 <- list(c('Trust_Media_w2'))


#Create list of Titles:
Titles <- c('Testing Effect of Intervention Initial level of Trust In Media Moderator on Effect on Trust In Media (Covariate-Adjusted)')

#Create list of filenames:
Tables_title <-  c('./Tables/Table_52.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         'SMP4310',
                         'Treatment:Moderator',
                         "IE_dummy",
                         "Chrome_dummy",
                         "Firefox_dummy",
                         "Social_Media_Use")


top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         'Pre-Treatment Value',
                         'Treatment*Moderator',
                         "Internet Explorer Browser",
                         "Chrome Browser",
                         "Firefox Browser",
                         "Social Media use")

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 53-55:
for(i in 1:length(list_variables_to_run_4)){
  list_possible_covariates_for_use <- c(list_variables_to_run_4[[i]],list_possible_covariates)
  
  data_for_analysis <- data_frame_1 %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #names_of_columns
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  
  
  #Affective Polarization  
  
  names_of_columns_M2 <- c('Trust_Media_w1',names_of_columns)
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M2)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Trust_Media_w1")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_AP <- lm_robust(as.formula(f), data = data_for_analysis)
  
  ####MATCHING WITH VARIABLES####
  #Match names_of_columns_3 with actual names
  Variable_Names <- names(lm_adj_MT_AP$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(lm_adj_MT_AP),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Trust in Media"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}




###########################################   Effect of Moderators on Affective Polarization - Unadjusted-Covariate Models   ###############################################

#Create Sets of variables to use in models:
#Create Sets of variables to use in models:
list_variables_to_run_4 <- list(c('aff_pol_w2'))


#Create list of Titles:
Titles <- c('Testing Effect of Intervention Initial level of Affective Polarization Moderator on Effect on Affective Polarization (Covariate-Adjusted)')

#Create list of filenames:
Tables_title <-  c('./Tables/Table_53.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         'SMP4310',
                         'Treatment:Moderator')


top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         'Pre-Treatment Value',
                         'Treatment*Moderator')

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 53-55:
for(i in 1:length(list_variables_to_run_4)){
  names_of_columns <- c(list_variables_to_run_4[[i]])
  
  
  #names_of_columns
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  
  
  #Affective Polarization  
  
  names_of_columns_M2 <- c('aff_pol_w1',names_of_columns)
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M2)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "aff_pol_w1")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_AP <- lm_robust(as.formula(f), data = data_for_analysis)
  
  ####MATCHING WITH VARIABLES####
  #Match names_of_columns_3 with actual names
  Variable_Names <- names(lm_adj_MT_AP$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(lm_adj_MT_AP),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Affective Polarization"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}

###########################################   Effect of Moderators on Trust In Media - Unadjusted-Covariate Models   ###############################################

#Create Sets of variables to use in models:
#Create Sets of variables to use in models:
list_variables_to_run_4 <- list(c('Trust_Media_w2'))


#Create list of Titles:
Titles <- c('Testing Effect of Intervention Initial level of Trust In Media (Inverse) Moderator on Effect on Trust In Media (Covariate-Adjusted)')

#Create list of filenames:
Tables_title <-  c('./Tables/Table_54.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         'SMP4310',
                         'Treatment:Moderator')


top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         'Pre-Treatment Value',
                         'Treatment*Moderator')

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 53-55:
for(i in 1:length(list_variables_to_run_4)){
  names_of_columns <- c(list_variables_to_run_4[[i]])
  #names_of_columns
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  
  
  #Affective Polarization  
  
  names_of_columns_M2 <- c('Trust_Media_w1',names_of_columns)
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M2)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Trust_Media_w1")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_AP <- lm_robust(as.formula(f), data = data_for_analysis)
  
  ####MATCHING WITH VARIABLES####
  #Match names_of_columns_3 with actual names
  Variable_Names <- names(lm_adj_MT_AP$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(lm_adj_MT_AP),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Trust in Media"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}

#################################### OTHER BEHAVIORAL MEASURES ####################################


################## Behavioral Measures (Covariate-Adjusted Models) ##############################################

#List of possible variables for inclusion:
list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "log_news",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")

#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_dur','Prop_Unreliable_NewsG_Score_dur'),
                               c('Prop_Reliable_NewsG_Score_dv_dur','Prop_Reliable_NewsG_Score_dur'),
                               c('Count_Unreliable_NewsG_Score_dv_dur','Count_Unreliable_NewsG_Score_dur'),
                               c('Count_Reliable_NewsG_Score_dv_dur','Count_Reliable_NewsG_Score_dur'),
                               c('Average_domain_NewsG_Score_dv_dur','Average_domain_NewsG_Score_dur'),
                               c('Prop_Unreliable_NewsG_Score_post_dur','Prop_Unreliable_NewsG_Score_dur'),
                               c('Prop_Reliable_NewsG_Score_post_dur','Prop_Reliable_NewsG_Score_dur'),
                               c('Count_Unreliable_NewsG_Score_post_dur','Count_Unreliable_NewsG_Score_dur'),
                               c('Count_Reliable_NewsG_Score_post_dur','Count_Reliable_NewsG_Score_dur'),
                               c('Average_domain_NewsG_Score_post_dur','Average_domain_NewsG_Score_dur'))

#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_103.txt',
                   './Tables/Table_104.txt',
                   './Tables/Table_105.txt',
                   './Tables/Table_106.txt',
                   './Tables/Table_107.txt',
                   './Tables/Table_108.txt',
                   './Tables/Table_109.txt',
                   './Tables/Table_110.txt',
                   './Tables/Table_111.txt',
                   './Tables/Table_112.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         "log_news",
                         'Prop_Unreliable_NewsG_Score_dur',
                         'Prop_Reliable_NewsG_Score_dur',
                         'Count_Unreliable_NewsG_Score_dur',
                         'Count_Reliable_NewsG_Score_dur',
                         'Average_domain_NewsG_Score_dur',
                         'compliance_check_1',
                         'Complied',
                         "IE_dummy",
                         "Chrome_dummy",
                         "Firefox_dummy",
                         "Social_Media_Use")

top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         "Log of news viewed",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Treatment',
                         'Treatment',
                         "Internet Explorer Browser",
                         "Chrome Browser",
                         "Firefox Browser",
                         "Social Media use")

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:


i=8
for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c(list_variables_to_run_1[[i]],list_possible_covariates)
  
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #names_of_columns
  
  names_of_columns <- c('Treated',names_of_columns)
  
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  data_for_analysis <- Pulse_data[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  #names of the variables:
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}



################## Behavioral Measures (Covariate-Adjusted Models) - Referral ##############################################

#List of possible variables for inclusion:
list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "log_news",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")

#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_All_ref','Prop_Unreliable_NewsG_Score_All_ref'),
                               c('Prop_Reliable_NewsG_Score_dv_All_ref','Prop_Reliable_NewsG_Score_All_ref'),
                               c('Count_Unreliable_NewsG_Score_dv_All_ref','Count_Unreliable_NewsG_Score_All_ref'),
                               c('Count_Reliable_NewsG_Score_dv_All_ref','Count_Reliable_NewsG_Score_All_ref'),
                               c('Average_domain_NewsG_Score_dv_All_ref','Average_domain_NewsG_Score_All_ref'),
                               c('Prop_Unreliable_NewsG_Score_post_All_ref','Prop_Unreliable_NewsG_Score_All_ref'),
                               c('Prop_Reliable_NewsG_Score_post_All_ref','Prop_Reliable_NewsG_Score_All_ref'),
                               c('Count_Unreliable_NewsG_Score_post_All_ref','Count_Unreliable_NewsG_Score_All_ref'),
                               c('Count_Reliable_NewsG_Score_post_All_ref','Count_Reliable_NewsG_Score_All_ref'),
                               c('Average_domain_NewsG_Score_post_All_ref','Average_domain_NewsG_Score_All_ref'))




#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Proportion News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_123.txt',
                   './Tables/Table_124.txt',
                   './Tables/Table_125.txt',
                   './Tables/Table_126.txt',
                   './Tables/Table_127.txt',
                   './Tables/Table_128.txt',
                   './Tables/Table_129.txt',
                   './Tables/Table_130.txt',
                   './Tables/Table_131.txt',
                   './Tables/Table_132.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         "log_news",
                         'Prop_Unreliable_NewsG_Score_All_ref',
                         'Prop_Reliable_NewsG_Score_All_ref',
                         'Count_Unreliable_NewsG_Score_All_ref',
                         'Count_Reliable_NewsG_Score_All_ref',
                         'Average_domain_NewsG_Score_All_ref',
                         'compliance_check_1',
                         'Complied',
                         "IE_dummy",
                         "Chrome_dummy",
                         "Firefox_dummy",
                         "Social_Media_Use")

top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         "Log of news viewed",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Treatment',
                         'Treatment',
                         "Internet Explorer Browser",
                         "Chrome Browser",
                         "Firefox Browser",
                         "Social Media use")

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:



for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c(list_variables_to_run_1[[i]],list_possible_covariates)
  
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #names_of_columns
  
  names_of_columns <- c('Treated',names_of_columns)
  
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  data_for_analysis <- Pulse_data[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  #names of the variables:
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}

##################################### Covariate Unadjusted #########################################################
################## Behavioral Measures (Covariate-Adjusted Models) ##############################################

#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_dur'),
                               c('Prop_Reliable_NewsG_Score_dv_dur'),
                               c('Count_Unreliable_NewsG_Score_dv_dur'),
                               c('Count_Reliable_NewsG_Score_dv_dur'),
                               c('Average_domain_NewsG_Score_dv_dur'),
                               c('Prop_Unreliable_NewsG_Score_post_dur'),
                               c('Prop_Reliable_NewsG_Score_post_dur'),
                               c('Count_Unreliable_NewsG_Score_post_dur'),
                               c('Count_Reliable_NewsG_Score_post_dur'),
                               c('Average_domain_NewsG_Score_post_dur'))

#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_133.txt',
                   './Tables/Table_134.txt',
                   './Tables/Table_135.txt',
                   './Tables/Table_136.txt',
                   './Tables/Table_137.txt',
                   './Tables/Table_138.txt',
                   './Tables/Table_139.txt',
                   './Tables/Table_140.txt',
                   './Tables/Table_141.txt',
                   './Tables/Table_142.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c('Prop_Unreliable_NewsG_Score_dur',
                         'Prop_Reliable_NewsG_Score_dur',
                         'Count_Unreliable_NewsG_Score_dur',
                         'Count_Reliable_NewsG_Score_dur',
                         'Average_domain_NewsG_Score_dur')

top_attributes_html <- c('Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value')

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:



for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c('Treated',list_variables_to_run_1[[i]])
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  names_of_columns_2 <- c('Treated',list_possible_covariates_for_use,'compliance_check_1')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  
  names_of_columns_3 <- c('Treated',list_possible_covariates_for_use,'Complied')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  
  #names of the variables:
  
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}



################## Behavioral Measures (Covariate-Adjusted Models) - Referral ##############################################

#List of possible variables for inclusion:
list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "log_news",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")

#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_All_ref'),
                               c('Prop_Reliable_NewsG_Score_dv_All_ref'),
                               c('Count_Unreliable_NewsG_Score_dv_All_ref'),
                               c('Count_Reliable_NewsG_Score_dv_All_ref'),
                               c('Average_domain_NewsG_Score_dv_All_ref'),
                               c('Prop_Unreliable_NewsG_Score_post_All_ref'),
                               c('Prop_Reliable_NewsG_Score_post_All_ref'),
                               c('Count_Unreliable_NewsG_Score_post_All_ref'),
                               c('Count_Reliable_NewsG_Score_post_All_ref'),
                               c('Average_domain_NewsG_Score_post_All_ref'))




#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_153.txt',
                   './Tables/Table_154.txt',
                   './Tables/Table_155.txt',
                   './Tables/Table_156.txt',
                   './Tables/Table_157.txt',
                   './Tables/Table_158.txt',
                   './Tables/Table_159.txt',
                   './Tables/Table_160.txt',
                   './Tables/Table_161.txt',
                   './Tables/Table_162.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c('Prop_Unreliable_NewsG_Score_All_ref',
                         'Prop_Reliable_NewsG_Score_All_ref',
                         'Count_Unreliable_NewsG_Score_All_ref',
                         'Count_Reliable_NewsG_Score_All_ref',
                         'Average_domain_NewsG_Score_All_ref')

top_attributes_html <- c('Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value')

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:



for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c('Treated',list_variables_to_run_1[[i]])
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  names_of_columns_2 <- c('Treated',list_possible_covariates_for_use,'compliance_check_1')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  
  names_of_columns_3 <- c('Treated',list_possible_covariates_for_use,'Complied')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  
  #names of the variables:
  
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}


#### Power T-Tests
### Test Hypotheses 1, 2, and 3:
#(H1) We test whether in-feed source reliability labels shift downstream news and information consumption from unreliable sources known for publishing misleading or false content to more reliable sources
#(H2) We test whether it increases trust in mainstream media and reliable sources (H2), and  
#(H3) mitigate phenomena associated with democratic dysfunction (affective polarization and political cynicism). 

######## FUNCTIONS:

library(tidyverse)
library(estimatr)
library(glmnet)
library(powerLATE)
library(haven)
source('code/functions.r')

########################################################
### Attitudinal Measures
########################################################


list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")

#Create Sets of variables to use in models:
list_variables_to_run_2 = list(c('aff_pol_w2','aff_pol_w1'),
                               c('SMP4326_w2','SMP4326'),
                               c('SMP4310_w2','SMP4310'),
                               c('Trust_inst_w2','Trust_inst_w1'),
                               c('CBS_Trust_2','CBS_Trust_1'),
                               c('ABC_Trust_2','ABC_Trust_1'),
                               c('NBC_Trust_2','NBC_Trust_1'),
                               c('CNN_Trust_2','CNN_Trust_1'),
                               c('Fox_Trust_2','Fox_Trust_1'))



list_text_models <- c('Affective Polarization',
                      'Whether They Believe “Fake News is a Problem”',
                      'Whether They Believe “Fake News is a Problem in the Mainstream Media”',
                      'Trust in Institutions',
                      'Trust in CBS',
                      'Trust in ABC',
                      'Trust in NBC',
                      'Trust in CNN',
                      'Trust in Fox News')



list_power <- c()

i=1

for(i in 1:length(list_variables_to_run_2)){
  list_possible_covariates_for_use <- c(list_variables_to_run_2[[i]],list_possible_covariates)
  
  data_for_analysis <- data_frame_1 %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns)]
  data_for_analysis <- data_frame_1[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f_ITT <- paste0(list_variables_to_run_2[[i]][1], " ~ .")
  ITT_Model <- lm_robust(as.formula(f_ITT), data = data_for_analysis)
  
  #CACE Model 1  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_2)]
  data_for_analysis <- data_frame_1[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_2[[i]][1], '~ . - Treated | . - compliance_check_1')
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]
  data_for_analysis <- data_frame_1[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_2[[i]][1], '~  . - Treated | . - Complied')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  #names of the variables:
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  trt <- "Treated"
  dv <- list_variables_to_run_2[[i]][1]
  dv_pre <- list_variables_to_run_2[[i]][2]
  D <- "Complied"
  
  x2 <- names_use[!names_use %in% dv]
  
  ## DIM
  svy <- data_for_analysis
  
  (dim <- difference_in_means(formula(paste0(dv, " ~ Treated")), 
                              data = svy))
  
  lin_model <- ITT_Model
  sig <- ifelse(abs(lin_model$statistic[2]) >= 1.96, "STARS!", "")
  lin_cov <- formula(paste0(dv, " ~ ", paste(x2, collapse = " + ")))
  
  ## ITT, with Lin's covariate adjustment
  itt <- list(lin_model, sig, lin_cov)
  compute_proportion_missing_covars(itt)
  ## CACE
  
  x2 <- x2[!x2 %in% 'Treated']
  
  f <- paste0(list_variables_to_run_2[[i]][1],' ~ ',paste(x2, collapse = " + "),' + Complied | ',paste(x2, collapse = " + "), ' + Treated')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_frame_1)
  
  cace <- CACE_Model_2
  
  
  pwr <- power2(dv, dim, itt, cace, covariates = TRUE, cace_mde=TRUE, D = 'Complied', trt = trt,trt_var = "Treated")
  
  #Minimum detectable effect
  d_2 <- as.character(pwr[2,2])
  d_1 <- as.character(pwr[2,1])
  
  list_power <- rbind(list_power,c(d_1,d_2))
  if(is.na(list_power[1,1])){
    list_power <- list_power[-1,]
  }
}

list_variables_to_run_2 = list(c('BLM_Misinfo_Index_w2'),
                               c('BLM_info_Index_w2'),
                               c('Covid_Misinfo_Index_w2'),
                               c('Covid_info_Index_w2'),
                               c('Trust_Media_w2'))

list_text_models <- c(list_text_models,c('Belief in Misinformation about the Black Lives Matter Movement',
                                         'Belief in True Information about the Black Lives Matter Movement',
                                         'Belief in Misinformation about Covid-19',
                                         'Belief in True Information about Covid-19',
                                         'Trust in Media'))

i=1

for(i in 1:length(list_variables_to_run_2)){
  list_possible_covariates_for_use <- c(list_variables_to_run_2[[i]],list_possible_covariates)
  
  data_for_analysis <- data_frame_1 %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns)]
  data_for_analysis <- data_frame_1[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f_ITT <- paste0(list_variables_to_run_2[[i]][1], " ~ .")
  ITT_Model <- lm_robust(as.formula(f_ITT), data = data_for_analysis)
  
  #CACE Model 1  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_2)]
  data_for_analysis <- data_frame_1[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_2[[i]][1], '~ . - Treated | . - compliance_check_1')
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]
  data_for_analysis <- data_frame_1[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_2[[i]][1], '~  . - Treated | . - Complied')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  #names of the variables:
  Variable_Names <- names(ITT_Model$coefficients)[-1]

  trt <- "Treated"
  dv <- list_variables_to_run_2[[i]][1]
  D <- "Complied"
  
  x2 <- names_use[!names_use %in% dv]
  
  ## DIM
  svy <- data_for_analysis
  
  (dim <- difference_in_means(formula(paste0(dv, " ~ Treated")), 
                              data = svy))
  
  lin_model <- ITT_Model
  sig <- ifelse(abs(lin_model$statistic[2]) >= 1.96, "STARS!", "")
  lin_cov <- formula(paste0(dv, " ~ ", paste(x2, collapse = " + ")))
  
  ## ITT, with Lin's covariate adjustment
  itt <- list(lin_model, sig, lin_cov)
  compute_proportion_missing_covars(itt)
  ## CACE
  
  x2 <- x2[!x2 %in% 'Treated']
  
  f <- paste0(list_variables_to_run_2[[i]][1],' ~ ',paste(x2, collapse = " + "),' + Complied | ',paste(x2, collapse = " + "), ' + Treated')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_frame_1)
  
  cace <- CACE_Model_2
  
  
  pwr <- power2(dv, dim, itt, cace, covariates = TRUE, cace_mde=TRUE, D = 'Complied', trt = trt,trt_var = "Treated")
  
  #Minimum detectable effect
  d_2 <- as.character(pwr[2,2])
  d_1 <- as.character(pwr[2,1])
  
  list_power <- rbind(list_power,c(d_1,d_2))
}

column_names <- matrix(list_text_models,ncol=1)

Power_matrix <- cbind(column_names,list_power)




colnames(Power_matrix) <- c('Variable','MDE (ITT)','MDE (CACE)')

#Create Xtable Object:
xt <- xtable(Power_matrix,
             digits=2,
             caption= 'Minimum Detectable Effect of Attitudinal Measures assuming power=0.80 and 95 percent Statistical Significance in both the covariate-adjusted Intent-To-Treat and CACE model using the strongest measure',
             align=c(
               "p{1cm}|","|p{10cm}|",
               "p{1cm}|","p{1cm}|"))

#Name Columns:


write(print(xt,
            include.rownames=FALSE,
            sanitize.colnames.function = identity,
            caption.placement='top'),
      file='./Tables/MDE_Table_1.txt')

#Behavioral Measures:

list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "log_news",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")

list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv','Prop_Unreliable_NewsG_Score'),
                               c('Prop_Reliable_NewsG_Score_dv','Prop_Reliable_NewsG_Score'),
                               c('Count_Unreliable_NewsG_Score_dv','Count_Unreliable_NewsG_Score'),
                               c('Count_Reliable_NewsG_Score_dv','Count_Reliable_NewsG_Score'),
                               c('Average_domain_NewsG_Score_dv','Average_domain_NewsG_Score'),
                               c('Prop_Unreliable_NewsG_Score_post','Prop_Unreliable_NewsG_Score'),
                               c('Prop_Reliable_NewsG_Score_post','Prop_Reliable_NewsG_Score'),
                               c('Count_Unreliable_NewsG_Score_post','Count_Unreliable_NewsG_Score'),
                               c('Count_Reliable_NewsG_Score_post','Count_Reliable_NewsG_Score'),
                               c('Average_domain_NewsG_Score_post','Average_domain_NewsG_Score'))

list_text_models <- c('Proportion of News Diet That is Unreliable',
                      'Proportion of News Diet That is Reliable',
                      'Count of Unreliable News Consumed',
                      'Count of Reliable News Consumed',
                      'Avg. Reliability Score of News Diet',
                      'Proportion of News Diet That is Unreliable',
                      'Proportion of News Diet That is Reliable',
                      'Count of Unreliable News Consumed',
                      'Count of Reliable News Consumed',
                      'Avg. Reliability Score of News Diet')


list_power <- matrix(ncol=2)
for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c(list_variables_to_run_1[[i]],list_possible_covariates)
  
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns)]
  data_for_analysis <- Pulse_data[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f_ITT <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  ITT_Model <- lm_robust(as.formula(f_ITT), data = data_for_analysis)
  
  #CACE Model 1  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  data_for_analysis <- Pulse_data[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  data_for_analysis <- Pulse_data[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  #names of the variables:
  Variable_Names <- names(ITT_Model$coefficients)[-1]

  trt <- "Treated"
  dv <- list_variables_to_run_1[[i]][1]
  dv_pre <- list_variables_to_run_1[[i]][2]
  D <- "Complied"
  
  x2 <- names_use[!names_use %in% dv]
  
  svy <- data_for_analysis
  ## DIM
  (dim <- difference_in_means(formula(paste0(dv, " ~ Treated")), 
                              data = svy))
  
  lin_model <- ITT_Model
  sig <- ifelse(abs(lin_model$statistic[2]) >= 1.96, "STARS!", "")
  lin_cov <- formula(paste0(dv, " ~ ", paste(x2, collapse = " + ")))
  
  ## ITT, with Lin's covariate adjustment
  itt <- list(lin_model, sig, lin_cov)
  compute_proportion_missing_covars(itt)
  ## CACE
  
  x2 <- x2[!x2 %in% 'Treated']
  
  f <- paste0(list_variables_to_run_1[[i]][1],' ~ ',paste(x2, collapse = " + "),' + Complied | ',paste(x2, collapse = " + "), ' + Treated')
  CACE_Model_2 <- iv_robust(as.formula(f), data = Pulse_data)
  
  cace <- CACE_Model_2
  
  
  pwr <- power2(dv, dim, itt, cace, covariates = TRUE, cace_mde=TRUE, D = 'Complied', trt = trt,trt_var = "Treated")
  
  #Minimum detectable effect
  d_2 <- as.character(pwr[2,2])
  d_1 <- as.character(pwr[2,1])
  
  list_power <- rbind(list_power,c(d_1,d_2))
  if(is.na(list_power[1,1])){
    list_power <- list_power[-1,]
  } 
}


column_names <- matrix(list_text_models,ncol=1)


Power_matrix <- cbind(column_names,list_power)

colnames(Power_matrix) <- c('Variable','MDE (ITT)','MDE (CACE)')

#Create Xtable Object:
xt <- xtable(Power_matrix,
             digits=2,
             caption= 'Minimum Detectable Effect of Behavioral Measures assuming power=0.80 and 95 percent Statistical Significance in both the covariate-adjusted Intent-To-Treat and CACE model using the strongest measure',
             align=c(
               "p{1cm}|","|p{10cm}|",
               "p{1cm}|","p{1cm}|"))

write(print(xt,
            include.rownames=FALSE,
            sanitize.colnames.function = identity,
            caption.placement='top'),
      file='./Tables/MDE_Table_2.txt')

#DISTRIBUTION OF AVERAGE PROPORTION OF UNRELIABLE NEWS

### Test Hypotheses 1, 2, and 3:
#(H1) We test whether in-feed source reliability labels shift downstream news and information consumption from unreliable sources known for publishing misleading or false content to more reliable sources
#(H2) We test whether it increases trust in mainstream media and reliable sources (H2), and  
#(H3) mitigate phenomena associated with democratic dysfunction (affective polarization and political cynicism). 

#Read in survey data with digital trace data:
Pulse_data <- read_csv('./Data/Clean_NewsGuard_Digital_Trace_Data.csv',
                       col_types = cols(
                         .default= col_character(),
                         BLM_info_Index_w2 = col_double(),
                         Covid_info_Index_w2 = col_double(),
                         BLM_Misinfo_Index_w2 = col_double(),
                         Covid_Misinfo_Index_w2 = col_double(),
                         Trust_Media_w2 = col_double(),
                         SMP4310_w2 = col_double(),
                         SMP4310 = col_double(),
                         Treated = col_double(),
                         Complied=col_double(),
                         Total_DL =  col_double(),
                         Total_Science_Misinfo  =  col_double(),
                         Social_Media_Use =  col_double(),
                         income_score =  col_double(),
                         abs_part =  col_double(),
                         aff_pol_w2 = col_double(),
                         aff_pol_w1 = col_double(),
                         mean_cons =  col_double(),
                         Average_domain_NewsG_Score_post= col_double(),
                         Average_domain_NewsG_Score= col_double(),
                         Prop_Unreliable_NewsG_Score_post= col_double(),
                         Prop_Unreliable_NewsG_Score= col_double(),
                         Prop_Reliable_NewsG_Score_post = col_double(),
                         Prop_Reliable_NewsG_Score = col_double(),
                         Count_Unreliable_NewsG_Score_post = col_double(),
                         Count_Unreliable_NewsG_Score = col_double(),
                         Count_Reliable_NewsG_Score_post = col_double(),
                         Count_Reliable_NewsG_Score = col_double(),
                         Prop_Unreliable_NewsG_Score_dv = col_double(),
                         Average_domain_NewsG_Score_dv = col_double(),
                         Prop_Reliable_NewsG_Score_dv = col_double(),
                         Count_Unreliable_NewsG_Score_dv = col_double(),
                         Count_Reliable_NewsG_Score_dv = col_double(),
                         Average_domain_NewsG_Score_post_dur= col_double(),
                         Average_domain_NewsG_Score_dur= col_double(),
                         Prop_Unreliable_NewsG_Score_post_dur= col_double(),
                         Prop_Unreliable_NewsG_Score_dur= col_double(),
                         Prop_Reliable_NewsG_Score_post_dur = col_double(),
                         Prop_Reliable_NewsG_Score_dur = col_double(),
                         Count_Unreliable_NewsG_Score_post_dur = col_double(),
                         Count_Unreliable_NewsG_Score_dur = col_double(),
                         Count_Reliable_NewsG_Score_post_dur = col_double(),
                         Count_Reliable_NewsG_Score_dur = col_double(),
                         Prop_Unreliable_NewsG_Score_dv_dur = col_double(),
                         Average_domain_NewsG_Score_dv_dur = col_double(),
                         Prop_Reliable_NewsG_Score_dv_dur = col_double(),
                         Count_Unreliable_NewsG_Score_dv_dur = col_double(),
                         Count_Reliable_NewsG_Score_dv_dur = col_double(),
                         Prop_Unreliable_NewsG_Score_dv_SM_ref = col_double(),
                         Average_domain_NewsG_Score_dv_SM_ref = col_double(),
                         Prop_Reliable_NewsG_Score_dv_SM_ref = col_double(),
                         Count_Unreliable_NewsG_Score_dv_SM_ref = col_double(),
                         Count_Reliable_NewsG_Score_dv_SM_ref = col_double(),
                         Average_domain_NewsG_Score_post_SM_ref= col_double(),
                         Average_domain_NewsG_Score_SM_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_post_SM_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_SM_ref= col_double(),
                         Prop_Reliable_NewsG_Score_post_SM_ref = col_double(),
                         Prop_Reliable_NewsG_Score_SM_ref = col_double(),
                         Count_Unreliable_NewsG_Score_post_SM_ref = col_double(),
                         Count_Unreliable_NewsG_Score_SM_ref = col_double(),
                         Count_Reliable_NewsG_Score_post_SM_ref = col_double(),
                         Count_Reliable_NewsG_Score_SM_ref = col_double(),
                         Prop_Unreliable_NewsG_Score_dv_All_ref = col_double(),
                         Average_domain_NewsG_Score_dv_All_ref = col_double(),
                         Prop_Reliable_NewsG_Score_dv_All_ref = col_double(),
                         Count_Unreliable_NewsG_Score_dv_All_ref = col_double(),
                         Count_Reliable_NewsG_Score_dv_All_ref = col_double(),
                         Average_domain_NewsG_Score_post_All_ref= col_double(),
                         Average_domain_NewsG_Score_All_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_post_All_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_All_ref= col_double(),
                         Prop_Reliable_NewsG_Score_post_All_ref = col_double(),
                         Prop_Reliable_NewsG_Score_All_ref = col_double(),
                         Count_Unreliable_NewsG_Score_post_All_ref = col_double(),
                         Count_Unreliable_NewsG_Score_All_ref = col_double(),
                         Count_Reliable_NewsG_Score_post_All_ref = col_double(),
                         Count_Reliable_NewsG_Score_All_ref = col_double(),
                         gender_dummy_fem=col_double(),
                         educ_score=col_double(),
                         Age=col_double(),
                         Age_Sq=col_double(),
                         SMP4326_w2=col_double(),
                         SMP4326=col_double(),
                         Trust_inst_w2=col_double(),
                         Trust_inst_w1=col_double(),
                         Pol_cyn_2=col_double(),
                         Pol_cyn_1=col_double(),
                         Fox_Trust_2=col_double(),
                         Fox_Trust_1=col_double(),
                         CNN_Trust_2=col_double(),
                         CNN_Trust_1=col_double(),
                         ABC_Trust_2=col_double(),
                         ABC_Trust_1=col_double(),
                         NBC_Trust_2=col_double(),
                         NBC_Trust_1=col_double(),
                         CBS_Trust_2=col_double(),
                         CBS_Trust_1=col_double(),
                         party_score=col_double(),
                         race_white=col_double(),
                         ideo_score=col_double(),
                         Trust_Media_w1=col_double(),
                         trust_news=col_double(),
                         trust_news_sm=col_double(),
                         cons_news_n=col_double(),
                         cons_cable=col_double(),
                         cons_print=col_double(),
                         cons_public=col_double(), 
                         cons_talk=col_double(),
                         cons_desk=col_double(),
                         cons_mobile=col_double(),
                         Safari_dummy=col_double(),
                         log_news=col_double(),
                         Safari_dummy=col_double(),
                         IE_dummy=col_double(),
                         Chrome_dummy=col_double(),
                         Firefox_dummy=col_double(),
                         Social_Media_Use=col_double()))



#Read in survey data without digital trace data:

data_frame_1 <- read_csv('./Data/Clean_NewsGuard_Survey_Study.csv',
                         col_types = cols(
                           .default= col_character(),
                           Treated = col_double(),
                           Pulse_Dummy=col_double(),
                           Complied=col_double(),
                           Total_DL =  col_double(),
                           Inverse_DL = col_double(),
                           income_score =  col_double(),
                           Total_Science_Misinfo  =  col_double(),
                           Social_Media_Use =  col_double(),
                           mean_cons =  col_double(),
                           SMP4310=col_double(),
                           SMP4310_w2=col_double(),
                           SMP4326=col_double(),
                           SMP4326_w2=col_double(),
                           Trust_inst_w2=col_double(),
                           Trust_inst_w1=col_double(),
                           BLM_Misinfo_Index_w2=col_double(),
                           Covid_Misinfo_Index_w2=col_double(),
                           BLM_info_Index_w2=col_double(),
                           Covid_info_Index_w2=col_double(),
                           Pol_cyn_2=col_double(),
                           Pol_cyn_1=col_double(),
                           Fox_Trust_2=col_double(),
                           Fox_Trust_1=col_double(),
                           CNN_Trust_2=col_double(),
                           CNN_Trust_1=col_double(),
                           ABC_Trust_2=col_double(),
                           ABC_Trust_1=col_double(),
                           NBC_Trust_2=col_double(),
                           NBC_Trust_1=col_double(),
                           CBS_Trust_2=col_double(),
                           CBS_Trust_1=col_double(),
                           aff_pol_w2=col_double(),
                           aff_pol_w1=col_double(),
                           Trust_Media_w2=col_double(),
                           Treated=col_double(),
                           Trust_Media_w1=col_double(),
                           gender_dummy_fem=col_double(),
                           educ_score=col_double(),
                           Age=col_double(),
                           Age_Sq=col_double(),
                           party_score=col_double(),
                           race_white=col_double(),
                           ideo_score=col_double(),
                           Trust_Media_w1=col_double(),
                           trust_news=col_double(),
                           trust_news_sm=col_double(),
                           cons_news_n=col_double(),
                           cons_cable=col_double(),
                           cons_print=col_double(),
                           cons_public=col_double(), 
                           cons_talk=col_double(),
                           cons_desk=col_double(),
                           cons_mobile=col_double(),
                           Safari_dummy=col_double(),
                           IE_dummy=col_double(),
                           Chrome_dummy=col_double(),
                           Firefox_dummy=col_double(),
                           Social_Media_Use=col_double()
                         ))

#Set order of dataset:
data_frame_1 <- data_frame_1[order(data_frame_1$visa1),]

################## Behavioral Measures (Covariate-Adjusted Models) ##############################################

#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st) on Different Samples of Respondents by Decile of their Quality of News Consumption Pre-Treatment (Average Reliability Score of News)',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st) on Different Samples of Respondents by Decile of their Quality of News Consumption Pre-Treatment (Average Reliability Score of News)',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st) on Different Samples of Respondents by Decile of their Quality of News Consumption Pre-Treatment (Average Reliability Score of News)',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st) on Different Samples of Respondents by Decile of their Quality of News Consumption Pre-Treatment (Average Reliability Score of News)',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st) on Different Samples of Respondents by Decile of their Quality of News Consumption Pre-Treatment (Average Reliability Score of News)',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st) on Different Samples of Respondents by Decile of their Quality of News Consumption Pre-Treatment (Average Reliability Score of News)',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st) on Different Samples of Respondents by Decile of their Quality of News Consumption Pre-Treatment (Average Reliability Score of News)',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st) on Different Samples of Respondents by Decile of their Quality of News Consumption Pre-Treatment (Average Reliability Score of News)',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st) on Different Samples of Respondents by Decile of their Quality of News Consumption Pre-Treatment (Average Reliability Score of News)',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st) on Different Samples of Respondents by Decile of their Quality of News Consumption Pre-Treatment (Average Reliability Score of News)')

Decile_Names <- c('Bottom 10 Percent',
                  'Bottom 20 Percent',
                  'Bottom 30 Percent',
                  'Bottom 40 Percent',
                  'Bottom 50 Percent',
                  'Bottom 60 Percent',
                  'Bottom 70 Percent',
                  'Bottom 80 Percent',
                  'Bottom 90 Percent')


Table_File <- c('./Tables/Table_Deciles_1.txt',
                './Tables/Table_Deciles_2.txt',
                './Tables/Table_Deciles_3.txt',
                './Tables/Table_Deciles_4.txt',
                './Tables/Table_Deciles_5.txt',
                './Tables/Table_Deciles_6.txt',
                './Tables/Table_Deciles_7.txt',
                './Tables/Table_Deciles_8.txt',
                './Tables/Table_Deciles_9.txt',
                './Tables/Table_Deciles_10.txt')


Deciles <- quantile(Pulse_data$Average_domain_NewsG_Score, prob = c(0.1,0.2,0.3,0.4,0.5,0.60,0.70,0.80,0.90),na.rm=T)
#Deciles <- quantile(Pulse_data$Average_domain_NewsG_Score, prob = c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),na.rm=T)



for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c(list_variables_to_run_1[[i]],list_possible_covariates)
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #names_of_columns
  names_of_columns <- c('Treated',names_of_columns)
  
  new_matr <- matrix(ncol=6)
  #x=1
  #x=2
  for(x in 1:length(Deciles)){
    #Pulse_data_2 <- Pulse_data %>% filter(Prop_Unreliable_NewsG_Score >= Deciles[x])
    Pulse_data_2 <- Pulse_data %>% filter(Average_domain_NewsG_Score <= Deciles[x])
    
    names_use <- names(Pulse_data_2)[(names(Pulse_data_2) %in% names_of_columns)]
    
    data_for_analysis <- Pulse_data_2[, names_use]
    data_for_analysis <- Clean(data_for_analysis)
    
    f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
    
    ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
    
    
    names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
    names_use <- names(Pulse_data_2)[(names(Pulse_data_2) %in% names_of_columns_2)]
    data_for_analysis <- Pulse_data_2[, names_use]
    
    #Clean Data:
    data_for_analysis <- Clean(data_for_analysis)
    
    f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
    
    #CACE Model 1
    CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
    
    CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
    
    # #CACE Model 2 - Passed first and second wave compliance check
    names_of_columns_3 <- c(names_of_columns,'Complied')
    names_use <- names(Pulse_data_2)[(names(Pulse_data_2) %in% names_of_columns_3)]
    data_for_analysis <- Pulse_data_2[, names_use]
    #Clean Data:
    data_for_analysis <- Clean(data_for_analysis)
    f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
    CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
    
    CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
    
    #names of the variables:
    Variable_Names <- names(ITT_Model$coefficients)[-1]
    for(z in 1:length(names(coef(ITT_Model)))){
      if(names(coef(ITT_Model))[z] == 'Treated'){
        ITT_coef = round(coef(ITT_Model)[z],3)
        ITT_SD = round(coef(summary(ITT_Model))[, "Std. Error"][z],3)
        ITT_P = coef(summary(ITT_Model))[, "Pr(>|t|)"][z]
      }
    }
    
    for(z in 1:length(names(coef(CACE_Model_1)))){
      if(names(coef(CACE_Model_1))[z] == 'compliance_check_11'){
        C_1_coef = round(coef(CACE_Model_1)[z],3)
        C_1_SD = round(coef(summary(CACE_Model_1))[, "Std. Error"][z],3)
        C_1_P = coef(summary(CACE_Model_1))[, "Pr(>|t|)"][z]
      }
    }
    
    for(z in 1:length(names(coef(CACE_Model_2)))){
      if(names(coef(CACE_Model_2))[z] == 'Complied'){
        C_2_coef = round(coef(CACE_Model_2)[z],3)
        C_2_SD = round(coef(summary(CACE_Model_2))[, "Std. Error"][z],3)
        C_2_P = coef(summary(CACE_Model_2))[, "Pr(>|t|)"][z]
      }
    }
    
    Stars_I_P = ''
    if(ITT_P < 0.05){
      Stars_I_P = '*'
    }
    if(ITT_P < 0.01){
      Stars_I_P = '**'
    }
    if(ITT_P < 0.001){
      Stars_I_P = '***'
    }
    
    Stars_C_1 = ''
    if(C_1_P < 0.05){
      Stars_C_1 = '*'
    }
    if(C_1_P < 0.01){
      Stars_C_1 = '**'
    }
    if(C_1_P < 0.001){
      Stars_C_1 = '***'
    }
    
    Stars_C_2 = ''
    if(C_2_P < 0.05){
      Stars_C_2 = '*'
    }
    if(C_2_P < 0.01){
      Stars_C_2 = '**'
    }
    if(C_2_P < 0.001){
      Stars_C_2 = '***'
    }
    ITT_1 <- paste0(ITT_coef,' (',ITT_SD,')',Stars_I_P)
    CACE_1 <- paste0(C_1_coef,' (',C_1_SD,')',Stars_C_1)
    CACE_2 <- paste0(C_2_coef,' (',C_2_SD,')',Stars_C_2)
    Prop_F <- as.character(round(Deciles[x],3))
    Num_Obs <- nrow(data_for_analysis)
    new_matr <- rbind(new_matr,c(Decile_Names[x],Prop_F,ITT_1,CACE_1,CACE_2,Num_Obs))
    
  }
  colnames(new_matr) <- c('Decile','Threshold','ITT Model','CACE Model (Weak Compliance Measure)','CACE Model (Strong Compliance Measure)','Sample Size')
  new_matr <- new_matr[-1,]
  
  #Create Xtable Object:
  xt <- xtable(new_matr,
               digits=2,
               caption= Titles[i],
               align=c(
                 "p{1cm}|","|p{3.5cm}|",
                 "p{1.5cm}|","p{3cm}|","p{3cm}|","p{3cm}|","p{2cm}|"))
  
  write(print.xtable(xt,
                     include.rownames=FALSE,
                     sanitize.colnames.function = identity,
                     caption.placement='top',
                     add.to.row = list(list(nrow(new_matr)),"\\hline \\multicolumn{5}{l}{\\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}} \\\\ ")),
        file=Table_File[i])
}

#3.71/mean(Pulse_data_2$Average_domain_NewsG_Score)
#5.86/mean(Pulse_data_2$Average_domain_NewsG_Score)




#Distributions:


ggplot(Pulse_data, aes(x=Average_domain_NewsG_Score)) + 
  geom_density(color="lightblue", fill="lightblue") +
  xlab("\n Average NewsGuard Reliability Score") +
  ylab("Density \n") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  xlim(0,100)

ggsave('./Figures/NewsG_Score_Dist.png',height=12,width=10)


ggplot(Pulse_data, aes(x=Prop_Unreliable_NewsG_Score)) + 
  geom_density(color="lightblue", fill="lightblue") +
  xlab("\n Proportion of News Diet Rated as Unreliable      ") +
  ylab("Density \n") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  xlim(0,1)

ggsave('./Figures/NewsG_Unreliable_Dist.png',height=12,width=10)






#Appendix Figures: Measure Treatment:
library(tidyverse)
#Read in survey data with digital trace data:
Pulse_data <- read_csv('./Data/Clean_NewsGuard_Digital_Trace_Data.csv',
                       col_types = cols(
                         .default= col_character()))

Agg_visit_d <- read_csv('./Data/Total_visit_data.csv',
                       col_types = cols(
                         .default= col_character(),
                         Google_Search_Referals = col_double(),
                         Facebook_Time = col_double(),
                         Twitter_Time = col_double(),
                         NG_Site_visits = col_double()))

Pulse_data <- Pulse_data %>% select(caseid)
Pulse_data <- merge(Pulse_data,Agg_visit_d,by='caseid')

Pulse_data$Google_Search_Referals <- ifelse(is.na(Pulse_data$Google_Search_Referals),0,Pulse_data$Google_Search_Referals)
Pulse_data$Facebook_Time <- ifelse(is.na(Pulse_data$Facebook_Time),0,Pulse_data$Facebook_Time)
Pulse_data$Twitter_Time <- ifelse(is.na(Pulse_data$Twitter_Time),0,Pulse_data$Twitter_Time)
Pulse_data$NG_Site_visits <- ifelse(is.na(Pulse_data$NG_Site_visits),0,Pulse_data$NG_Site_visits)


Pulse_data$Google_Search_Referals <- ifelse(Pulse_data$Google_Search_Referals>500,500,Pulse_data$Google_Search_Referals)


ggplot(Pulse_data, aes(x=Google_Search_Referals)) + 
  geom_density(color="lightblue", fill="lightblue") +
  xlab("\n Count of Visits to a Google Search Result Page (Max=500)") +
  ylab("Density \n") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))

ggsave('./Figures/Google_Search_Dist.png',height=12,width=10)

Pulse_data$Facebook_Time <- ifelse(Pulse_data$Facebook_Time>2000,2000,Pulse_data$Facebook_Time)

test_1 <- Pulse_data %>% filter(Facebook_Time > 600)

nrow(test_1)/902

ggplot(Pulse_data, aes(x=Facebook_Time)) + 
  geom_density(color="lightblue", fill="lightblue") +
  xlab("\n Time Spent on Facebook (in Minutes) (Max=2,000)") +
  ylab("Density \n") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))

ggsave('./Figures/Facebook_Dist.png',height=12,width=10)

Pulse_data$Twitter_Time <- ifelse(Pulse_data$Twitter_Time>500,500,Pulse_data$Twitter_Time)

ggplot(Pulse_data, aes(x=Twitter_Time)) + 
  geom_density(color="lightblue", fill="lightblue") +
  xlab("\n  Time Spent on Twitter (in Minutes) (Max=500)") +
  ylab("Density \n") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))

ggsave('./Figures/Twitter_Dist.png',height=12,width=10)


Pulse_data$NG_Site_visits <- ifelse(Pulse_data$NG_Site_visits>2000,2000,Pulse_data$NG_Site_visits)

ggplot(Pulse_data, aes(x=NG_Site_visits)) + 
  geom_density(color="lightblue", fill="lightblue") +
  xlab("\n Count of Visits to Online News \n Sites Rated by NewsGuard (Max=2,000)") +
  ylab("Density \n") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))

ggsave('./Figures/Newsguard_Visits_Dist.png',height=12,width=10)















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

#Read in survey data with digital trace data:
Pulse_data <- read_csv('./Data/Clean_NewsGuard_Digital_Trace_Data.csv',
                       col_types = cols(
                         .default= col_character(),
                         Treated = col_double(),
                         Complied=col_double(),
                         Total_DL =  col_double(),
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
data_frame_1$dem_dummy <- ifelse(data_frame_1$party_score < 0,1,0)
data_frame_1$rep_dummy <- ifelse(data_frame_1$party_score > 0,1,0)
data_frame_1$Total_DL <- (data_frame_1$Total_DL-min(data_frame_1$Total_DL,na.rm=T))/(max(data_frame_1$Total_DL,na.rm=T)-min(data_frame_1$Total_DL,na.rm=T))
data_frame_1$income_score <- (data_frame_1$income_score-min(data_frame_1$income_score,na.rm=T))/(max(data_frame_1$income_score,na.rm=T)-min(data_frame_1$income_score,na.rm=T))


#Create Democrat and Republican dummy variable and make all variables proportions of maximum and minimum:

Pulse_data$Age <- (Pulse_data$Age-min(Pulse_data$Age))/(max(Pulse_data$Age)-min(Pulse_data$Age))
Pulse_data$gender_dummy_fem <- (Pulse_data$gender_dummy_fem-min(Pulse_data$gender_dummy_fem))/(max(Pulse_data$gender_dummy_fem)-min(Pulse_data$gender_dummy_fem))
Pulse_data$race_white <- (Pulse_data$race_white-min(Pulse_data$race_white))/(max(Pulse_data$race_white)-min(Pulse_data$race_white))
Pulse_data$educ_score <- (Pulse_data$educ_score-min(Pulse_data$educ_score))/(max(Pulse_data$educ_score)-min(Pulse_data$educ_score))
Pulse_data$dem_dummy <- ifelse(Pulse_data$party_score < 0,1,0)
Pulse_data$rep_dummy <- ifelse(Pulse_data$party_score > 0,1,0)
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
set.seed(983)


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
  ylab("\n Mean") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 18),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(0,1) +
  scale_x_continuous(" \n",breaks=c(0.9,1.4,1.9,2.4,2.9,3.4,3.9,4.4),labels=c("Digital Literacy",
                                                                                      "Race (White Dummy)*",
                                                                                      "Income*",
                                                                                      "Democrat (Dummy)",
                                                                                      "Republican (Dummy)*",
                                                                                      "Education",
                                                                                      "Gender (Female Dummy)",
                                                                                      "Age*"),limits=c(0.5,5.0)) +
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
set.seed(983)
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
  ylab("\n Mean") +
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
                                                                                      "Republican\n(Dummy)",
                                                                                      "Race\n(White Dummy)",
                                                                                      "Proportion of News Diet\nthat is Unreliable*",
                                                                                      "Income",
                                                                                      "Gender\n(Female dummy)",
                                                                                      "Education",
                                                                                      "Democrat\n(Dummy)",
                                                                                      "Reliability Score\nof News Diet",
                                                                                      "Age")),limits=c(0.5,6.0)) +
  coord_flip()


ggsave("./Figures/fig_2.png", width=11)





#Figure 3: Histogram of NewsGuard Scores - Data Cannot be shared

#NG_Rating <- read.csv('./Data/NewsGuard_Ratings.csv')

#png(file="./Figures/fig_3.png",
#    width=600, height=350)
#hist(NG_Rating$Score,col='grey')
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
                           Safari_dummy=col_double()
                         ))

#Set order of dataset:
data_frame_1 <- data_frame_1[order(data_frame_1$visa1),]



#Clean Data:

Clean <- function(data_for_analysis) {
  #Replace Infinite values in data with NA
  data_for_analysis <- do.call(data.frame,                      
                               lapply(data_for_analysis,
                                      function(x) replace(x, is.infinite(x), NA)))
  #Remove NA values:
  data_for_analysis <- na.omit(data_for_analysis)
  
  return(data_for_analysis)
}


#Create a function using glmnet lasso that chooses the variables to use:
Lasso <- function(data_for_analysis) {
  y <- as.matrix(data_for_analysis[,1])
  x <- as.matrix(data_for_analysis[,-1])
  
  #Set seed as specified in the pre-registration:
  set.seed(938)
  #k-fold cross-validation for glmnet returns a value for lambda
  fit1 = glmnet(x,y, family="gaussian")
  cvob1 = cv.glmnet(x,y)
  coefficients <- coef(fit1,s=cvob1$lambda.min)
  
  #Dependent variable data:
  data_for_regression = data_for_analysis[,1]
  #Possible independent variables:
  names_of_columns = colnames(data_for_analysis)[1]
  
  #Create list of coefficients that should be included:
  for(i in 2:nrow(coefficients)){
    if(coefficients[i,1] != 0){
      z=i-1
      data_for_regression = cbind(data_for_regression,data_for_analysis[,i])
      names_of_columns <- c(names_of_columns,colnames(data_for_analysis)[i])
    }
  }
  #Create list of covariates that should be included:
  return(names_of_columns)
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
                              "Safari_dummy",
                              "log_news")


#Create Sets of variables to use in models:
var1 <- list('variables'=c('Prop_Unreliable_NewsG_Score_dv',
                                         'Prop_Unreliable_NewsG_Score'))

var2 <- list('variables'=c('Prop_Reliable_NewsG_Score_dv',
                                         'Prop_Reliable_NewsG_Score'))

var3 <- list('variables'=c('Count_Unreliable_NewsG_Score_dv',
                                         'Count_Unreliable_NewsG_Score'))

var4 <- list('variables'=c('Count_Reliable_NewsG_Score_dv',
                                         'Count_Reliable_NewsG_Score'))

var5 <- list('variables'=c('Average_domain_NewsG_Score_dv',
                                         'Average_domain_NewsG_Score'))


var6 <- list('variables'=c('Prop_Unreliable_NewsG_Score_post',
                                         'Prop_Unreliable_NewsG_Score'))

var7 <- list('variables'=c('Prop_Reliable_NewsG_Score_post',
                                         'Prop_Reliable_NewsG_Score'))

var8 <- list('variables'=c('Count_Unreliable_NewsG_Score_post',
                                         'Count_Unreliable_NewsG_Score'))

var9 <- list('variables'=c('Count_Reliable_NewsG_Score_post',
                                         'Count_Reliable_NewsG_Score'))

var10 <- list('variables'=c('Average_domain_NewsG_Score_post',
                                         'Average_domain_NewsG_Score'))

list_variables_to_run <- c('a'=var1,
                           'b'=var2,
                           'c'=var3,
                           'd'=var4,
                           'e'=var5,
                           'f'=var6,
                           'g'=var7,
                           'h'=var8,
                           'i'=var9,
                           'j'=var10)

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
  'Average_domain_NewsG_Score')

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
                         'Pre-Treatment Value')


names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:

for(i in 1:length(list_variables_to_run)){
  print(i)
  list_possible_covariates_for_use <- c(list_variables_to_run[[i]],list_possible_covariates)

  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #names_of_columns
  
  names_of_columns <- c('Treated',names_of_columns)
  
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run[[i]][1], '~ . - compliance_check_1 | . - Treated')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  data_for_analysis <- Pulse_data[, names.use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run[[i]][1], '~  . - Complied | . - Treated')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  #names of the variables:
  Variable_Names <- names(CACE_Model_2$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = c(Variable_Names),
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
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
                              "Safari_dummy")

#Create Sets of variables to use in models:
var11 <- list('variables'=c('BLM_Misinfo_Index_w2'))
var12 <- list('variables'=c('BLM_info_Index_w2'))
var13 <- list('variables'=c('Covid_Misinfo_Index_w2'))
var14 <- list('variables'=c('Covid_info_Index_w2'))
var15 <- list('variables'=c('Trust_Media_w2'))
var16 <- list('variables'=c('aff_pol_w2',
                           'aff_pol_w1'))
var17 <- list('variables'=c('SMP4326_w2',
                           'SMP4326'))
var18 <- list('variables'=c('SMP4310_w2',
                           'SMP4310'))
var19 <- list('variables'=c('Trust_inst_w2',
                           'Trust_inst_w1'))
var20 <- list('variables'=c('CBS_Trust_2',
                            'CBS_Trust_1'))
var21 <- list('variables'=c('ABC_Trust_2',
                           'ABC_Trust_1'))
var22 <- list('variables'=c('NBC_Trust_2',
                           'NBC_Trust_1'))
var23 <- list('variables'=c('CNN_Trust_2',
                            'CNN_Trust_1'))
var24 <- list('variables'=c('Fox_Trust_2',
                            'Fox_Trust_1'))

list_variables_to_run <- c('a'=var11,
                           'b'=var12,
                           'c'=var13,
                           'd'=var14,
                           'e'=var15,
                           'f'=var16,
                           'g'=var17,
                           'h'=var18,
                           'i'=var19,
                           'j'=var20,
                           'g'=var21,
                           'h'=var22,
                           'i'=var23,
                           'j'=var24)

#Create list of Titles:
Titles <- c('Testing Effect of Intervention on Belief in Misinformation about the Black Lives Matter Movement with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Belief in Misinformation about Covid-19 with Covariate-Adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Belief in True Information about the Black Lives Matter Movement with Covariate-Adjusted Models (HC2 Robust standard errors)',
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


#Run For loop to produce Tables 13-26:

for(i in 1:length(list_variables_to_run)){
  print(i)
  list_possible_covariates_for_use <- c(list_variables_to_run[[i]],list_possible_covariates)
  
  data_for_analysis <- data_frame_1 %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns)]
  data_for_analysis <- data_frame_1[, names.use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run[[i]][1], " ~ .")
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #CACE Model 1  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_2)]
  data_for_analysis <- data_frame_1[, names.use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run[[i]][1], '~ . - compliance_check_1 | . - Treated')
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]
  data_for_analysis <- data_frame_1[, names.use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run[[i]][1], '~  . - Complied | . - Treated')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  #names of the variables:
  Variable_Names <- names(CACE_Model_2$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  

  #Write Table
texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
                     include.ci = FALSE,
                     digits=4,
                     omit.coef = '(Intercept)',
                     caption= Titles[i],
                     label = "table",
                     include.rmse = FALSE,
                     custom.coef.names = c(Variable_Names),
                     custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
                     file=Tables_title[i],
       caption.above = TRUE)
}


################## Behavioral Measures (Covariate-Unadjusted Models) ##############################################

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
                              "Safari_dummy",
                              "log_news")

#Create Sets of variables to use in models:
var1 <- list('variables'=c('Prop_Unreliable_NewsG_Score_dv',
                           'Prop_Unreliable_NewsG_Score'))

var2 <- list('variables'=c('Prop_Reliable_NewsG_Score_dv',
                           'Prop_Reliable_NewsG_Score'))

var3 <- list('variables'=c('Count_Unreliable_NewsG_Score_dv',
                           'Count_Unreliable_NewsG_Score'))

var4 <- list('variables'=c('Count_Reliable_NewsG_Score_dv',
                           'Count_Reliable_NewsG_Score'))

var5 <- list('variables'=c('Average_domain_NewsG_Score_dv',
                           'Average_domain_NewsG_Score'))


var6 <- list('variables'=c('Prop_Unreliable_NewsG_Score_post',
                           'Prop_Unreliable_NewsG_Score'))

var7 <- list('variables'=c('Prop_Reliable_NewsG_Score_post',
                           'Prop_Reliable_NewsG_Score'))

var8 <- list('variables'=c('Count_Unreliable_NewsG_Score_post',
                           'Count_Unreliable_NewsG_Score'))

var9 <- list('variables'=c('Count_Reliable_NewsG_Score_post',
                           'Count_Reliable_NewsG_Score'))

var10 <- list('variables'=c('Average_domain_NewsG_Score_post',
                            'Average_domain_NewsG_Score'))

list_variables_to_run <- c('a'=var1,
                           'b'=var2,
                           'c'=var3,
                           'd'=var4,
                           'e'=var5,
                           'f'=var6,
                           'g'=var7,
                           'h'=var8,
                           'i'=var9,
                           'j'=var10)

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
                         'Average_domain_NewsG_Score')

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
                         'Pre-Treatment Value')


names(top_attributes_html) <- top_attribute_names

#Run For loop to produce Tables 27-36:

for(i in 1:length(list_variables_to_run)){
  print(i)
  list_possible_covariates_for_use <- c('Treated',list_variables_to_run[[i]],list_possible_covariates)
  
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  names_of_columns_2 <- c('Treated',list_possible_covariates_for_use,'compliance_check_1')
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run[[i]][1], '~ . - compliance_check_1 | . - Treated')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  
  # #CACE Model 2 - Passed first and second wave compliance check
  
  names_of_columns_3 <- c('Treated',list_possible_covariates_for_use,'Complied')
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run[[i]][1], '~  . - Complied | . - Treated')
  
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  #names of the variables:
  
  Variable_Names <- names(CACE_Model_2$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
                     include.ci = FALSE,
                     digits=4,
                     omit.coef = '(Intercept)',
                     caption= Titles[i],
                     label = "table",
                     include.rmse = FALSE,
                     custom.coef.names = c(Variable_Names),
                     custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
                     file=Tables_title[i],
       caption.above = TRUE)
}


################## Attitudinal Measures (Covariate-Unadjusted Models) ##############################################

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
                              "Safari_dummy")

#Create Sets of variables to use in models:
var11 <- list('variables'=c('BLM_Misinfo_Index_w2'))
var12 <- list('variables'=c('BLM_info_Index_w2'))
var13 <- list('variables'=c('Covid_Misinfo_Index_w2'))
var14 <- list('variables'=c('Covid_info_Index_w2'))
var15 <- list('variables'=c('Trust_Media_w2'))
var16 <- list('variables'=c('aff_pol_w2',
                            'aff_pol_w1'))
var17 <- list('variables'=c('SMP4326_w2',
                            'SMP4326'))
var18 <- list('variables'=c('SMP4310_w2',
                            'SMP4310'))
var19 <- list('variables'=c('Trust_inst_w2',
                            'Trust_inst_w1'))
var20 <- list('variables'=c('CBS_Trust_2',
                            'CBS_Trust_1'))
var21 <- list('variables'=c('ABC_Trust_2',
                            'ABC_Trust_1'))
var22 <- list('variables'=c('NBC_Trust_2',
                            'NBC_Trust_1'))
var23 <- list('variables'=c('CNN_Trust_2',
                            'CNN_Trust_1'))
var24 <- list('variables'=c('Fox_Trust_2',
                            'Fox_Trust_1'))

list_variables_to_run <- c('a'=var11,
                           'b'=var12,
                           'c'=var13,
                           'd'=var14,
                           'e'=var15,
                           'f'=var16,
                           'g'=var17,
                           'h'=var18,
                           'i'=var19,
                           'j'=var20,
                           'g'=var21,
                           'h'=var22,
                           'i'=var23,
                           'j'=var24)

#Create list of Titles:
Titles <- c('Testing Effect of Intervention on Belief in Misinformation about the Black Lives Matter Movement with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Belief in Misinformation about Covid-19 with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention on Belief in True Information about the Black Lives Matter Movement with Covariate-Unadjusted Models (HC2 Robust standard errors)',
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

for(i in 1:length(list_variables_to_run)){
  print(i)
  list_possible_covariates_for_use <- c('Treated',list_variables_to_run[[i]],list_possible_covariates)
  
  data_for_analysis <- data_frame_1 %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(ITT_Model)
  
  
  names_of_columns_2 <- c('Treated',list_possible_covariates_for_use,'compliance_check_1')
  
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_2)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run[[i]][1], '~ . - compliance_check_1 | . - Treated')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  
  # #CACE Model 2 - Passed first and second wave compliance check
  
  names_of_columns_3 <- c('Treated',list_possible_covariates_for_use,'Complied')
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run[[i]][1], '~  . - Complied | . - Treated')
  
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  #names of the variables:
  
  Variable_Names <- names(CACE_Model_2$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  

  #Write Table
texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
                     include.ci = FALSE,
                     digits=4,
                     omit.coef = '(Intercept)',
                     caption= Titles[i],
                     label = "table",
                     include.rmse = FALSE,
                     custom.coef.names = c(Variable_Names),
                     custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
                     file=Tables_title[i],
       caption.above = TRUE)
}



###########################################   Effect of Moderators on Behavioral Measures - Adjusted Covariate Models   ###############################################



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
                              'log_news')


#Create Sets of variables to use in models:
var1 <- list('variables'=c('Prop_Unreliable_NewsG_Score_post',
                           'Prop_Unreliable_NewsG_Score'))

var2 <- list('variables'=c('Average_domain_NewsG_Score_post',
                            'Average_domain_NewsG_Score'))

list_variables_to_run <- c('a'=var1,
                           'b'=var2)

#Create list of Titles:
Titles <- c('Testing Effect of Intervention Using Different Moderators on Average Reliability Score of Online News Viewed (Covariate-Adjusted)',
            'Testing Effect of Intervention Using Different Moderators on Proportion of Unreliable Online News Viewed (Covariate-Adjusted)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_51.txt',
                   './Tables/Table_52.txt')

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
                         'Average_domain_NewsG_Score',
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
                         "Log of news viewed",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Treatment*Moderator')

names(top_attributes_html) <- top_attribute_names

list_moderators <- c('Age',
                     'Total_DL',
                     'Total_Science_Misinfo',
                     'Social_Media_Use',
                     'mean_cons',
                     'abs_part',
                     'Prop_Unreliable_NewsG_Score')



#Run For loop to produce Tables 51-52:

for(i in 1:length(list_variables_to_run)){
list_possible_covariates_for_use <- c(list_variables_to_run[[i]],list_possible_covariates)
data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
#Clean Data:
data_for_analysis <- Clean(data_for_analysis)
#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns <- Lasso(data_for_analysis)

#(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
#$p$-values from two-tailed $t$-tests.

names_of_columns <- c('Treated',names_of_columns)

#DL
names_of_columns_M1 <- c(names_of_columns,'Total_DL')

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]

data_for_analysis <- Pulse_data[, names.use]

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_DL")] <- "Moderator"

f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")

lm_adj_MT_DL <- lm_robust(as.formula(f), data = data_for_analysis)
summary(lm_adj_MT_DL)

Variable_Names <- names(lm_adj_MT_DL$coefficients)[-1]
Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)

#Scientific Misinformation

names_of_columns_M1 <- c(names_of_columns,'Total_Science_Misinfo')

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]

data_for_analysis <- Pulse_data[, names.use]

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)


colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_Science_Misinfo")] <- "Moderator"

f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")

lm_adj_MT_S_Misinfo <- lm_robust(as.formula(f), data = data_for_analysis)
summary(lm_adj_MT_S_Misinfo)

#Age
names_of_columns_M1 <- c(names_of_columns,'Age')

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]

data_for_analysis <- Pulse_data[, names.use]

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

colnames(data_for_analysis)[which(names(data_for_analysis) == "Age")] <- "Moderator"

f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")

lm_adj_MT_Age <- lm_robust(as.formula(f), data = data_for_analysis)
summary(lm_adj_MT_Age)


#SM Use
names_of_columns_M1 <- c(names_of_columns,'Social_Media_Use')

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]

data_for_analysis <- Pulse_data[, names.use]

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

colnames(data_for_analysis)[which(names(data_for_analysis) == "Social_Media_Use")] <- "Moderator"

f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")

lm_adj_MT_SM_use <- lm_robust(as.formula(f), data = data_for_analysis)
summary(lm_adj_MT_SM_use)

#Moderator:  mean_cons

names_of_columns_M2 <- c('mean_cons',names_of_columns)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]

data_for_analysis <- Pulse_data[, names.use]

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

colnames(data_for_analysis)[which(names(data_for_analysis) == "mean_cons")] <- "Moderator"

f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + . - cons_desk - cons_mobile")

lm_adj_MT_consump <- lm_robust(as.formula(f), data = data_for_analysis)
summary(lm_adj_MT_consump)

#Pulse (absolute partisanship)

names_of_columns_M2 <- c('abs_part',names_of_columns)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]

data_for_analysis <- Pulse_data[, names.use]

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)


colnames(data_for_analysis)[which(names(data_for_analysis) == "abs_part")] <- "Moderator"

f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")

lm_adj_MT_Part <- lm_robust(as.formula(f), data = data_for_analysis)
summary(lm_adj_MT_Part)

#Pulse (Pre-unreliable)


names_of_columns_M2 <- c('Prop_Unreliable_NewsG_Score',names_of_columns)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]

data_for_analysis <- Pulse_data[, names.use]

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

colnames(data_for_analysis)[which(names(data_for_analysis) == "Prop_Unreliable_NewsG_Score")] <- "Moderator"

f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")

lm_adj_MT_Prop_Unrel <- lm_robust(as.formula(f), data = data_for_analysis)
summary(lm_adj_MT_Prop_Unrel)


#Write Table:

texreg(list(lm_adj_MT_DL,lm_adj_MT_S_Misinfo,lm_adj_MT_Age,lm_adj_MT_SM_use,lm_adj_MT_consump,lm_adj_MT_Part,lm_adj_MT_Prop_Unrel),
       include.ci = FALSE,
       digits=4,
       omit.coef = '(Intercept)',
       caption= Titles[i],
       label = "table",
       include.rmse = FALSE,
       custom.coef.names = Variable_Names,
       custom.model.names= c("Dig. Lit.", "Sci. Misinf.","Age","S.M. Use","News Consump.","Partisan. of News Diet","Unreliable News Consumed"),
       file=Tables_title[i],
       caption.above = TRUE)
}


###########################################   Effect of Moderators on Attitudinal Measures - Adjusted Covariate Models   ###############################################

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
                              "Safari_dummy")

#Create Sets of variables to use in models:
var1 <- list('variables'=c('BLM_Misinfo_Index_w2'))
var2 <- list('variables'=c('Covid_Misinfo_Index_w2'))
var3 <- list('variables'=c('Trust_Media_w2'))
var4 <- list('variables'=c('SMP4310_w2',
                            'SMP4310'))

list_variables_to_run <- c('a'=var1,
                           'b'=var2,
                           'c'=var3,
                           'd'=var4)

#Create list of Titles:
Titles <- c('Testing Effect of Intervention Using Different Moderators on Belief in BLM Misinformation (Covariate-Adjusted)',
            'Testing Effect of Intervention Using Different Moderators on Belief in Covid Misinformation (Covariate-Adjusted)',
            'Testing Effect of Intervention Using Different Moderators on Trust in Media (Covariate-Adjusted)',
            'Testing Effect of Intervention Using Different Moderators on Belief that \'\'fake news is a problem in the mainstream media\'\' (Covariate-Adjusted)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_53.txt',
                   './Tables/Table_54.txt',
                   './Tables/Table_55.txt',
                   './Tables/Table_56.txt')

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

list_moderators <- c('Age',
                     'Total_DL',
                     'Total_Science_Misinfo',
                     'Social_Media_Use',
                     'mean_cons',
                     'abs_part',
                     'Prop_Unreliable_NewsG_Score')

#Run For loop to produce Tables 53-56:

for(i in 1:length(list_variables_to_run)){
  list_possible_covariates_for_use <- c(list_variables_to_run[[i]],list_possible_covariates)
  
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
  
  #DL
  names_of_columns_M1 <- c(names_of_columns,'Total_DL')
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_DL")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_DL <- lm_robust(as.formula(f), data = data_for_analysis)
  
  Variable_Names <- names(lm_adj_MT_DL$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Scientific Misinformation
  
  names_of_columns_M1 <- c(names_of_columns,'Total_Science_Misinfo')
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_Science_Misinfo")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_S_Misinfo <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Age
  names_of_columns_M1 <- c(names_of_columns,'Age')
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Age")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Age <- lm_robust(as.formula(f), data = data_for_analysis)
  
  
  #SM Use
  names_of_columns_M1 <- c(names_of_columns,'Social_Media_Use')
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Social_Media_Use")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_SM_use <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Moderator:  mean_cons
  
  names_of_columns_M2 <- c('mean_cons',names_of_columns)
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M2)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "mean_cons")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + . - cons_desk - cons_mobile")
  
  lm_adj_MT_consump <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Pulse (absolute partisanship)
  
  names_of_columns_M2 <- c('abs_part',names_of_columns)
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "abs_part")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Part <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Pulse (Pre-unreliable)
  
  
  names_of_columns_M2 <- c('Prop_Unreliable_NewsG_Score',names_of_columns)
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Prop_Unreliable_NewsG_Score")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Prop_Unrel <- lm_robust(as.formula(f), data = data_for_analysis)
  
  ####MATCHING WITH VARIABLES####
  #Match names_of_columns_3 with actual names
  
  
  #Write Table
texreg(list(lm_adj_MT_DL,lm_adj_MT_S_Misinfo,lm_adj_MT_Age,lm_adj_MT_SM_use,lm_adj_MT_consump,lm_adj_MT_Part,lm_adj_MT_Prop_Unrel),
                     include.ci = FALSE,
                     digits=4,
                     omit.coef = '(Intercept)',
                     caption= Titles[i],
                     label = "table",
                     include.rmse = FALSE,
                     custom.coef.names = Variable_Names,
                     custom.model.names= c("Dig. Lit.", "Sci. Misinf.","Age","S.M. Use","News Consump.","Partisan. of News Diet","Unreliable News Consumed"),
                     file=Tables_title[i],
       caption.above = TRUE)
}




###########################################   Effect of Moderators on Behavioral Measures - Unadjusted Covariate Models   ###############################################


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
                              'log_news')


#Create Sets of variables to use in models:
var1 <- list('variables'=c('Prop_Unreliable_NewsG_Score_post',
                           'Prop_Unreliable_NewsG_Score'))

var2 <- list('variables'=c('Average_domain_NewsG_Score_post',
                           'Average_domain_NewsG_Score'))

list_variables_to_run <- c('a'=var1,
                           'b'=var2)

#Create list of Titles:
Titles <- c('Testing Effect of Intervention Using Different Moderators on Average Reliability Score of Online News Viewed (Covariate-Unadjusted)',
            'Testing Effect of Intervention Using Different Moderators on Proportion of Unreliable Online News Viewed (Covariate-Unadjusted)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_57.txt',
                   './Tables/Table_58.txt')


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
                         'Average_domain_NewsG_Score',
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
                         "Log of news viewed",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Treatment*Moderator')

names(top_attributes_html) <- top_attribute_names

list_moderators <- c('Age',
                     'Total_DL',
                     'Total_Science_Misinfo',
                     'Social_Media_Use',
                     'mean_cons',
                     'abs_part',
                     'Prop_Unreliable_NewsG_Score')



#Run For loop to produce Tables 57-58:

for(i in 1:length(list_variables_to_run)){
  #names_of_columns
  names_of_columns <- c(list_variables_to_run[[i]],list_possible_covariates)
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  
  #DL
  names_of_columns_M1 <- c(names_of_columns,'Total_DL')
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_DL")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_DL <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_DL)
  
  Variable_Names <- names(lm_adj_MT_DL$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Scientific Misinformation
  
  names_of_columns_M1 <- c(names_of_columns,'Total_Science_Misinfo')
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_Science_Misinfo")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_S_Misinfo <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_S_Misinfo)
  
  #Age
  names_of_columns_M1 <- c(names_of_columns,'Age')
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Age")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Age <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_Age)
  
  
  #SM Use
  names_of_columns_M1 <- c(names_of_columns,'Social_Media_Use')
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Social_Media_Use")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_SM_use <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_SM_use)
  
  #Moderator:  mean_cons
  
  names_of_columns_M2 <- c('mean_cons',names_of_columns)
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "mean_cons")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + . - cons_desk - cons_mobile")
  
  lm_adj_MT_consump <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_consump)
  
  #Pulse (absolute partisanship)
  
  names_of_columns_M2 <- c('abs_part',names_of_columns)
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "abs_part")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Part <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_Part)
  
  #Pulse (Pre-unreliable)
  
  
  names_of_columns_M2 <- c('Prop_Unreliable_NewsG_Score',names_of_columns)
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Prop_Unreliable_NewsG_Score")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Prop_Unrel <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_Prop_Unrel)
  
  #Write Table
texreg(list(lm_adj_MT_DL,lm_adj_MT_S_Misinfo,lm_adj_MT_Age,lm_adj_MT_SM_use,lm_adj_MT_consump,lm_adj_MT_Part,lm_adj_MT_Prop_Unrel),
                     include.ci = FALSE,
                     digits=4,
                     omit.coef = '(Intercept)',
                     caption= Titles[i],
                     label = "table",
                     include.rmse = FALSE,
                     custom.coef.names = Variable_Names,
                     custom.model.names= c("Dig. Lit.", "Sci. Misinf.","Age","S.M. Use","News Consump.","Partisan. of News Diet","Unreliable News Consumed"),
                     file=Tables_title[i],
       caption.above = TRUE)
}


###########################################   Effect of Moderators on Attitudinal Measures - Unadjusted Covariate Models   ###############################################


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
                              "Safari_dummy")

#Create Sets of variables to use in models:
var1 <- list('variables'=c('BLM_Misinfo_Index_w2'))
var2 <- list('variables'=c('Covid_Misinfo_Index_w2'))
var3 <- list('variables'=c('Trust_Media_w2'))
var4 <- list('variables'=c('SMP4310_w2',
                           'SMP4310'))

list_variables_to_run <- c('a'=var1,
                           'b'=var2,
                           'c'=var3,
                           'd'=var4)

#Create list of Titles:
Titles <- c('Testing Effect of Intervention Using Different Moderators on Belief in BLM Misinformation  (Covariate-Unadjusted)',
            'Testing Effect of Intervention Using Different Moderators on Belief in Covid Misinformation  (Covariate-Unadjusted)',
            'Testing Effect of Intervention Using Different Moderators on Trust in Media  (Covariate-Unadjusted)',
            'Testing Effect of Intervention Using Different Moderators on Belief that \'\'fake news is a problem in the mainstream media\'\'  (Covariate-Unadjusted)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_59.txt',
                   './Tables/Table_60.txt',
                   './Tables/Table_61.txt',
                   './Tables/Table_62.txt')


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

list_moderators <- c('Age',
                     'Total_DL',
                     'Total_Science_Misinfo',
                     'Social_Media_Use',
                     'mean_cons',
                     'abs_part',
                     'Prop_Unreliable_NewsG_Score')

#Run For loop to produce Tables 59-62:

for(i in 1:length(list_variables_to_run)){
  #names_of_columns
  names_of_columns <- c(list_variables_to_run[[i]],list_possible_covariates)
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  
  #DL
  names_of_columns_M1 <- c(names_of_columns,'Total_DL')
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_DL")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_DL <- lm_robust(as.formula(f), data = data_for_analysis)
  
  Variable_Names <- names(lm_adj_MT_DL$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Scientific Misinformation
  
  names_of_columns_M1 <- c(names_of_columns,'Total_Science_Misinfo')
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_Science_Misinfo")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_S_Misinfo <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Age
  names_of_columns_M1 <- c(names_of_columns,'Age')
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Age")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Age <- lm_robust(as.formula(f), data = data_for_analysis)
  
  
  #SM Use
  names_of_columns_M1 <- c(names_of_columns,'Social_Media_Use')
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Social_Media_Use")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_SM_use <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Moderator:  mean_cons
  
  names_of_columns_M2 <- c('mean_cons',names_of_columns)
  
  names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M2)]
  
  data_for_analysis <- data_frame_1[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "mean_cons")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + . - cons_desk - cons_mobile")

  lm_adj_MT_consump <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Pulse (absolute partisanship)
  
  names_of_columns_M2 <- c('abs_part',names_of_columns)
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "abs_part")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Part <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Pulse (Pre-unreliable)
  
  
  names_of_columns_M2 <- c('Prop_Unreliable_NewsG_Score',names_of_columns)
  
  names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names.use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Prop_Unreliable_NewsG_Score")] <- "Moderator"
  
  f <- paste0(list_variables_to_run[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Prop_Unrel <- lm_robust(as.formula(f), data = data_for_analysis)
  

  #Write Table
texreg(list(lm_adj_MT_DL,lm_adj_MT_S_Misinfo,lm_adj_MT_Age,lm_adj_MT_SM_use,lm_adj_MT_consump,lm_adj_MT_Part,lm_adj_MT_Prop_Unrel),
                     include.ci = FALSE,
                     digits=4,
                     omit.coef = '(Intercept)',
                     caption= Titles[i],
                     label = "table",
                     include.rmse = FALSE,
                     custom.coef.names = Variable_Names,
                     custom.model.names= c("Dig. Lit.", "Sci. Misinf.","Age","S.M. Use","News Consump.","Partisan. of News Diet","Unreliable News Consumed"),
                     file=Tables_title[i],
       caption.above = TRUE)
}





#Figure 4: This figure presents the proportion of the average daily proportion of unreliable news viewed of the treatment and control groups across this study

#Read in Wave 1 Data
Wave_1_Data <- read.csv('./Data/NYUU0017_w1_OUTPUT.csv')

#Read in Wave 2 Data
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
dataframe_1 <- merge(Wave_1_Data,Wave_2_Data,by.x='caseid',by.y='caseid_w1')
dataframe_1$plugin_treat <- as.character(dataframe_1$plugin_treat)
dataframe_1$Treated <- ifelse(dataframe_1$plugin_treat == 'Offered plug-in',1,0)

crosswalk_1 <- dataframe_1 %>% select(caseid,visa1)


dataframe_1 <- dataframe_1 %>% select(caseid,endtime,endtime_w2,Treated)

dataframe_1$endtime <- as.character(dataframe_1$endtime)
dataframe_1$endtime_w2 <- as.character(dataframe_1$endtime_w2)

dataframe_1$endtime <- substr(dataframe_1$endtime,1,10)
dataframe_1$endtime_w2 <- substr(dataframe_1$endtime_w2,1,10)

#Read in Wave 1 Data
Pulse_Data <- read.csv('./Data/pulsebyday.csv')

colnames(Pulse_Data)

Pulse_Data <- merge(Pulse_Data,dataframe_1,by='caseid')

Pulse_Data$endtime <- as.Date(Pulse_Data$endtime)
Pulse_Data$endtime_w2 <- as.Date(Pulse_Data$endtime_w2)
Pulse_Data$date <- as.Date(Pulse_Data$date)

Pulse_Data$Around_Treatment <- -1*(Pulse_Data$endtime-Pulse_Data$date)
Pulse_Data$After_Treatment <- (Pulse_Data$date-as.Date('2020-07-01'))
Pulse_Data$Day_of_Study <- (Pulse_Data$date-as.Date('2020-05-16'))
Pulse_Data$Around_Treatment <- as.numeric(Pulse_Data$Around_Treatment)
Pulse_Data$After_Treatment <- as.numeric(Pulse_Data$After_Treatment)
Pulse_Data$Dummy_Before <- ifelse(Pulse_Data$Around_Treatment < 1,'Pre-Treatment','Treatment')
Pulse_Data$Dummy_Before <- ifelse(Pulse_Data$After_Treatment > -1,'After Treatment',Pulse_Data$Dummy_Before)

Pulse_Data$date_2 <- as.Date(as.character(Pulse_Data$date),origin = '1970-01-01')  

Analysis_D <- Pulse_Data %>% group_by(date_2,Treated) %>% mutate(AVG_Score = mean(Average_domain_NewsG_Score_day,na.rm=T))
Analysis_D <- Analysis_D %>% group_by(date_2,Treated) %>% mutate(AVG_Unrel_Prop = mean(Prop_Unreliable_NewsG_Score_day,na.rm=T))


Count_D <- Analysis_D %>% group_by(date_2,Treated,Dummy_Before) %>% count()

Analysis_D <- merge(Analysis_D,Count_D,by=c('date_2','Treated','Dummy_Before'))

Analysis_D <- Analysis_D %>% select(date_2,AVG_Score,AVG_Unrel_Prop,Treated,n)

Analysis_D <- unique(Analysis_D)

Analysis_D <- Analysis_D %>% filter(n > 99)

unique(Analysis_D$date_2)

Analysis_D$Group <- ifelse(Analysis_D$Treated == 1,'Treatment','Control')

#Figure 4: This figure presents the proportion of the average daily proportion of unreliable news viewed of the treatment and control groups across this study

ggplot(Analysis_D, aes(x=date_2,y=AVG_Unrel_Prop,colour=Group)) +
  annotate("rect", xmin=as.Date('2020-06-02'), xmax=as.Date('2020-06-09'), ymin=-0.01, ymax=0.06,
           alpha = .5,fill='gray46') +
  stat_smooth(method='loess',se=T,span=0.5, method.args = list(degree = 1),aes(fill=Group)) +
  scale_color_manual(values=c("blue","red"),
                     aesthetics = c("colour", "fill")) +
  geom_vline(xintercept=as.Date('2020-06-02'), linetype="dashed", 
             color = "black", size=1, show.legend=T) +
  geom_vline(xintercept=as.Date('2020-06-09'), linetype="dashed", 
             color = "black", size=1,) +
  geom_vline(xintercept=as.Date('2020-07-01'), linetype="dashed", 
             color = "black", size=1, show.legend=T) +
  ylab('Proportion of News Viewed that is Unreliable\n') +
  coord_cartesian(xlim=c(as.Date('2020-06-03'),as.Date('2020-07-14')),ylim=c(0.03,0.045)) +
  xlab('\n        First Wave                                    Treatment Ends                           ') +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=14),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))

ggsave('./Figures/fig_4.png',width=11)






























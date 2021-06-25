


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



########################################################################################################################################################

#Load Libraries:
library(tidyverse)
library(dplyr)
library(AER)
library(glmnet)
library(estimatr)
library(huxtable)
library(magrittr)
library(texreg)
library(ivpack)



#Read in compliance check data (compliance_check_1,compliance_check_2,Complied):
Compliance_Check <- read.csv('./Data/Compliance_Check_Data.csv')

#Only use compliance checks during the period in which the first wave survey was out
First_Check <- Compliance_Check %>% filter(timestamp < 1592500000000)
#Use the last compliance check for each respondent in this time frame:
First_Check <- First_Check %>% group_by(user_id) %>% top_n(1, timestamp)
#Set column names:
colnames(First_Check) <- c('user_id','compliance_check_1','timestamp_1')




mean(First_Check$compliance_check_1,na.rm=T)


#Only use compliance checks during the period in which the second wave survey was out
Second_Check <- Compliance_Check %>% filter(timestamp > 1592500000000)
#Use the last compliance check for each respondent in this time frame:
Second_Check <- Second_Check %>% group_by(user_id) %>% top_n(1, timestamp)
#Set column names:
colnames(Second_Check) <- c('user_id','compliance_check_2','timestamp_2')

#Merge first check with the existing data-frame
df_1 <- merge(Attrition_Data,First_Check,by.x='visa1',by.y='user_id',all.x=T)
df_new <- df_1 %>% select(visa1,Treated,compliance_check_1,timestamp_1,Both_Waves)
#Merge the second check with the existing data-frame
df_new_1 <- merge(df_new,Second_Check,by.x='visa1',by.y='user_id',all.x=T)


df_new_1$Complied <- ifelse(df_new_1$compliance_check_1 == 1 & df_new_1$compliance_check_1 == 1,1,0)



Attrit_Data <- df_new_1 %>% filter(Both_Waves == 0)
Stay_Data <- df_new_1 %>% filter(Both_Waves == 1)


mean(Attrit_Data$compliance_check_1,na.rm=T)
mean(Stay_Data$compliance_check_1,na.rm=T)

















Attrition_Group <- Attrition_Data_2 %>% filter(Both_Waves == 0)

Stayed_Group <- Attrition_Data_2 %>% filter(Both_Waves == 1)

mean(Attrition_Group$compliance_check_1,na.rm=T)

mean(Stayed_Group$compliance_check_1,na.rm=T)



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















########################################################################################################################################################
### Title: News Credibility Labels Improve News Diets, Reduce Misperceptions, and Increase Media Trust
### Authors: Kevin Aslett, Andrew Guess, Jonathan Nagler, Richard Bonneaua, and Joshua A. Tucker
### Purpose of code: Produce figures and tables of analyses that are displayed in the main test of the paper

##### Data In:
# (1) ./Data/Clean_NewsGuard_Digital_Trace_Data.csv
# (2) ./Data/Clean_NewsGuard_Survey_Study.csv

##### Data Out: NONE

########################################################################################################################################################

#Load Libraries:
library(tidyverse)
library(dplyr)
library(AER)
library(glmnet)
library(estimatr)
library(huxtable)
library(magrittr)
library(texreg)
library(ivpack)

#Read in survey data without digital trace data:

data_frame_1 <- read_csv('./Data/Clean_NewsGuard_Survey_Study.csv',
                         col_types = cols(
                           .default= col_character(),
                           Treated = col_double(),
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
                           Complied=col_double(),
                           compliance_check_1=col_double(),
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

          
          
data_frame_1$NG_feeling <- factor(data_frame_1$SMP3005_w2, levels = c('I liked it a lot',
                                                                      'I liked it a little',
                                                                      'I neither liked it nor disliked it',
                                                                      'I disliked it a little',
                                                                      'I disliked it a lot'))

data_frame_1$NG_feeling <- as.numeric(data_frame_1$NG_feeling)
Compl_Data <- data_frame_1 %>% filter(compliance_check_1 == 1)


  
table(Compl_Data$NG_feeling)



#Figure 3a in Paper:

CP_data <- Pulse_data %>% filter(Treated == 0)
CP_data <- CP_data %>% ungroup() %>% select(caseid,Prop_Unreliable_NewsG_Score,Prop_Unreliable_NewsG_Score_post)
CP_data <- na.omit(CP_data)
data_T_Pre <- cbind(CP_data$caseid,CP_data$Prop_Unreliable_NewsG_Score,'Pre-Treatment')
data_T_Post <- cbind(CP_data$caseid,CP_data$Prop_Unreliable_NewsG_Score_post,'Post-Treatment')
colnames(data_T_Pre) <- c('caseid','Pre','Group')
colnames(data_T_Post) <- c('caseid','Post','Group')
scatter_data_Control <- merge(data_T_Pre,data_T_Post,by='caseid')
scatter_data_Control$Pre <- as.character(scatter_data_Control$Pre)
scatter_data_Control$Pre <- as.numeric(scatter_data_Control$Pre)
scatter_data_Control$Post <- as.character(scatter_data_Control$Post)
scatter_data_Control$Post <- as.numeric(scatter_data_Control$Post)
scatter_data_Control$L_G <- 'Control'

TP_data <- Pulse_data %>% filter(Treated == 1)
TP_data <- TP_data %>% ungroup() %>% select(caseid,Prop_Unreliable_NewsG_Score,Prop_Unreliable_NewsG_Score_post)
TP_data <- na.omit(TP_data)
data_T_Pre <- cbind(TP_data$caseid,TP_data$Prop_Unreliable_NewsG_Score,'Pre-Treatment')
data_T_Post <- cbind(TP_data$caseid,TP_data$Prop_Unreliable_NewsG_Score_post,'Post-Treatment')
colnames(data_T_Pre) <- c('caseid','Prop_unrel','Group')
colnames(data_T_Post) <- c('caseid','Prop_unrel','Group')
data_T <- rbind(data_T_Pre,data_T_Post)
colnames(data_T_Pre) <- c('caseid','Pre','Group')
colnames(data_T_Post) <- c('caseid','Post','Group')
scatter_data_Treat <- merge(data_T_Pre,data_T_Post,by='caseid')
scatter_data_Treat$Pre <- as.character(scatter_data_Treat$Pre)
scatter_data_Treat$Pre <- as.numeric(scatter_data_Treat$Pre)
scatter_data_Treat$Post <- as.character(scatter_data_Treat$Post)
scatter_data_Treat$Post <- as.numeric(scatter_data_Treat$Post)
scatter_data_Treat$L_G <- 'Treatment'
scatter_full <- rbind(scatter_data_Control,scatter_data_Treat)

tmp_1 <-  data.frame(x=c(0.25,1,1), y=c(0.0,0.0,0.75))
tmp_2 <-  data.frame(x=c(0,0,0.75), y=c(0.25,1,1))

ggplot() +
  theme_classic() +
  coord_cartesian(ylim=c(0,1),xlim=c(0,1)) +
  geom_smooth(data=scatter_full, aes(x=Pre,y=Post,colour=L_G), method=lm, se=T) +
  geom_smooth(data=scatter_full, aes(x=Pre,y=Post,colour=L_G), method=lm, se=T) +
  geom_point(data=scatter_full, aes(x=Pre,y=Post,color=L_G,shape=L_G),alpha=0.9,size=3) +
  scale_colour_manual(values=c("blue","red")) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=18),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=18),
        plot.title = element_text(size = 18),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  xlab('\nProportion of online news diet from unreliable news sources (Pre-Treatment)') +
  ylab('Proportion of online news diet from unreliable news sources (July 1st-July 13th)\n') +
  guides(shape=guide_legend(title="Group"),color=guide_legend(title="Group"))

ggsave('./Figures/Scatter_Full.png',width =9,height=9)


#Figure 3b in Paper:
CP_data <- Pulse_data %>% filter(Treated == 0)
CP_data <- CP_data %>% ungroup() %>% select(caseid,Average_domain_NewsG_Score,Average_domain_NewsG_Score_post)
CP_data <- na.omit(CP_data)
data_T_Pre <- cbind(CP_data$caseid,CP_data$Average_domain_NewsG_Score,'Pre-Treatment')
data_T_Post <- cbind(CP_data$caseid,CP_data$Average_domain_NewsG_Score_post,'Post-Treatment')
colnames(data_T_Pre) <- c('caseid','Score','Group')
colnames(data_T_Post) <- c('caseid','Score','Group')
data_T <- rbind(data_T_Pre,data_T_Post)
data_T <- as.data.frame(data_T)
data_T$Score <- as.character(data_T$Score)
data_T$Score <- as.numeric(data_T$Score)
data_T$Group <- factor(data_T$Group,levels=c('Pre-Treatment',
                                             'Post-Treatment'))
colnames(data_T_Pre) <- c('caseid','Pre','Group')
colnames(data_T_Post) <- c('caseid','Post','Group')
scatter_data_Control <- merge(data_T_Pre,data_T_Post,by='caseid')
scatter_data_Control$Pre <- as.character(scatter_data_Control$Pre)
scatter_data_Control$Pre <- as.numeric(scatter_data_Control$Pre)
scatter_data_Control$Post <- as.character(scatter_data_Control$Post)
scatter_data_Control$Post <- as.numeric(scatter_data_Control$Post)
scatter_data_Control$L_G <- 'Control'


TP_data <- Pulse_data %>% filter(Treated == 1)
TP_data <- TP_data %>% ungroup() %>% select(caseid,Average_domain_NewsG_Score,Average_domain_NewsG_Score_post)
TP_data <- na.omit(TP_data)
data_T_Pre <- cbind(TP_data$caseid,TP_data$Average_domain_NewsG_Score,'Pre-Treatment')
data_T_Post <- cbind(TP_data$caseid,TP_data$Average_domain_NewsG_Score_post,'Post-Treatment')
colnames(data_T_Pre) <- c('caseid','Prop_unrel','Group')
colnames(data_T_Post) <- c('caseid','Prop_unrel','Group')
data_T <- rbind(data_T_Pre,data_T_Post)
colnames(data_T_Pre) <- c('caseid','Pre','Group')
colnames(data_T_Post) <- c('caseid','Post','Group')
scatter_data_Treat <- merge(data_T_Pre,data_T_Post,by='caseid')
scatter_data_Treat$Pre <- as.character(scatter_data_Treat$Pre)
scatter_data_Treat$Pre <- as.numeric(scatter_data_Treat$Pre)
scatter_data_Treat$Post <- as.character(scatter_data_Treat$Post)
scatter_data_Treat$Post <- as.numeric(scatter_data_Treat$Post)
scatter_data_Treat$L_G <- 'Treatment'
scatter_full <- rbind(scatter_data_Control,scatter_data_Treat)
tmp_1 <-  data.frame(x=c(25,100,100), y=c(0.0,0.0,75))
tmp_2 <-  data.frame(x=c(0,0,75), y=c(25,100,100))

ggplot() +
  theme_classic() +
  coord_cartesian(ylim=c(0,100),xlim=c(0,100)) +
  geom_point(data=scatter_full, aes(x=Pre,y=Post,color=L_G,shape=L_G),alpha=0.9,size=3) +
  geom_smooth(data=scatter_full, aes(x=Pre,y=Post,colour=L_G), method=lm, se=T) +
  geom_smooth(data=scatter_full, aes(x=Pre,y=Post,colour=L_G), method=lm, se=T) +
  scale_colour_manual(values=c("blue","red")) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=18),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=18),
        plot.title = element_text(size = 18),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  xlab('\nAverage Reliability Score of Online News Diet (Pre-Treatment)') +
  ylab('Average Reliability Score of Online News Diet (July 1st-July 13th)\n') +
  guides(shape=guide_legend(title="Group"),color=guide_legend(title="Group"))

ggsave('./Figures/Scatter_Full_2.png',width =9,height=9)













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






#ggplot(Pulse_data, aes(x=Prop_Reliable_NewsG_Score)) + 
#  geom_density(color="lightblue", fill="lightblue") +
#  xlab("\n Proportion of News Diet Rated as Reliable") +
#  ylab("Density \n") +
#  theme_classic() +
#  theme(axis.title.x = element_text(size=16),
#        axis.text.x  = element_text(size=16),
#        axis.title.y = element_text(size=16),
#        axis.text.y  = element_text(size=16),
#        plot.title = element_text(size = 16),
#        legend.title = element_text(size=16),
#        legend.text = element_text(size=14)) +
#  xlim(0,1)

#ggsave('./Figures/NewsG_Reliable_Dist.png',height=12,width=10)




#Count_NA <- Pulse_data %>% filter(is.na(Prop_Unreliable_NewsG_Score)) %>% count()
#Count_NA <- as.numeric(Count_NA)

#Count_Prop_Zero <- Pulse_data %>% filter(Prop_Unreliable_NewsG_Score == 0) %>% count()
#Count_Prop_Zero <- as.numeric(Count_Prop_Zero)

#(Count_NA+Count_Prop_Zero)/nrow(Pulse_data)


#Count_Prop_Zero <- Pulse_data %>% filter(Prop_Unreliable_NewsG_Score >= 0.05) %>% count()
#Count_Prop_Zero <- as.numeric(Count_Prop_Zero)

#(Count_Prop_Zero)/nrow(Pulse_data)


#Count_All <- Pulse_data %>% filter(!is.na(Average_domain_NewsG_Score)) %>% count()
#Count_All <- as.numeric(Count_All)

#Count_Avg_NewsG <- Pulse_data %>% filter(Average_domain_NewsG_Score >= 60) %>% count()
#Count_Avg_NewsG <- as.numeric(Count_Avg_NewsG)

#Count_Avg_NewsG/Count_All

#mean(Pulse_data$Average_domain_NewsG_Score,na.rm=T)



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
                              'log_news',
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")


#Create Sets of variables to use in models:
list_variables_to_run_3 = list(c('Prop_Unreliable_NewsG_Score_dv','Prop_Unreliable_NewsG_Score'),
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
Titles <- c('Testing the Effect of Moderators on the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of Moderators on the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of Moderators on the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of Moderators on the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of Moderators on the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of Moderators on the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of Moderators on the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of Moderators on the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of Moderators on the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of Moderators on the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_55.txt',
                   './Tables/Table_56.txt',
                   './Tables/Table_57.txt',
                   './Tables/Table_58.txt',
                   './Tables/Table_59.txt',
                   './Tables/Table_60.txt',
                   './Tables/Table_61.txt',
                   './Tables/Table_62.txt',
                   './Tables/Table_63.txt',
                   './Tables/Table_64.txt')

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
                         'Treatment:Moderator',
                         "IE_dummy",
                         "Chrome_dummy",
                         "Firefox_dummy",
                         "Social_Media_Use",
                         'Prop_Reliable_NewsG_Score',
                         'Count_Unreliable_NewsG_Score',
                         'Count_Reliable_NewsG_Score')

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
                         'Treatment*Moderator',
                         "Internet Explorer Browser",
                         "Chrome Browser",
                         "Firefox Browser",
                         "Social Media use",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value')

names(top_attributes_html) <- top_attribute_names

#Run For loop to produce Tables 51-52:
i=1
for(i in 1:length(list_variables_to_run_3)){
  list_possible_covariates_for_use <- c(list_variables_to_run_3[[i]],list_possible_covariates)
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
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_DL")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_DL <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_DL)
  
  Variable_Names <- names(lm_adj_MT_DL$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Scientific Misinformation
  
  names_of_columns_M1 <- c(names_of_columns,'Total_Science_Misinfo')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_Science_Misinfo")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_S_Misinfo <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_S_Misinfo)
  
  #SM Use
  names_of_columns_M1 <- c(names_of_columns,'Social_Media_Use')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Social_Media_Use")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_SM_use <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_SM_use)
  
  #Moderator:  mean_cons
  
  names_of_columns_M2 <- c('mean_cons',names_of_columns)
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "mean_cons")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + . - cons_desk - cons_mobile")
  
  lm_adj_MT_consump <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_consump)
  
  #Pulse (absolute partisanship)
  
  names_of_columns_M2 <- c('abs_part',names_of_columns)
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "abs_part")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Part <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_Part)
  
  #Pulse (Pre-unreliable)
  
  
  names_of_columns_M2 <- c('Prop_Unreliable_NewsG_Score',names_of_columns)
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Prop_Unreliable_NewsG_Score")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Prop_Unrel <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_Prop_Unrel)
  
  
  #Write Table:
  
  texreg(list(lm_adj_MT_DL,lm_adj_MT_S_Misinfo,lm_adj_MT_SM_use,lm_adj_MT_consump,lm_adj_MT_Part,lm_adj_MT_Prop_Unrel),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Dig. Lit.", "Sci. Misinf.","S.M. Use","News Consump.","Partisan. of News Diet","Unreliable News Consumed"),
         file=Tables_title[i],
         float.pos = "!htbp",
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
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")



#Create Sets of variables to use in models:
#Create Sets of variables to use in models:

list_variables_to_run_4 = list(c('BLM_Misinfo_Index_w2'),
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
Titles <- c('Testing Effect of Intervention Using Different Moderators on Belief in Misinformation about the Black Lives Matter Movement with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Belief in True Information about the Black Lives Matter Movement with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Belief in Misinformation about Covid-19 with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Belief in True Information about Covid-19 with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in Media with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Affective Polarization with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Whether They Believe “Fake News is a Problem” with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Whether They Believe “Fake News is a Problem in the Mainstream Media” with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in Institutions with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in CBS with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in ABC with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in NBC with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in CNN with Covariate-adjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in Fox News with Covariate-adjusted Models (HC2 Robust standard errors)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_65.txt',
                   './Tables/Table_66.txt',
                   './Tables/Table_67.txt',
                   './Tables/Table_68.txt',
                   './Tables/Table_69.txt',
                   './Tables/Table_70.txt',
                   './Tables/Table_71.txt',
                   './Tables/Table_72.txt',
                   './Tables/Table_73.txt',
                   './Tables/Table_74.txt',
                   './Tables/Table_75.txt',
                   './Tables/Table_76.txt',
                   './Tables/Table_77.txt',
                   './Tables/Table_78.txt')

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
  
  #DL
  names_of_columns_M1 <- c(names_of_columns,'Total_DL')
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_DL")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_DL <- lm_robust(as.formula(f), data = data_for_analysis)
  
  Variable_Names <- names(lm_adj_MT_DL$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Scientific Misinformation
  
  names_of_columns_M1 <- c(names_of_columns,'Total_Science_Misinfo')
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_Science_Misinfo")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_S_Misinfo <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #SM Use
  names_of_columns_M1 <- c(names_of_columns,'Social_Media_Use')
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Social_Media_Use")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_SM_use <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Moderator:  mean_cons
  
  names_of_columns_M2 <- c('mean_cons',names_of_columns)
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M2)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "mean_cons")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + . - cons_desk - cons_mobile")
  
  lm_adj_MT_consump <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Pulse (absolute partisanship)
  
  names_of_columns_M2 <- c('abs_part',names_of_columns)
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "abs_part")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Part <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Pulse (Pre-unreliable)
  
  names_of_columns_M2 <- c('Prop_Unreliable_NewsG_Score',names_of_columns)
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Prop_Unreliable_NewsG_Score")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Prop_Unrel <- lm_robust(as.formula(f), data = data_for_analysis)
  
  ####MATCHING WITH VARIABLES####
  #Match names_of_columns_3 with actual names
  
  
  #Write Table
  texreg(list(lm_adj_MT_DL,lm_adj_MT_S_Misinfo,lm_adj_MT_SM_use,lm_adj_MT_consump,lm_adj_MT_Part,lm_adj_MT_Prop_Unrel),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Dig. Lit.", "Sci. Misinf.","S.M. Use","News Consump.","Partisan. of News Diet","Unreliable News Consumed"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}




###########################################   Effect of Moderators on Behavioral Measures - Unadjusted Covariate Models   ###############################################

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
                         'Prop_Reliable_NewsG_Score',
                         'Count_Unreliable_NewsG_Score',
                         'Count_Reliable_NewsG_Score',
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
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Treatment*Moderator')

names(top_attributes_html) <- top_attribute_names

#Create Sets of variables to use in models:
list_variables_to_run_3 = list(c('Prop_Unreliable_NewsG_Score_dv'),
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
Titles <- c('Testing the Effect of Moderators on the Intervention on Proportion of News Diet That is Unreliable with Covariate-Unadjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of Moderators on the Intervention on Proportion of News Diet That is Reliable with Covariate-Unadjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of Moderators on the Intervention on Count of Unreliable News Consumed with Covariate-Unadjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of Moderators on the Intervention on Count of Reliable News Consumed with Covariate-Unadjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of Moderators on the Intervention on Reliability Score of News Diet with Covariate-Unadjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of Moderators on the Intervention on Proportion of News Diet That is Unreliable with Covariate-Unadjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of Moderators on the Intervention on Proportion of News Diet That is Reliable with Covariate-Unadjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of Moderators on the Intervention on Count of Unreliable News Consumed with Covariate-Unadjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of Moderators on the Intervention on Count of Reliable News Consumed with Covariate-Unadjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of Moderators on the Intervention on Reliability Score of News Diet with Covariate-Unadjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_79.txt',
                   './Tables/Table_80.txt',
                   './Tables/Table_81.txt',
                   './Tables/Table_82.txt',
                   './Tables/Table_83.txt',
                   './Tables/Table_84.txt',
                   './Tables/Table_85.txt',
                   './Tables/Table_86.txt',
                   './Tables/Table_87.txt',
                   './Tables/Table_88.txt')

#Run For loop to produce Tables 57-58:
for(i in 1:length(list_variables_to_run_3)){
  #names_of_columns
  names_of_columns <- c(list_variables_to_run_3[[i]])
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  
  #DL
  names_of_columns_M1 <- c(names_of_columns,'Total_DL')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_DL")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_DL <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_DL)
  
  Variable_Names <- names(lm_adj_MT_DL$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Scientific Misinformation
  
  names_of_columns_M1 <- c(names_of_columns,'Total_Science_Misinfo')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_Science_Misinfo")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_S_Misinfo <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_S_Misinfo)
  
  #SM Use
  names_of_columns_M1 <- c(names_of_columns,'Social_Media_Use')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M1)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Social_Media_Use")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_SM_use <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_SM_use)
  
  #Moderator:  mean_cons
  
  names_of_columns_M2 <- c('mean_cons',names_of_columns)
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "mean_cons")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + . - cons_desk - cons_mobile")
  
  lm_adj_MT_consump <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_consump)
  
  #Pulse (absolute partisanship)
  
  names_of_columns_M2 <- c('abs_part',names_of_columns)
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "abs_part")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Part <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_Part)
  
  #Pulse (Pre-unreliable)
  
  
  names_of_columns_M2 <- c('Prop_Unreliable_NewsG_Score',names_of_columns)
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Prop_Unreliable_NewsG_Score")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_3[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Prop_Unrel <- lm_robust(as.formula(f), data = data_for_analysis)
  summary(lm_adj_MT_Prop_Unrel)
  
  #Write Table
  texreg(list(lm_adj_MT_DL,lm_adj_MT_S_Misinfo,lm_adj_MT_SM_use,lm_adj_MT_consump,lm_adj_MT_Part,lm_adj_MT_Prop_Unrel),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Dig. Lit.", "Sci. Misinf.","S.M. Use","News Consump.","Partisan. of News Diet","Unreliable News Consumed"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}


###########################################   Effect of Moderators on Attitudinal Measures - Unadjusted Covariate Models   ###############################################


list_variables_to_run_4 = list(c('BLM_Misinfo_Index_w2'),
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

#Create list of Titles:
Titles <- c('Testing Effect of Intervention Using Different Moderators on Belief in Misinformation about the Black Lives Matter Movement with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Belief in True Information about the Black Lives Matter Movement with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Belief in Misinformation about Covid-19 with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Belief in True Information about Covid-19 with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in Media with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Affective Polarization with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Whether They Believe “Fake News is a Problem” with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Whether They Believe “Fake News is a Problem in the Mainstream Media” with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in Institutions with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in CBS with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in ABC with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in NBC with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in CNN with Covariate-Unadjusted Models (HC2 Robust standard errors)',
            'Testing Effect of Intervention Using Different Moderators on Trust in Fox News with Covariate-Unadjusted Models (HC2 Robust standard errors)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_89.txt',
                   './Tables/Table_90.txt',
                   './Tables/Table_91.txt',
                   './Tables/Table_92.txt',
                   './Tables/Table_93.txt',
                   './Tables/Table_94.txt',
                   './Tables/Table_95.txt',
                   './Tables/Table_96.txt',
                   './Tables/Table_97.txt',
                   './Tables/Table_98.txt',
                   './Tables/Table_99.txt',
                   './Tables/Table_100.txt',
                   './Tables/Table_101.txt',
                   './Tables/Table_102.txt')


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

#Run For loop to produce Tables 59-62:
for(i in 1:length(list_variables_to_run_4)){
  #names_of_columns
  names_of_columns <- c(list_variables_to_run_4[[i]])
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  
  #Reporting unadjusted (differences in means) and covariate-adjusted 
  #(OLS) estimates of treatment effects. We will use HC2 robust standard errors in all analyses and report 
  #$p$-values from two-tailed $t$-tests.
  
  names_of_columns <- c('Treated',names_of_columns)
  
  #DL
  names_of_columns_M1 <- c(names_of_columns,'Total_DL')
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_DL")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_DL <- lm_robust(as.formula(f), data = data_for_analysis)
  
  Variable_Names <- names(lm_adj_MT_DL$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Scientific Misinformation
  
  names_of_columns_M1 <- c(names_of_columns,'Total_Science_Misinfo')
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Total_Science_Misinfo")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_S_Misinfo <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #SM Use
  names_of_columns_M1 <- c(names_of_columns,'Social_Media_Use')
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M1)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Social_Media_Use")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_SM_use <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Moderator:  mean_cons
  
  names_of_columns_M2 <- c('mean_cons',names_of_columns)
  
  names_use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_M2)]
  
  data_for_analysis <- data_frame_1[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "mean_cons")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + . - cons_desk - cons_mobile")
  
  lm_adj_MT_consump <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Pulse (absolute partisanship)
  
  names_of_columns_M2 <- c('abs_part',names_of_columns)
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "abs_part")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Part <- lm_robust(as.formula(f), data = data_for_analysis)
  
  #Pulse (Pre-unreliable)
  
  
  names_of_columns_M2 <- c('Prop_Unreliable_NewsG_Score',names_of_columns)
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_M2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  colnames(data_for_analysis)[which(names(data_for_analysis) == "Prop_Unreliable_NewsG_Score")] <- "Moderator"
  
  f <- paste0(list_variables_to_run_4[[i]][1], " ~ Treated*Moderator + .")
  
  lm_adj_MT_Prop_Unrel <- lm_robust(as.formula(f), data = data_for_analysis)
  
  
  #Write Table
  texreg(list(lm_adj_MT_DL,lm_adj_MT_S_Misinfo,lm_adj_MT_SM_use,lm_adj_MT_consump,lm_adj_MT_Part,lm_adj_MT_Prop_Unrel),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Dig. Lit.", "Sci. Misinf.","S.M. Use","News Consump.","Partisan. of News Diet","Unreliable News Consumed"),
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
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_SM_ref','Prop_Unreliable_NewsG_Score_SM_ref'),
                               c('Prop_Reliable_NewsG_Score_dv_SM_ref','Prop_Reliable_NewsG_Score_SM_ref'),
                               c('Count_Unreliable_NewsG_Score_dv_SM_ref','Count_Unreliable_NewsG_Score_SM_ref'),
                               c('Count_Reliable_NewsG_Score_dv_SM_ref','Count_Reliable_NewsG_Score_SM_ref'),
                               c('Average_domain_NewsG_Score_dv_SM_ref','Average_domain_NewsG_Score_SM_ref'),
                               c('Prop_Unreliable_NewsG_Score_post_SM_ref','Prop_Unreliable_NewsG_Score_SM_ref'),
                               c('Prop_Reliable_NewsG_Score_post_SM_ref','Prop_Reliable_NewsG_Score_SM_ref'),
                               c('Count_Unreliable_NewsG_Score_post_SM_ref','Count_Unreliable_NewsG_Score_SM_ref'),
                               c('Count_Reliable_NewsG_Score_post_SM_ref','Count_Reliable_NewsG_Score_SM_ref'),
                               c('Average_domain_NewsG_Score_post_SM_ref','Average_domain_NewsG_Score_SM_ref'))

#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Proportion News Diet of Referrals From Social Media (Twitter and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_113.txt',
                   './Tables/Table_114.txt',
                   './Tables/Table_115.txt',
                   './Tables/Table_116.txt',
                   './Tables/Table_117.txt',
                   './Tables/Table_118.txt',
                   './Tables/Table_119.txt',
                   './Tables/Table_120.txt',
                   './Tables/Table_121.txt',
                   './Tables/Table_122.txt')

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
                         'Prop_Unreliable_NewsG_Score_SM_ref',
                         'Prop_Reliable_NewsG_Score_SM_ref',
                         'Count_Unreliable_NewsG_Score_SM_ref',
                         'Count_Reliable_NewsG_Score_SM_ref',
                         'Average_domain_NewsG_Score_SM_ref',
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


i=1
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


#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_SM_ref'),
                               c('Prop_Reliable_NewsG_Score_dv_SM_ref'),
                               c('Count_Unreliable_NewsG_Score_dv_SM_ref'),
                               c('Count_Reliable_NewsG_Score_dv_SM_ref'),
                               c('Average_domain_NewsG_Score_dv_SM_ref'),
                               c('Prop_Unreliable_NewsG_Score_post_SM_ref'),
                               c('Prop_Reliable_NewsG_Score_post_SM_ref'),
                               c('Count_Unreliable_NewsG_Score_post_SM_ref'),
                               c('Count_Reliable_NewsG_Score_post_SM_ref'),
                               c('Average_domain_NewsG_Score_post_SM_ref'))

#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Proportion News Diet of Referrals From Social Media (Twitter and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_143.txt',
                   './Tables/Table_144.txt',
                   './Tables/Table_145.txt',
                   './Tables/Table_146.txt',
                   './Tables/Table_147.txt',
                   './Tables/Table_148.txt',
                   './Tables/Table_149.txt',
                   './Tables/Table_150.txt',
                   './Tables/Table_151.txt',
                   './Tables/Table_152.txt')

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


i=1
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

Decile_Names <- c('Top 100 Percent',
                  'Top 30 Percent',
                  'Top 20 Percent',
                  'Top 10 Percent')



Deciles <- quantile(Pulse_data$Prop_Unreliable_NewsG_Score, prob = c(0.60,0.70,0.80,0.90),na.rm=T)
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
  
  for(x in 1:length(Deciles)){
    #Pulse_data_2 <- Pulse_data %>% filter(Prop_Unreliable_NewsG_Score >= Deciles[x])
    Pulse_data_2 <- Pulse_data %>% filter(Prop_Unreliable_NewsG_Score >= Deciles[x])
    
    names_use <- names(Pulse_data_2)[(names(Pulse_data_2) %in% names_of_columns)]
    
    data_for_analysis <- Pulse_data_2[, names_use]
    
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
    Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
    z=2
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
    
    Stars_I_1 = ''
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
    ITT_1 <- paste0(ITT_coef,'(',ITT_SD,')',Stars_I_1)
    CACE_1 <- paste0(C_1_coef,'(',C_1_SD,')',Stars_C_1)
    CACE_2 <- paste0(C_2_coef,'(',C_2_SD,')',Stars_C_2)
    Prop_F <- as.character(round(Deciles[x],3))
    
    print(c(Decile_Names[x],Prop_F,ITT_1,CACE_1,CACE_2))
    
  }
  #<- rbind(,Prop_F,ITT_1,CACE_1,CACE_2))
  #list_text_models[i]
}



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




#T-test - Analyses:
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



list_power <- c()
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
  
  data_for_analysis <- na.omit(data_for_analysis)
  print(ncol(data_for_analysis))
  
  data_1 <- data_for_analysis %>% filter(Treated == 1)
  data_2 <- data_for_analysis %>% filter(Treated == 0)
  test_pwr <- pwr.t2n.test(n1 = nrow(data_1), n2 = nrow(data_2), 
                           sig.level = 0.05, power = NULL, 
                           d = 0.2, alternative="two.sided")
  
  pow_i <- round(test_pwr$power,3)
  pow_i <- as.character(pow_i)
  
  list_power <- c(list_power,pow_i)
}
list_power_1 <- list_power



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


list_text_Period <- c('Before July 1st',
                      'Before July 1st',
                      'Before July 1st',
                      'Before July 1st',
                      'Before July 1st',
                      'After July 1st',
                      'After July 1st',
                      'After July 1st',
                      'After July 1st',
                      'After July 1st')

Power_matrix <- matrix(c(list_text_models,list_text_Period,list_power),ncol=3)

colnames(Power_matrix) <- c('Model Testing Effect Of Intervention On This Variable:','Time Period','Power')



xtable(Power_matrix)

#Create Xtable Object:
xt <- xtable(Power_matrix,
             digits=2,
             caption= 'Power Analysis for Covariate-Adjusted Models using Behavioral Measures for Reporting a 0.2 Standard Deviation Change at the 95 Percent Statistical Significance',
             align=c(
               "p{1cm}|","|p{10cm}|",
               "p{3cm}|","p{1cm}|"))

#Name Columns:


write(print(xt,
            include.rownames=FALSE,
            sanitize.colnames.function = identity,
            caption.placement='top'),
      file='./Tables/Power_Table_1.txt')




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



i=6
list_power <- c()
for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c('Treated',list_variables_to_run_1[[i]])
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  print(ncol(data_for_analysis))
  
  data_for_analysis <- na.omit(data_for_analysis)
  data_1 <- data_for_analysis %>% filter(Treated == 1)
  data_2 <- data_for_analysis %>% filter(Treated == 0)
  test_pwr <- pwr.t2n.test(n1 = nrow(data_1), n2 = nrow(data_2), 
                           sig.level = 0.05, power = NULL, 
                           d = 0.2, alternative="two.sided")
  
  pow_i <- round(test_pwr$power,3)
  pow_i <- as.character(pow_i)
  list_power <- c(list_power,pow_i)
}

Power_matrix <- matrix(c(list_text_models,list_text_Period,list_power),ncol=3)

colnames(Power_matrix) <- c('Model Testing Effect Of Intervention On This Variable:','Time Period','Power')

#Create Xtable Object:
xt <- xtable(Power_matrix,
             digits=2,
             caption= 'Power Analysis for Covariate-Unadjusted Models using Behavioral Measures for Reporting a 0.2 Standard Deviation Change at the 95 Percent Statistical Significance',
             align=c(
               "p{1cm}|","|p{10cm}|",
               "p{3cm}|","p{1cm}|"))

#Name Columns:


write(print(xt,
            include.rownames=FALSE,
            sanitize.colnames.function = identity,
            caption.placement='top'),
      file='./Tables/Power_Table_2.txt')



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
list_text_models <- c('Belief in Misinformation about the Black Lives Matter Movement',
                      'Belief in True Information about the Black Lives Matter Movement',
                      'Belief in Misinformation about Covid-19',
                      'Belief in True Information about Covid-19',
                      'Trust in Media',
                      'Affective Polarization',
                      'Whether They Believe “Fake News is a Problem”',
                      'Whether They Believe “Fake News is a Problem in the Mainstream Media”',
                      'Trust in Institutions',
                      'Trust in CBS',
                      'Trust in ABC',
                      'Trust in NBC',
                      'Trust in CNN',
                      'Trust in Fox News')

#Run For loop to produce Tables 13-26:




list_power <- c()
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
  
  data_for_analysis <- na.omit(data_for_analysis)
  data_1 <- data_for_analysis %>% filter(Treated == 1)
  data_2 <- data_for_analysis %>% filter(Treated == 0)
  test_pwr <- pwr.t2n.test(n1 = nrow(data_1), n2 = nrow(data_2), 
                           sig.level = 0.05, power = NULL, 
                           d = 0.2, alternative="two.sided")
  
  pow_i <- round(test_pwr$power,3)
  pow_i <- as.character(pow_i)
  
  list_power <- c(list_power,pow_i)
}

Power_matrix <- matrix(c(list_text_models,list_power),ncol=2)

colnames(Power_matrix) <- c('Model Testing Effect Of Intervention On This Variable:','Power')

#Create Xtable Object:
xt <- xtable(Power_matrix,
             digits=2,
             caption= 'Power Analysis for Covariate-Adjusted Models using Attitudinal Measures for Reporting a 0.2 Standard Deviation Change at the 95 Percent Statistical Significance',
             align=c(
               "p{1cm}|","|p{10cm}|",
               "p{1cm}|"))

#Name Columns:


write(print(xt,
            include.rownames=FALSE,
            sanitize.colnames.function = identity,
            caption.placement='top'),
      file='./Tables/Power_Table_3.txt')



list_variables_to_run_1 = list(c('BLM_Misinfo_Index_w2'),
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

i=6
list_power <- c()
for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c(list_variables_to_run_1[[i]])
  data_for_analysis <- data_frame_1 %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  list_possible_covariates_for_use <- c('Treated',list_variables_to_run_1[[i]])
  data_for_analysis <- data_frame_1 %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  print(ncol(data_for_analysis))
  
  data_for_analysis <- na.omit(data_for_analysis)
  data_1 <- data_for_analysis %>% filter(Treated == 1)
  data_2 <- data_for_analysis %>% filter(Treated == 0)
  test_pwr <- pwr.t2n.test(n1 = nrow(data_1), n2 = nrow(data_2), 
                           sig.level = 0.05, power = NULL, 
                           d = 0.2, alternative="two.sided")
  
  pow_i <- round(test_pwr$power,3)
  pow_i <- as.character(pow_i)
  list_power <- c(list_power,pow_i)
}

Power_matrix <- matrix(c(list_text_models,list_power),ncol=2)

colnames(Power_matrix) <- c('Model Testing Effect Of Intervention On This Variable:','Power')

#Create Xtable Object:
xt <- xtable(Power_matrix,
             digits=2,
             caption= 'Power Analysis for Covariate-Unadjusted Models using Attitudinal Measures for Reporting a 0.2 Standard Deviation Change at the 95 Percent Statistical Significance',
             align=c(
               "p{1cm}|","|p{10cm}|",
               "p{1cm}|"))

#Name Columns:


write(print(xt,
            include.rownames=FALSE,
            sanitize.colnames.function = identity,
            caption.placement='top'),
      file='./Tables/Power_Table_4.txt')






















#Load in libraries:
library(dplyr)

#Establish set working directory: 

#Read in Survey Wave 1 Data
Wave_1_Data <- read.csv('./Data/NYUU0017_w1_OUTPUT.csv')
#Read in Survey Wave 2 Data
Wave_2_Data <- read.csv('./Data/NYUU0017_w2_OUTPUT.csv')



#Change the column names that were identical in Wave 1 and Wave 2
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


#Create income score variable (income_score)
dataframe_1$faminc_new  <- factor(dataframe_1$faminc_new , levels = c("Less than $10,000",
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
dataframe_1$income_score  <- as.numeric(dataframe_1$faminc_new)

#Create treatment variable (Treated)
dataframe_1$plugin_treat <- as.character(dataframe_1$plugin_treat)
dataframe_1$Treated <- ifelse(dataframe_1$plugin_treat == 'Offered plug-in',1,0)

#Create political cynicism variable (Wave 1: Pol_cyn_1 ; Wave 2: Pol_cyn_2)
dataframe_1$SM  <- factor(dataframe_1$SM, levels = c("Hardly any",
                                                     "Not very many",
                                                     "Quite a few"))
dataframe_1$Pol_cyn_1  <- as.numeric(dataframe_1$SM)
dataframe_1$SM_w2  <- factor(dataframe_1$SM_w2, levels = c("Hardly any",
                                                           "Not very many",
                                                           "Quite a few"))
dataframe_1$Pol_cyn_2  <- as.numeric(dataframe_1$SM_w2)

##Create Affective Polarization: variable (Wave 1: aff_pol_w1 ; Wave 2: aff_pol_w2)
dataframe_1$SMP3001 <- as.character(dataframe_1$SMP3001)
dataframe_1$SMP3001 <- as.numeric(dataframe_1$SMP3001)
dataframe_1$SMP3001_w2 <- as.character(dataframe_1$SMP3001_w2)
dataframe_1$SMP3001_w2 <- as.numeric(dataframe_1$SMP3001_w2)
dataframe_1$SMP3002 <- as.character(dataframe_1$SMP3002)
dataframe_1$SMP3002 <- as.numeric(dataframe_1$SMP3002)
dataframe_1$SMP3002_w2 <- as.character(dataframe_1$SMP3002_w2)
dataframe_1$SMP3002_w2 <- as.numeric(dataframe_1$SMP3002_w2)
dataframe_1$aff_pol_w1 <- abs((dataframe_1$SMP3001 - dataframe_1$SMP3002))
dataframe_1$aff_pol_w2 <- abs((dataframe_1$SMP3001_w2 - dataframe_1$SMP3002_w2))
dataframe_1$diff_aff_pol <- abs((dataframe_1$aff_pol_w1 - dataframe_1$aff_pol_w2))
dataframe_1$diff_aff_pol <- as.numeric(dataframe_1$diff_aff_pol)

## Create Dummy Variables for the browser that each respondent used (IE_dummy,Chrome_dummy,Firefox_dummy,Safari_dummy):
dataframe_1$web_browser <- as.character(dataframe_1$web_browser)
dataframe_1$IE_dummy <- ifelse(startsWith(dataframe_1$web_browser,'MS'),1,0)
dataframe_1$Chrome_dummy <- ifelse(startsWith(dataframe_1$web_browser,'Chrome'),1,0)
dataframe_1$Firefox_dummy <- ifelse(startsWith(dataframe_1$web_browser,'Firefox'),1,0)
dataframe_1$Safari_dummy <- ifelse(startsWith(dataframe_1$web_browser,'Safari'),1,0)

##Create Variables for the trust of different news producers:

#Trust in Fox News (Wave 1):
dataframe_1$SMP67007  <- factor(dataframe_1$SMP67007, levels = c('Not at all',
                                                                 'Not too much',
                                                                 'Some',
                                                                 'A lot'))
dataframe_1$Fox_Trust_1 <- as.numeric(dataframe_1$SMP67007)


#Trust in Fox News (Wave 2):
dataframe_1$SMP67007_w2  <- factor(dataframe_1$SMP67007_w2, levels = c('Not at all',
                                                                       'Not too much',
                                                                       'Some',
                                                                       'A lot'))
dataframe_1$Fox_Trust_2 <- as.numeric(dataframe_1$SMP67007_w2)



#Trust in CNN (Wave 1):
dataframe_1$SMP67008  <- factor(dataframe_1$SMP67008, levels = c('Not at all',
                                                                 'Not too much',
                                                                 'Some',
                                                                 'A lot'))
dataframe_1$CNN_Trust_1 <- as.numeric(dataframe_1$SMP67008)


#Trust in CNN (Wave 2):
dataframe_1$SMP67008_w2  <- factor(dataframe_1$SMP67008_w2, levels = c('Not at all',
                                                                       'Not too much',
                                                                       'Some',
                                                                       'A lot'))
dataframe_1$CNN_Trust_2 <- as.numeric(dataframe_1$SMP67008_w2)



#Trust in MSNBC (Wave 1):
dataframe_1$SMP67009  <- factor(dataframe_1$SMP67009, levels = c('Not at all',
                                                                 'Not too much',
                                                                 'Some',
                                                                 'A lot'))
dataframe_1$MSNBC_Trust_1 <- as.numeric(dataframe_1$SMP67009)

#Trust in MSNBC (Wave 2):
dataframe_1$SMP67009_w2  <- factor(dataframe_1$SMP67009_w2, levels = c('Not at all',
                                                                       'Not too much',
                                                                       'Some',
                                                                       'A lot'))

dataframe_1$MSNBC_Trust_2 <- as.numeric(dataframe_1$SMP67009_w2)

#Trust in CBS (Wave 1):
dataframe_1$SMP67010  <- factor(dataframe_1$SMP67010, levels = c('Not at all',
                                                                 'Not too much',
                                                                 'Some',
                                                                 'A lot'))

dataframe_1$CBS_Trust_1 <- as.numeric(dataframe_1$SMP67010)


#Trust in CBS (Wave 2):
dataframe_1$SMP67010_w2  <- factor(dataframe_1$SMP67010_w2, levels = c('Not at all',
                                                                       'Not too much',
                                                                       'Some',
                                                                       'A lot'))

dataframe_1$CBS_Trust_2 <- as.numeric(dataframe_1$SMP67010_w2)



#Trust in ABC (Wave 1):
dataframe_1$SMP67011  <- factor(dataframe_1$SMP67011, levels = c('Not at all',
                                                                 'Not too much',
                                                                 'Some',
                                                                 'A lot'))
dataframe_1$ABC_Trust_1 <- as.numeric(dataframe_1$SMP67011)

#Trust in ABC (Wave 2):
dataframe_1$SMP67011_w2  <- factor(dataframe_1$SMP67011_w2, levels = c('Not at all',
                                                                       'Not too much',
                                                                       'Some',
                                                                       'A lot'))

dataframe_1$ABC_Trust_2 <- as.numeric(dataframe_1$SMP67011_w2)

#Trust in NBC (Wave 1):
dataframe_1$SMP67012  <- factor(dataframe_1$SMP67012, levels = c('Not at all',
                                                                 'Not too much',
                                                                 'Some',
                                                                 'A lot'))
dataframe_1$NBC_Trust_1 <- as.numeric(dataframe_1$SMP67012)



dataframe_1$SMP67012_w2  <- factor(dataframe_1$SMP67012_w2, levels = c('Not at all',
                                                                       'Not too much',
                                                                       'Some',
                                                                       'A lot'))

dataframe_1$NBC_Trust_2 <- as.numeric(dataframe_1$SMP67012_w2)



##Create digital literacy variable:

#Run for loop to set the columns 18-23 as factors with the same levels
dl_matrix <- matrix(nrow=nrow(dataframe_1))
for(i in 18:23){
  dataframe_1[,i]  <- factor(dataframe_1[,i], levels = c('No Understanding',
                                                         '2',
                                                         '3',
                                                         '4',
                                                         'Full Understanding'))
  
  dl_matrix <- cbind(dl_matrix,dataframe_1[,i])
}
#Remove the first row in the dataset (Consists solely of NAs)
dl_matrix <- dl_matrix[,-1]

#Run for loop to set the columns 25-26 as factors with the same levels
for(i in c(25,26)){
  dataframe_1[,i]  <- factor(dataframe_1[,i], levels = c('Strongly Disagree',
                                                         '-3',
                                                         '-2',
                                                         '-1',
                                                         '0',
                                                         '1',
                                                         '2',
                                                         '3',
                                                         'Strongly Agree'))
  dl_matrix <- cbind(dl_matrix,dataframe_1[,i])
}

#Run for loop to set the columns 24-27 as factors with the same levels
for(i in c(24,27)){
  dataframe_1[,i]  <- factor(dataframe_1[,i], levels = c('Strongly Agree',
                                                         '3',
                                                         '2',
                                                         '1',
                                                         '0',
                                                         '-1',
                                                         '-2',
                                                         '-3',
                                                         'Strongly Disagree'))
  dl_matrix <- cbind(dl_matrix,dataframe_1[,i])
}

#We take the matrix with the all of the answers to our digital literacy question and convert the matrix into 
dl_matrix <- data.frame(dl_matrix)

#Rename the columns to correspond with each digital literacy question:
colnames(dl_matrix) <- c('DL_1','DL_2','DL_3','DL_4','DL_5','DL_6','DL_7','DL_8','DL_9','DL_10')

#Sum the values assigned from each digital literacy question:
dl_matrix <- dl_matrix %>% mutate(Total_DL = DL_1 + DL_2 + DL_3 + DL_4 + DL_5 + DL_6 + DL_7 + DL_8 + DL_9 + DL_10)
dl_matrix <- dl_matrix %>% select(Total_DL)


dataframe_1 <- cbind(dataframe_1,dl_matrix)

#Create Scientific Misinformation Index:

#Set factor levels for this scientific misinformation Q:
dataframe_1$SMP5361  <- factor(dataframe_1$SMP5361, levels = c('Strongly agree',
                                                               'Agree',
                                                               'Neither agree nor disagree',
                                                               'Disagree',
                                                               'Strongly disagree'))

#Set factor levels for this scientific misinformation Q:
dataframe_1$SMP5362  <- factor(dataframe_1$SMP5362, levels = c('Strongly disagree',
                                                               'Disagree',
                                                               'Neither agree nor disagree',
                                                               'Agree',
                                                               'Strongly agree'))

#Set factor levels for this scientific misinformation Q:
dataframe_1$SMP5363  <- factor(dataframe_1$SMP5363, levels = c('Strongly disagree',
                                                               'Disagree',
                                                               'Neither agree nor disagree',
                                                               'Agree',
                                                               'Strongly agree'))

#Set factor levels for this scientific misinformation Q:
dataframe_1$SMP5364  <- factor(dataframe_1$SMP5364, levels = c('Strongly disagree',
                                                               'Disagree',
                                                               'Neither agree nor disagree',
                                                               'Agree',
                                                               'Strongly agree'))

#Convert factors into numeric variables:
dataframe_1$SMP5361  <- as.numeric(dataframe_1$SMP5364)
dataframe_1$SMP5362  <- as.numeric(dataframe_1$SMP5362)
dataframe_1$SMP5363  <- as.numeric(dataframe_1$SMP5363)
dataframe_1$SMP5364  <- as.numeric(dataframe_1$SMP5364)


#Create average of numeric values to produce the scientific misinformation index:
dataframe_1 <- dataframe_1 %>% mutate(Total_Science_Misinfo = (SMP5361 + SMP5362 + SMP5363 + SMP5364)/4)


#SMP1020A:	Look at Facebook
#SMP1020B:	Post things on Facebook

#Set factor levels for how often respondent looks at Twitter:
dataframe_1$SMP1019A <- factor(dataframe_1$SMP1019A, levels = c("Never",
                                                                "Less often",
                                                                "Every few weeks",
                                                                "1 to 2 days a week",
                                                                "3 to 6 days a week",
                                                                "About once a day",
                                                                "Several times a day",
                                                                "At least 10 times a day"))


#Set factor levels for how often respondent posts on Twitter:
dataframe_1$SMP1019B <- factor(dataframe_1$SMP1019B, levels = c("Never",
                                                                "Less often",
                                                                "Every few weeks",
                                                                "1 to 2 days a week",
                                                                "3 to 6 days a week",
                                                                "About once a day",
                                                                "Several times a day",
                                                                "At least 10 times a day"))


#Set factor levels for how often respondent looks at Facebook:
dataframe_1$SMP1020A <- factor(dataframe_1$SMP1020A, levels = c("Never",
                                                                "Less often",
                                                                "Every few weeks",
                                                                "1 to 2 days a week",
                                                                "3 to 6 days a week",
                                                                "About once a day",
                                                                "Several times a day",
                                                                "At least 10 times a day"))

#Set factor levels for how often respondent posts on Facebook:
dataframe_1$SMP1020B <- factor(dataframe_1$SMP1020B, levels = c("Never",
                                                                "Less often",
                                                                "Every few weeks",
                                                                "1 to 2 days a week",
                                                                "3 to 6 days a week",
                                                                "About once a day",
                                                                "Several times a day",
                                                                "At least 10 times a day"))

#Convert factors into numeric variables:
dataframe_1$SMP1019A <- as.numeric(dataframe_1$SMP1019A)
dataframe_1$SMP1020A <- as.numeric(dataframe_1$SMP1020A)
dataframe_1$SMP1019B <- as.numeric(dataframe_1$SMP1019B)
dataframe_1$SMP1020B <- as.numeric(dataframe_1$SMP1020B)

#Create dummy variables for if each respondent has an account on Twitter or Facebook:
dataframe_1$Twitter_Acct <- ifelse(dataframe_1$SMP1012_18_1 == 'selected',1,0)
dataframe_1$FB_Acct <- ifelse(dataframe_1$SMP1012_18_2 == 'selected',1,0)

#Create dummy variables for if each respondent has an account on both Twitter or Facebook:
dataframe_1$FB_Twitt_Acct <- ifelse(dataframe_1$Twitter_Acct == 1 | dataframe_1$FB_Acct == 1,1,0)

#Add a zero value for respondents who do not have an account on Twitter or Facebook:
dataframe_1$SMP1019A <- ifelse(dataframe_1$Twitter_Acct == 0,0,dataframe_1$SMP1019A)
dataframe_1$SMP1019B <- ifelse(dataframe_1$Twitter_Acct == 0,0,dataframe_1$SMP1019B)
dataframe_1$SMP1020B <- ifelse(dataframe_1$FB_Acct == 0,0,dataframe_1$SMP1020B)
dataframe_1$SMP1020A <- ifelse(dataframe_1$FB_Acct == 0,0,dataframe_1$SMP1020A)

#Create social media use index:
dataframe_1$Social_Media_Use <- (dataframe_1$SMP1019A + dataframe_1$SMP1020A + dataframe_1$SMP1019B + dataframe_1$SMP1020B)/4


#Party ID Questions:

#Ensure that variables are strings:
dataframe_1$SMP8202A <- as.character(dataframe_1$SMP8202A) 
dataframe_1$SMP8202B <- as.character(dataframe_1$SMP8202B) 
dataframe_1$SMP8202C <- as.character(dataframe_1$SMP8202C) 
dataframe_1$SMP8202D <- as.character(dataframe_1$SMP8202D) 

#Create Party-ID variable with values (-3 = Strong Democrat ; 3 = Strong Republican):
dataframe_1$Party_ID <- dataframe_1$SMP8202A
dataframe_1$Party_ID <- ifelse(dataframe_1$Party_ID == 'Independent',dataframe_1$SMP8202B,dataframe_1$Party_ID)
dataframe_1$Party_ID <- ifelse(dataframe_1$Party_ID == 'A Democrat',dataframe_1$SMP8202C,dataframe_1$Party_ID)
dataframe_1$Party_ID <- ifelse(dataframe_1$Party_ID == 'A Republican',dataframe_1$SMP8202D,dataframe_1$Party_ID)


dataframe_1$party_score <- ifelse(dataframe_1$Party_ID == "Strong Democrat",-3,0)
dataframe_1$party_score <- ifelse(dataframe_1$Party_ID == "Not very strong Democrat",-2,dataframe_1$party_score)
dataframe_1$party_score <- ifelse(dataframe_1$Party_ID == "Closer to the Democratic Party",-1,dataframe_1$party_score)
dataframe_1$party_score <- ifelse(dataframe_1$Party_ID == "Strong Republican",3,dataframe_1$party_score)
dataframe_1$party_score <- ifelse(dataframe_1$Party_ID == "Not very strong Republican",2,dataframe_1$party_score)
dataframe_1$party_score <- ifelse(dataframe_1$Party_ID == "Closer to the Republican Party",1,dataframe_1$party_score)

#Create Dummy variable for race (White):
dataframe_1 <- dataframe_1 %>% mutate(race_white = ifelse(race == 'White',1,0))


#Create education variable with numeric values:
dataframe_1$educ <- factor(dataframe_1$educ, levels = c('No HS',
                                                        'High school graduate',
                                                        'Some college',
                                                        '2-year',
                                                        '4-year',
                                                        'Post-grad'))
dataframe_1$educ_score <- as.numeric(dataframe_1$educ)

#Create gender dummy variable (1 = Female):
dataframe_1$gender <- as.character(dataframe_1$gender)
dataframe_1 <- dataframe_1 %>% mutate(gender_dummy_fem = ifelse(gender == 'Female',1,0))


##Create variable for age:
dataframe_1 <- dataframe_1 %>% mutate(Age = 2020 - birthyr)
dataframe_1 <- dataframe_1 %>% mutate(Age_Sq = Age^2)
dataframe_1 <- dataframe_1 %>% mutate(DL_inverse = Total_DL - (max(Total_DL,na.rm=T)+1))


#Create Media Trust measure (Wave 1: Trust_Media_w1 ; Wave 2: Trust_Media_w2):
dataframe_1$SMP  <- factor(dataframe_1$SMP, levels = c('Keeps political leaders from doing their job',
                                                       'Stops political leaders from doing things that shouldn’t be done'))

dataframe_1$SMP69002  <- factor(dataframe_1$SMP69002, levels = c('Tend to favor one side',
                                                                 'Deal fairly with all sides'))

dataframe_1$SMP69003  <- factor(dataframe_1$SMP69003, levels = c('All the time',
                                                                 'Most of the time',
                                                                 'About half the time',
                                                                 'Once in a while',
                                                                 'Never'))

dataframe_1$SMP_w2  <- factor(dataframe_1$SMP_w2, levels = c('Keeps political leaders from doing their job',
                                                             'Stops political leaders from doing things that shouldn’t be done'))

dataframe_1$SMP69002_w2  <- factor(dataframe_1$SMP69002_w2, levels = c('Tend to favor one side',
                                                                       'Deal fairly with all sides'))

dataframe_1$SMP69003_w2  <- factor(dataframe_1$SMP69003_w2, levels = c('All the time',
                                                                       'Most of the time',
                                                                       'About half the time',
                                                                       'Once in a while',
                                                                       'Never'))

dataframe_1$SMP <- as.numeric(dataframe_1$SMP)
dataframe_1$SMP <- dataframe_1$SMP - 1
dataframe_1$SMP69002 <- as.numeric(dataframe_1$SMP69002)
dataframe_1$SMP69002 <- dataframe_1$SMP69002 - 1
dataframe_1$SMP69003 <- as.numeric(dataframe_1$SMP69003)
dataframe_1$SMP69003 <- dataframe_1$SMP69003 - 1
dataframe_1$SMP69003 <- dataframe_1$SMP69003/4

dataframe_1$SMP_w2 <- as.numeric(dataframe_1$SMP_w2)
dataframe_1$SMP_w2 <- dataframe_1$SMP_w2 - 1
dataframe_1$SMP69002_w2 <- as.numeric(dataframe_1$SMP69002_w2)
dataframe_1$SMP69002_w2 <- dataframe_1$SMP69002_w2 - 1
dataframe_1$SMP69003_w2 <- as.numeric(dataframe_1$SMP69003_w2)
dataframe_1$SMP69003_w2 <- dataframe_1$SMP69003_w2 - 1
dataframe_1$SMP69003_w2 <- dataframe_1$SMP69003_w2/4

dataframe_1$Trust_Media_w1 <- as.numeric(dataframe_1$SMP) + as.numeric(dataframe_1$SMP69002) + as.numeric(dataframe_1$SMP69003)
dataframe_1$Trust_Media_w2 <- as.numeric(dataframe_1$SMP_w2) + as.numeric(dataframe_1$SMP69002_w2) + as.numeric(dataframe_1$SMP69003_w2)






#Read in compliance check data (compliance_check_1,compliance_check_2,Complied):
Compliance_Check <- read.csv('./Data/Compliance_Check_Data.csv')

#Only use compliance checks during the period in which the first wave survey was out
First_Check <- Compliance_Check %>% filter(timestamp < 1593000000000)
#Use the last compliance check for each respondent in this time frame:
First_Check <- First_Check %>% group_by(user_id) %>% top_n(1, timestamp)
#Set column names:
colnames(First_Check) <- c('user_id','compliance_check_1','timestamp_1')

#Only use compliance checks during the period in which the second wave survey was out
Second_Check <- Compliance_Check %>% filter(timestamp > 1593000000000)
#Use the last compliance check for each respondent in this time frame:
Second_Check <- Second_Check %>% group_by(user_id) %>% top_n(1, timestamp)
#Set column names:
colnames(Second_Check) <- c('user_id','compliance_check_2','timestamp_2')

#Merge first check with the existing data-frame
df_1 <- merge(dataframe_1,First_Check,by.x='visa1',by.y='user_id',all.x=T)
df_new <- df_1 %>% select(visa1,Treated,compliance_check_1,timestamp_1)
#Merge the second check with the existing data-frame
df_new_1 <- merge(df_new,Second_Check,by.x='visa1',by.y='user_id',all.x=T)

#Create Groups of Respondent:
df_new_1 <- df_new_1 %>% mutate(Groups = ifelse(Treated == 1 & compliance_check_1 == 1 & compliance_check_2 == 1,'Treated and Complied','Treated, but only complied in the second wave'))
df_new_1 <- df_new_1 %>% mutate(Groups = ifelse(Treated == 1 & compliance_check_1 == 1 & compliance_check_2 == 0,'Treated, but removed NewsGuard between the first and second wave',Groups))
df_new_1 <- df_new_1 %>% mutate(Groups = ifelse(Treated == 1 & compliance_check_1 == 0 & compliance_check_2 == 0,'Treated, but never complied',Groups))
df_new_1 <- df_new_1 %>% mutate(Groups = ifelse(Treated == 0 & compliance_check_1 == 0 & compliance_check_2 == 0,'Not Treated',Groups))
df_new_1 <- df_new_1 %>% mutate(Groups = ifelse(Treated == 0 & compliance_check_1 == 1,'Not Treated, but had NewsGuard',Groups))
df_new_1 <- df_new_1 %>% mutate(Groups = ifelse(Treated == 0 & compliance_check_2 == 1,'Not Treated, but had NewsGuard',Groups))
df_new_1 <- df_new_1 %>% mutate(Groups = ifelse(Treated == 0 & is.na(compliance_check_2),'Not Treated, but missing some compliance check data',Groups))
df_new_1 <- df_new_1 %>% mutate(Groups = ifelse(Treated == 1 & is.na(compliance_check_2),'Treated, but missing some compliance check data',Groups))
df_new_1 <- df_new_1 %>% mutate(Groups = ifelse(Treated == 0 & is.na(compliance_check_1),'Not Treated, but missing some compliance check data',Groups))
df_new_1 <- df_new_1 %>% mutate(Groups = ifelse(Treated == 1 & is.na(compliance_check_1),'Treated, but missing some compliance check data',Groups))

#Create measure that is only assigned a "1" if the respondent passed both compliance checks:
df_new_1 <- df_new_1 %>% mutate(Complied = ifelse(compliance_check_1 == 1 & compliance_check_2 == 1,1,0))

#Select only a few variables and merge with existing dataset:
df_new_2 <- df_new_1 %>% select(visa1,compliance_check_1,compliance_check_2,Complied,Groups)

dataframe_1 <- merge(dataframe_1,df_new_2,by='visa1',all.x=T)


#Create Ideology Score:
dataframe_1$SMP8201 <- factor(dataframe_1$SMP8201, levels = c('Very liberal',
                                                              'Somewhat liberal',
                                                              'Slightly liberal',
                                                              'Moderate; middle of the road',
                                                              'Slightly conservative',
                                                              'Somewhat conservative',
                                                              'Very conservative'))


dataframe_1$ideo_score <- as.numeric(dataframe_1$SMP8201)


#Create COVID-19 Misinformation and Information Index:

#Set factor levels
dataframe_1$SMP5061_w2 <- factor(dataframe_1$SMP5061_w2, levels = c("Not at all accurate",
                                                                    "Not very accurate",
                                                                    "Somewhat accurate",
                                                                    "Very accurate"))
#Set factor levels
dataframe_1$SMP5062_w2 <- factor(dataframe_1$SMP5062_w2, levels = c("Not at all accurate",
                                                                    "Not very accurate",
                                                                    "Somewhat accurate",
                                                                    "Very accurate"))

#Set factor levels
dataframe_1$SMP5063_w2 <- factor(dataframe_1$SMP5063_w2, levels = c("Not at all accurate",
                                                                    "Not very accurate",
                                                                    "Somewhat accurate",
                                                                    "Very accurate"))

#Set factor levels
dataframe_1$SMP5064_w2 <- factor(dataframe_1$SMP5064_w2, levels = c("Not at all accurate",
                                                                    "Not very accurate",
                                                                    "Somewhat accurate",
                                                                    "Very accurate"))

#Set factor levels
dataframe_1$SMP5065_w2 <- factor(dataframe_1$SMP5065_w2, levels = c("Not at all accurate",
                                                                    "Not very accurate",
                                                                    "Somewhat accurate",
                                                                    "Very accurate"))

#Set Convert factors into numeric values:
dataframe_1$SMP5061_w2 <- as.numeric(dataframe_1$SMP5061_w2)
dataframe_1$SMP5062_w2 <- as.numeric(dataframe_1$SMP5062_w2)
dataframe_1$SMP5063_w2 <- as.numeric(dataframe_1$SMP5063_w2)
dataframe_1$SMP5064_w2 <- as.numeric(dataframe_1$SMP5064_w2)
dataframe_1$SMP5065_w2 <- as.numeric(dataframe_1$SMP5065_w2)

#Set indices:
dataframe_1 <- dataframe_1 %>% group_by(visa1)  %>% mutate(Covid_Misinfo_Index_w2 = mean(c(SMP5061_w2,SMP5062_w2,SMP5064_w2),na.rm=T))
dataframe_1 <- dataframe_1 %>% group_by(visa1)  %>% mutate(Covid_info_Index_w2 = mean(c(SMP5063_w2,SMP5065_w2),na.rm=T))


#Create BLM Misinformation and Information Index:

#Set factor levels:
dataframe_1$SMP5081_w2 <- factor(dataframe_1$SMP5081_w2, levels = c("Not at all accurate",
                                                                    "Not very accurate",
                                                                    "Somewhat accurate",
                                                                    "Very accurate"))

#Set factor levels:
dataframe_1$SMP5082_w2 <- factor(dataframe_1$SMP5082_w2, levels = c("Not at all accurate",
                                                                    "Not very accurate",
                                                                    "Somewhat accurate",
                                                                    "Very accurate"))

#Set factor levels:
dataframe_1$SMP5083_w2 <- factor(dataframe_1$SMP5083_w2, levels = c("Not at all accurate",
                                                                    "Not very accurate",
                                                                    "Somewhat accurate",
                                                                    "Very accurate"))

#Set factor levels:
dataframe_1$SMP5084_w2 <- factor(dataframe_1$SMP5084_w2, levels = c("Not at all accurate",
                                                                    "Not very accurate",
                                                                    "Somewhat accurate",
                                                                    "Very accurate"))

#Set factor levels:
dataframe_1$SMP5085_w2 <- factor(dataframe_1$SMP5085_w2, levels = c("Not at all accurate",
                                                                    "Not very accurate",
                                                                    "Somewhat accurate",
                                                                    "Very accurate"))

#Set Convert factors into numeric values:
dataframe_1$SMP5081_w2 <- as.numeric(dataframe_1$SMP5081_w2)
dataframe_1$SMP5082_w2 <- as.numeric(dataframe_1$SMP5082_w2)
dataframe_1$SMP5083_w2 <- as.numeric(dataframe_1$SMP5083_w2)
dataframe_1$SMP5084_w2 <- as.numeric(dataframe_1$SMP5084_w2)
dataframe_1$SMP5085_w2 <- as.numeric(dataframe_1$SMP5085_w2)


#Set indices:
dataframe_1 <- dataframe_1 %>% group_by(visa1)  %>% mutate(BLM_Misinfo_Index_w2 = mean(c(SMP5081_w2,SMP5082_w2,SMP5084_w2),na.rm=T))
dataframe_1 <- dataframe_1 %>% group_by(visa1)  %>% mutate(BLM_info_Index_w2 = mean(c(SMP5083_w2,SMP5085_w2),na.rm=T))




#Trust and Consumption in news:

#Consumption of news on news networks:
dataframe_1$SMP4201 <- as.character(dataframe_1$SMP4201)
unique(dataframe_1$SMP4201)
dataframe_1$SMP4201 <- factor(dataframe_1$SMP4201, levels = c("Never",
                                                              "Less Than Once a Week",
                                                              "At Least Once a Week",
                                                              "Several Times a Week",
                                                              "Every Day"))
dataframe_1$cons_news_n <- as.numeric(dataframe_1$SMP4201)

#Consumption of news on cable television:
dataframe_1$SMP4202 <- as.character(dataframe_1$SMP4202)
dataframe_1$SMP4202 <- factor(dataframe_1$SMP4202, levels = c("Never",
                                                              "Less Than Once a Week",
                                                              "At Least Once a Week",
                                                              "Several Times a Week",
                                                              "Every Day"))
dataframe_1$cons_cable <- as.numeric(dataframe_1$SMP4202)


#Consumption of news in print:
dataframe_1$SMP4203 <- as.character(dataframe_1$SMP4203)
dataframe_1$SMP4203 <- factor(dataframe_1$SMP4203, levels = c("Never",
                                                              "Less Than Once a Week",
                                                              "At Least Once a Week",
                                                              "Several Times a Week",
                                                              "Every Day"))
dataframe_1$cons_print <- as.numeric(dataframe_1$SMP4203)

#Consumption of news on public radio:
dataframe_1$SMP4204 <- as.character(dataframe_1$SMP4204)
dataframe_1$SMP4204 <- factor(dataframe_1$SMP4204, levels = c("Never",
                                                              "Less Than Once a Week",
                                                              "At Least Once a Week",
                                                              "Several Times a Week",
                                                              "Every Day"))
dataframe_1$cons_public <- as.numeric(dataframe_1$SMP4204)


#Consumption of news on talk radio:
dataframe_1$SMP4205 <- as.character(dataframe_1$SMP4205)
dataframe_1$SMP4205 <- factor(dataframe_1$SMP4205, levels = c("Never",
                                                              "Less Than Once a Week",
                                                              "At Least Once a Week",
                                                              "Several Times a Week",
                                                              "Every Day"))
dataframe_1$cons_talk <- as.numeric(dataframe_1$SMP4205)


#Consumption of news on desktop:
dataframe_1$SMP4206 <- as.character(dataframe_1$SMP4206)
dataframe_1$SMP4206 <- factor(dataframe_1$SMP4206, levels = c("Never",
                                                              "Less Than Once a Week",
                                                              "At Least Once a Week",
                                                              "Several Times a Week",
                                                              "Every Day"))
dataframe_1$cons_desk <- as.numeric(dataframe_1$SMP4206)


#Consumption of news on mobile device:
dataframe_1$SMP4207 <- as.character(dataframe_1$SMP4207)
dataframe_1$SMP4207 <- factor(dataframe_1$SMP4207, levels = c("Never",
                                                              "Less Than Once a Week",
                                                              "At Least Once a Week",
                                                              "Several Times a Week",
                                                              "Every Day"))
dataframe_1$cons_mobile <- as.numeric(dataframe_1$SMP4207)


#Trust in news  on social media:
dataframe_1$SMP66001 <- as.character(dataframe_1$SMP66001)
dataframe_1$SMP66002 <- as.character(dataframe_1$SMP66002)
dataframe_1$SMP66001 <- as.numeric(dataframe_1$SMP66001)
dataframe_1$SMP66002 <- as.numeric(dataframe_1$SMP66002)

dataframe_1 <- dataframe_1 %>% group_by(caseid) %>% mutate(trust_news_sm = mean(c(SMP66001,SMP66002),na.rm=T))

#Trust in newspapers:
dataframe_1$SMP67001 <- as.character(dataframe_1$SMP67001)
dataframe_1$SMP67001 <- factor(dataframe_1$SMP67001, levels = c("Not at all",
                                                                "Not too much",
                                                                "Some",
                                                                "A lot"))
dataframe_1$trust_news <- as.numeric(dataframe_1$SMP67001)





E#Create is Fake News a problem in the mainstream media? variable:
dataframe_1$SMP4310 <- factor(dataframe_1$SMP4310, levels = c("5 - Mistakes by the mainstream media are rare, and I generally find what they report to be credible",
                                                              "4",
                                                              "3",
                                                              "2",
                                                              "1 - \"Fake news\" is a serious problem affecting the mainstream media"))
dataframe_1$SMP4310 <- as.numeric(dataframe_1$SMP4310)

dataframe_1$SMP4310<- dataframe_1$SMP4310-1
mean(dataframe_1$SMP4310,na.rm=T)


sd_4310 <- sd(dataframe_1$SMP4310,na.rm=T)



dataframe_1$SMP4310_w2 <- factor(dataframe_1$SMP4310_w2, levels = c("5 - Mistakes by the mainstream media are rare, and I generally find what they report to be credible",
                                                                    "4",
                                                                    "3",
                                                                    "2",
                                                                    "1 - \"Fake news\" is a serious problem affecting the mainstream media"))
dataframe_1$SMP4310_w2 <- as.numeric(dataframe_1$SMP4310_w2)





#Fake News is a Problem Variable


dataframe_1$SMP4326 <- factor(dataframe_1$SMP4326, levels = c("Not a problem at all",
                                                              "A small problem",
                                                              "A moderately big problem",
                                                              "A very big problem"))

dataframe_1$SMP4326 <- as.numeric(dataframe_1$SMP4326)


dataframe_1$SMP4326_w2 <- factor(dataframe_1$SMP4326_w2, levels = c("Not a problem at all",
                                                                    "A small problem",
                                                                    "A moderately big problem",
                                                                    "A very big problem"))


dataframe_1$SMP4326_w2 <- as.numeric(dataframe_1$SMP4326_w2)


#### Trust in Institutions variable
dataframe_1$SMP6701 <- as.character(dataframe_1$SMP6701)
dataframe_1$SMP6701 <- as.numeric(dataframe_1$SMP6701)
dataframe_1$SMP6702 <- as.character(dataframe_1$SMP6702)
dataframe_1$SMP6702 <- as.numeric(dataframe_1$SMP6702)
dataframe_1$SMP6703 <- as.character(dataframe_1$SMP6703)
dataframe_1$SMP6703 <- as.numeric(dataframe_1$SMP6703)
dataframe_1$SMP6704 <- as.character(dataframe_1$SMP6704)
dataframe_1$SMP6704 <- as.numeric(dataframe_1$SMP6704)

dataframe_1$SMP6701_w2 <- as.character(dataframe_1$SMP6701_w2)
dataframe_1$SMP6701_w2 <- as.numeric(dataframe_1$SMP6701_w2)
dataframe_1$SMP6702_w2 <- as.character(dataframe_1$SMP6702_w2)
dataframe_1$SMP6702_w2 <- as.numeric(dataframe_1$SMP6702_w2)
dataframe_1$SMP6703_w2 <- as.character(dataframe_1$SMP6703_w2)
dataframe_1$SMP6703_w2 <- as.numeric(dataframe_1$SMP6703_w2)
dataframe_1$SMP6704_w2 <- as.character(dataframe_1$SMP6704_w2)
dataframe_1$SMP6704_w2 <- as.numeric(dataframe_1$SMP6704_w2)

dataframe_1 <- dataframe_1 %>% group_by(visa1)  %>% mutate(Trust_inst_w1 = mean(c(SMP6701,SMP6702,SMP6703,SMP6704)))
dataframe_1 <- dataframe_1 %>% group_by(visa1)  %>% mutate(Trust_inst_w2 = mean(c(SMP6701_w2,SMP6702_w2,SMP6703_w2,SMP6704_w2)))

#Mean consumption of news on mobile and desktop:
dataframe_1 <- dataframe_1 %>% group_by(visa1) %>% mutate(mean_cons = mean(c(cons_desk,cons_mobile),na.rm=T))

#Create Dummy variable for if we have pulse data for that respondent:
######Read in pre-treatment Pulse Data:
Pulse_Pre <- read.csv('./Data/pulse_vars_pre.csv')
######Read in post-treatment Pulse Data:
Pulse_Post <- read.csv('./Data/pulse_vars_post.csv')
#Merge Survey Data with digital
Pulse_data <- merge(dataframe_1,Pulse_Pre,by='caseid')
Pulse_data <- merge(Pulse_data,Pulse_Post,by='caseid')
####### Pull in DV Data ##############
Pulse_DV <- read.csv('./Data/pulse_vars_DV.csv')
Pulse_data <- merge(Pulse_data,Pulse_DV,by='caseid')
Pulse_data$Pulse_Dummy <- 1
Pulse_data <- Pulse_data %>% select(caseid,Pulse_Dummy)
dataframe_1<- merge(dataframe_1,Pulse_data,by='caseid',all=T)
write.csv(dataframe_1,'./Data/Clean_NewsGuard_Survey_Study.csv')



################################### Digital Trace Data  (YouGov Pulse Data) ##################################

######Read in pre-treatment Pulse Data:
Pulse_Pre <- read.csv('./Data/pulse_vars_pre.csv')
######Read in post-treatment Pulse Data:
Pulse_Post <- read.csv('./Data/pulse_vars_post.csv')
#Merge Survey Data with digital
Pulse_data <- merge(dataframe_1,Pulse_Pre,by='caseid')
Pulse_data <- merge(Pulse_data,Pulse_Post,by='caseid')
####### Pull in DV Data ##############
Pulse_DV <- read.csv('./Data/pulse_vars_DV.csv')
Pulse_data <- merge(Pulse_data,Pulse_DV,by='caseid')
Pulse_data <- Pulse_data %>% mutate(Prop_Unreliable_NewsG_Score_dv = ifelse(is.na(Average_domain_NewsG_Score_dv),NA,Prop_Unreliable_NewsG_Score_dv))
Pulse_data <- Pulse_data %>% mutate(Prop_Reliable_NewsG_Score_dv = ifelse(is.na(Average_domain_NewsG_Score_dv),NA,Prop_Reliable_NewsG_Score_dv))
Pulse_data <- Pulse_data %>% mutate(Prop_Unreliable_NewsG_Score_post = ifelse(is.na(Average_domain_NewsG_Score_post),NA,Prop_Unreliable_NewsG_Score_post))
Pulse_data <- Pulse_data %>% mutate(Prop_Reliable_NewsG_Score_post = ifelse(is.na(Average_domain_NewsG_Score_post),NA,Prop_Reliable_NewsG_Score_post))
Pulse_data <- Pulse_data %>% mutate(Prop_Unreliable_NewsG_Score = ifelse(is.na(Average_domain_NewsG_Score),NA,Prop_Unreliable_NewsG_Score))
Pulse_data <- Pulse_data %>% mutate(Prop_Reliable_NewsG_Score = ifelse(is.na(Average_domain_NewsG_Score),NA,Prop_Reliable_NewsG_Score))
Pulse_data$partisanship_news_diet <- as.numeric(Pulse_data$partisanship_news_diet)
Pulse_data$abs_part <- abs(Pulse_data$partisanship_news_diet)

write.csv(Pulse_data,'./Data/Clean_NewsGuard_Digital_Trace_Data.csv')









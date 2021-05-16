library(tidyverse)
library(dtplyr)
library(data.table)

# pulse data
load("Data/pulse_desktop_clean.Rdata")

# get Eady/Linder mediascores
mediascores <- read_csv("Data/Media_ideology.csv")
mediascores$domain <- str_to_lower(mediascores$domain)

# get treatment start dates
w1 <- read_csv("Data/NYUU0017_w1_OUTPUT.csv")
w2 <- read_csv("Data/NYUU0017_w2_OUTPUT.csv")

################################################################################
# 1) BEFORE TREATMENT
################################################################################

# select only relevant URLs
pulse_subset <- desktop_3[desktop_3$date <= as.Date("2020-06-08"),]

# left join with start dates
w1 <- left_join(w1, select(w2, caseid = caseid_w1, caseid_pulse = caseid))
#w1 <- lazy_dt(w1)
#mediascores <- lazy_dt(mediascores)
#pulse_subset <- lazy_dt(pulse_subset)
pulse_subset <- left_join(pulse_subset, select(w1, caseid = caseid_pulse, caseid_w1 = caseid, starttime))
pulse_subset <- left_join(pulse_subset, select(mediascores, domain, zeta))
pulse_subset <- as_tibble(pulse_subset)

# keep only visits before treatment
pulse_subset <- pulse_subset[pulse_subset$date < pulse_subset$starttime,]

#Amount of time consuming news with a newsguard score

pulse_subset_2<- pulse_subset %>% mutate(visit_score = ifelse(is.na(domain_credibility_score),0,1))
pulse_subset_2<- pulse_subset_2 %>% mutate(Duration_with_score = visit_score*page_duration)

head(pulse_subset_2)

# aggregate for each person
pulse_agg <- pulse_subset_2 %>% 
  group_by(caseid_w1) %>% 
  # compute relevant metrics
  summarize(totalnewsbinary = max(visit_ng),
            totalnewscount = sum(visit_ng),
            totalnewsduration = sum(visit_ng*page_duration),
            log_news = log(totalnewscount),
            totalvisits = n(),
            log_total = log(totalvisits),
            total_news_score_duration = sum(Duration_with_score),
            
            Average_domain_NewsG_Score = mean(domain_credibility_score, na.rm = TRUE),
            Count_Reliable_NewsG_Score =  log10(sum(domain_reliable_dummy)+1),
            Count_Unreliable_NewsG_Score = log10(sum(domain_unreliable_dummy)+1),
            Prop_Reliable_NewsG_Score = sum(domain_reliable_dummy)/totalnewscount,
            Prop_Unreliable_NewsG_Score = sum(domain_unreliable_dummy)/totalnewscount,
            partisanship_news_diet = mean(zeta, na.rm = TRUE),
            
            Prop_Reliable_NewsG_Score_dur = sum(domain_reliable_dummy*page_duration)/totalnewsduration,
            Prop_Unreliable_NewsG_Score_dur = sum(domain_unreliable_dummy*page_duration)/totalnewsduration,
            Count_Unreliable_NewsG_Score_dur = sum(domain_unreliable_dummy*page_duration),
            Count_Reliable_NewsG_Score_dur = sum(domain_reliable_dummy*page_duration),
            Average_domain_NewsG_Score_dur = sum(domain_credibility_score*page_duration,na.rm=T)/total_news_score_duration
  )

pulse_agg <- as_tibble(pulse_agg)
names(pulse_agg)[1] <- "caseid"

write_csv(pulse_agg, "Data/pulse_vars_pre.csv")

mean(pulse_agg$Average_domain_NewsG_Score_dur,na.rm=T)
mean(pulse_agg$Average_domain_NewsG_Score,na.rm=T)


mean(pulse_agg$Prop_Unreliable_NewsG_Score_dur,na.rm=T)
mean(pulse_agg$Prop_Unreliable_NewsG_Score,na.rm=T)


################################################################################
# 2) DATA PREP: POST-TREATMENT
################################################################################

# select only relevant URLs
pulse_subset <- desktop_3[desktop_3$date <= as.Date("2020-06-30"),]

# left join with start dates
#pulse_subset <- lazy_dt(pulse_subset)
pulse_subset <- left_join(pulse_subset, select(w1, caseid = caseid_pulse, caseid_w1 = caseid, starttime))
pulse_subset <- left_join(pulse_subset, select(mediascores, domain, zeta))
pulse_subset <- as_tibble(pulse_subset)

# keep only visits after treatment
pulse_subset <- pulse_subset[pulse_subset$date > pulse_subset$starttime,]

#Amount of time consuming news with a newsguard score

pulse_subset_2<- pulse_subset %>% mutate(visit_score = ifelse(is.na(domain_credibility_score),0,1))
pulse_subset_2<- pulse_subset_2 %>% mutate(Duration_with_score = visit_score*page_duration)

head(pulse_subset_2)

# aggregate for each person
pulse_agg <- pulse_subset_2 %>% 
  group_by(caseid_w1) %>% 
  # compute relevant metrics
  summarize(totalnewsbinary = max(visit_ng),
            totalnewscount = sum(visit_ng),
            totalnewsduration = sum(visit_ng*page_duration),
            log_news = log(totalnewscount),
            totalvisits = n(),
            log_total = log(totalvisits),
            total_news_score_duration = sum(Duration_with_score),
            
            Average_domain_NewsG_Score_dv = mean(domain_credibility_score, na.rm = TRUE),
            Count_Reliable_NewsG_Score_dv =  log10(sum(domain_reliable_dummy)+1),
            Count_Unreliable_NewsG_Score_dv = log10(sum(domain_unreliable_dummy)+1),
            Prop_Reliable_NewsG_Score_dv = sum(domain_reliable_dummy)/totalnewscount,
            Prop_Unreliable_NewsG_Score_dv = sum(domain_unreliable_dummy)/totalnewscount,
            partisanship_news_diet_dv = mean(zeta, na.rm = TRUE),
            
            Prop_Reliable_NewsG_Score_dv_dur = sum(domain_reliable_dummy*page_duration)/totalnewsduration,
            Prop_Unreliable_NewsG_Score_dv_dur = sum(domain_unreliable_dummy*page_duration)/totalnewsduration,
            Count_Unreliable_NewsG_Score_dv_dur = sum(domain_unreliable_dummy*page_duration),
            Count_Reliable_NewsG_Score_dv_dur = sum(domain_reliable_dummy*page_duration),
            Average_domain_NewsG_Score_dv_dur = sum(domain_credibility_score*page_duration,na.rm=T)/total_news_score_duration
  )

pulse_agg <- as_tibble(pulse_agg)
names(pulse_agg)[1] <- "caseid"

write_csv(pulse_agg, "Data/pulse_vars_dv.csv")


mean(pulse_agg$Average_domain_NewsG_Score_dv_dur,na.rm=T)
mean(pulse_agg$Average_domain_NewsG_Score_dv,na.rm=T)


mean(pulse_agg$Prop_Unreliable_NewsG_Score_dv_dur,na.rm=T)
mean(pulse_agg$Prop_Unreliable_NewsG_Score_dv,na.rm=T)

################################################################################
# 3) DATA PREP: AFTER TREATMENT PERIOD
################################################################################

# select only relevant URLs
pulse_subset <- desktop_3[desktop_3$date > as.Date("2020-06-30"),]

# left join with start dates
#pulse_subset <- lazy_dt(pulse_subset)
pulse_subset <- left_join(pulse_subset, select(w1, caseid = caseid_pulse, caseid_w1 = caseid, starttime))
pulse_subset <- left_join(pulse_subset, select(mediascores, domain, zeta))
pulse_subset <- as_tibble(pulse_subset)

# keep only visits after treatment
pulse_subset <- pulse_subset[pulse_subset$date > pulse_subset$starttime,]

#Amount of time consuming news with a newsguard score

pulse_subset_2<- pulse_subset %>% mutate(visit_score = ifelse(is.na(domain_credibility_score),0,1))
pulse_subset_2<- pulse_subset_2 %>% mutate(Duration_with_score = visit_score*page_duration)

head(pulse_subset_2)

# aggregate for each person
pulse_agg <- pulse_subset_2 %>% 
  group_by(caseid_w1) %>% 
  # compute relevant metrics
  summarize(totalnewsbinary = max(visit_ng),
            totalnewscount = sum(visit_ng),
            totalnewsduration = sum(visit_ng*page_duration),
            log_news = log(totalnewscount),
            totalvisits = n(),
            log_total = log(totalvisits),
            total_news_score_duration = sum(Duration_with_score),
            
            Average_domain_NewsG_Score_post = mean(domain_credibility_score, na.rm = TRUE),
            Count_Reliable_NewsG_Score_post =  log10(sum(domain_reliable_dummy)+1),
            Count_Unreliable_NewsG_Score_post = log10(sum(domain_unreliable_dummy)+1),
            Prop_Reliable_NewsG_Score_post = sum(domain_reliable_dummy)/totalnewscount,
            Prop_Unreliable_NewsG_Score_post = sum(domain_unreliable_dummy)/totalnewscount,
            partisanship_news_diet_post = mean(zeta, na.rm = TRUE),
            
            Prop_Reliable_NewsG_Score_post_dur = sum(domain_reliable_dummy*page_duration)/totalnewsduration,
            Prop_Unreliable_NewsG_Score_post_dur = sum(domain_unreliable_dummy*page_duration)/totalnewsduration,
            Count_Unreliable_NewsG_Score_post_dur = sum(domain_unreliable_dummy*page_duration),
            Count_Reliable_NewsG_Score_post_dur = sum(domain_reliable_dummy*page_duration),
            Average_domain_NewsG_Score_post_dur = sum(domain_credibility_score*page_duration,na.rm=T)/total_news_score_duration
  )

pulse_agg <- as_tibble(pulse_agg)
names(pulse_agg)[1] <- "caseid"

write_csv(pulse_agg, "Data/pulse_vars_post.csv")



mean(pulse_agg$Average_domain_NewsG_Score_post_dur,na.rm=T)
mean(pulse_agg$Average_domain_NewsG_Score_post,na.rm=T)


mean(pulse_agg$Prop_Unreliable_NewsG_Score_post_dur,na.rm=T)
mean(pulse_agg$Prop_Unreliable_NewsG_Score_post,na.rm=T)


################################################################################
# 4) RESPONDENT BY DAY
################################################################################

# select only relevant URLs
# pulse_subset <- desktop_3[desktop_3$date <= as.Date("2020-06-08"),]
pulse_subset <- desktop_3
rm(desktop_3)

# left join with start dates
w1 <- left_join(w1, select(w2, caseid = caseid_w1, caseid_pulse = caseid))
#w1 <- lazy_dt(w1)
#mediascores <- lazy_dt(mediascores)
#pulse_subset <- lazy_dt(pulse_subset)
pulse_subset <- left_join(pulse_subset, select(w1, caseid = caseid_pulse, caseid_w1 = caseid, starttime))
pulse_subset <- left_join(pulse_subset, select(mediascores, domain, zeta))
pulse_subset <- as_tibble(pulse_subset)

# keep only visits before treatment
# pulse_subset <- pulse_subset[pulse_subset$date < pulse_subset$starttime,]

# aggregate for each person by day
pulse_agg <- pulse_subset %>% 
  group_by(caseid_w1, date) %>% 
  # compute relevant metrics
  summarize(totalnewsbinary_day = max(visit_ng),
            totalnewscount_day = sum(visit_ng),
            totalnewsduration_day = sum(visit_ng*page_duration),
            log_news_day = log(totalnewscount_day),
            totalvisits_day = n(),
            log_total_day = log(totalvisits_day),
            
            Average_domain_NewsG_Score_day = mean(domain_credibility_score, na.rm = TRUE),
            Count_Reliable_NewsG_Score_day =  log10(sum(domain_reliable_dummy)+1),
            Count_Unreliable_NewsG_Score_day = log10(sum(domain_unreliable_dummy)+1),
            Prop_Reliable_NewsG_Score_day = sum(domain_reliable_dummy)/totalnewscount_day,
            Prop_Unreliable_NewsG_Score_day = sum(domain_unreliable_dummy)/totalnewscount_day,
            partisanship_news_diet_day = mean(zeta, na.rm = TRUE),
            
            Prop_Reliable_NewsG_Score_day_dur = sum(domain_reliable_dummy*page_duration)/totalnewsduration_day,
            Prop_Unreliable_NewsG_Score_day_dur = sum(domain_unreliable_dummy*page_duration)/totalnewsduration_day
  )

pulse_agg <- as_tibble(pulse_agg)
names(pulse_agg)[1] <- "caseid"

write_csv(pulse_agg, "Data/pulsebyday.csv")

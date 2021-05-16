



load("Data/pulse_desktop_clean.Rdata")

desktop_3$session_start_time

pulse <- pulse %>% arrange(caseid, used_at)


desktop <- desktop_3 %>% arrange(caseid, session_start_time)

desktop$used_at <- desktop$session_start_time


desktop<- desktop %>% group_by(caseid) %>% mutate(prev1 = lag(domain),
                                               prev2 = lag(domain, 2),
                                               prev3 = lag(domain, 3))
                                               #prev4 = lag(domain, 4),
                                               #prev5 = lag(domain, 5))







desktop <- desktop %>% group_by(caseid) %>% mutate(within30 = ifelse(used_at <= (lag(used_at) + lag(page_duration) + 30), 1, 0),
                                               within302 = ifelse(used_at <= lag(used_at, 2) + lag(page_duration) + 30, 1, 0),
                                               within303 = ifelse(used_at <= lag(used_at, 3) + lag(page_duration) + 30, 1, 0),
                                               within15 = ifelse(used_at <= lag(used_at) + lag(page_duration) + 15, 1, 0),
                                               within152 = ifelse(used_at <= lag(used_at, 2) + lag(page_duration) + 15, 1, 0),
                                               within153 = ifelse(used_at <= lag(used_at, 3) + lag(page_duration) + 15, 1, 0),
                                               within45 = ifelse(used_at <= lag(used_at) + lag(page_duration) + 45, 1, 0),
                                               within452 = ifelse(used_at <= lag(used_at, 2) + lag(page_duration) + 45, 1, 0),
                                               within453 = ifelse(used_at <= lag(used_at, 3) + lag(page_duration) + 45, 1, 0))

desktop <- desktop %>% ungroup()

# 30 seconds

desktop <- desktop %>% mutate(refer_fb15 = ifelse(within15 == 1 & prev1 == "facebook.com", 1, 0))
#desktop <- desktop %>% mutate(refer_fb30 = ifelse(within302 == 1 & prev2 == "facebook.com", 1, refer_fb30))
#desktop <- desktop %>% mutate(refer_fb30 = ifelse(within303 == 1 & prev3 == "facebook.com", 1, refer_fb30))

desktop <- desktop %>% mutate(refer_goog15 = ifelse(within15 == 1 & prev1 == "google.com", 1, 0))
#desktop <- desktop %>% mutate(refer_goog30 = ifelse(within302 == 1 & prev2 == "google.com", 1, refer_goog30))
#desktop <- desktop %>% mutate(refer_goog30 = ifelse(within303 == 1 & prev3 == "google.com", 1, refer_goog30))

desktop <- desktop %>% mutate(refer_tw15 = ifelse(within15 == 1 & prev1 == "twitter.com", 1, 0))
#desktop <- desktop %>% mutate(refer_tw30 = ifelse(within302 == 1 & prev2 == "twitter.com", 1, refer_tw30))
#desktop <- desktop %>% mutate(refer_tw30 = ifelse(within303 == 1 & prev3 == "twitter.com", 1, refer_tw30))



desktop <- desktop %>% filter(visit_ng == 1)

desktop_fb_ref <- desktop %>% filter(refer_fb15 == 1)
desktop_twitt_ref <- desktop %>% filter(refer_tw15 == 1)
desktop_google_ref <- desktop %>% filter(refer_goog15 == 1)


desktop_All$used_at


desktop_All <- desktop %>% filter(refer_goog15 == 1 | refer_tw15 == 1 | refer_fb15 == 1)

desktop_SM <- desktop %>% filter(refer_tw15 == 1 | refer_fb15 == 1)



# need to merge in treatment indicator, survey weight

# get treatment start dates
w1 <- read_csv("Data/NYUU0017_w1_OUTPUT.csv")
w2 <- read_csv("Data/NYUU0017_w2_OUTPUT.csv")

# select only relevant URLs
pulse_subset <- desktop_All[desktop_All$used_at <= as.Date("2020-06-08"),]

# left join with start dates
w1 <- left_join(w1, select(w2, caseid = caseid_w1, caseid_pulse = caseid))
#w1 <- lazy_dt(w1)
#mediascores <- lazy_dt(mediascores)
#pulse_subset <- lazy_dt(pulse_subset)
pulse_subset <- left_join(pulse_subset, select(w1, caseid = caseid_pulse, caseid_w1 = caseid, starttime))
pulse_subset <- left_join(pulse_subset, select(mediascores, domain, zeta))
pulse_subset <- as_tibble(pulse_subset)

# keep only visits before treatment
pulse_subset <- pulse_subset[pulse_subset$used_at < pulse_subset$starttime,]

#Amount of time consuming news with a newsguard score

head(pulse_subset)

# aggregate for each person
pulse_agg <- pulse_subset %>% 
  group_by(caseid_w1) %>% 
  # compute relevant metrics
  summarize(totalnewsbinary_All_ref = max(visit_ng),
            totalnewscount_All_ref = sum(visit_ng),
            log_news_All_ref = log(totalnewscount_All_ref),
            
            Average_domain_NewsG_Score_All_ref = mean(domain_credibility_score, na.rm = TRUE),
            Count_Reliable_NewsG_Score_All_ref =  log10(sum(domain_reliable_dummy)+1),
            Count_Unreliable_NewsG_Score_All_ref = log10(sum(domain_unreliable_dummy)+1),
            Prop_Reliable_NewsG_Score_All_ref = sum(domain_reliable_dummy)/totalnewscount_All_ref,
            Prop_Unreliable_NewsG_Score_All_ref = sum(domain_unreliable_dummy)/totalnewscount_All_ref,
  )

pulse_agg <- as_tibble(pulse_agg)
names(pulse_agg)[1] <- "caseid"

write_csv(pulse_agg, "Data/pulse_vars_pre_ref_All.csv")


mean(pulse_agg$Prop_Unreliable_NewsG_Score_All_ref)
mean(pulse_agg$Average_domain_NewsG_Score_All_ref,na.rm=T)

pulse_sum <- pulse_agg

# need to merge in treatment indicator, survey weight

# get treatment start dates
w1 <- read_csv("Data/NYUU0017_w1_OUTPUT.csv")
w2 <- read_csv("Data/NYUU0017_w2_OUTPUT.csv")

# select only relevant URLs
pulse_subset <- desktop_SM[desktop_SM$used_at <= as.Date("2020-06-08"),]

# left join with start dates
w1 <- left_join(w1, select(w2, caseid = caseid_w1, caseid_pulse = caseid))
#w1 <- lazy_dt(w1)
#mediascores <- lazy_dt(mediascores)
#pulse_subset <- lazy_dt(pulse_subset)
pulse_subset <- left_join(pulse_subset, select(w1, caseid = caseid_pulse, caseid_w1 = caseid, starttime))
pulse_subset <- left_join(pulse_subset, select(mediascores, domain, zeta))
pulse_subset <- as_tibble(pulse_subset)

# keep only visits before treatment
pulse_subset <- pulse_subset[pulse_subset$used_at < pulse_subset$starttime,]

#Amount of time consuming news with a newsguard score

head(pulse_subset)

# aggregate for each person
pulse_agg <- pulse_subset %>% 
  group_by(caseid_w1) %>% 
  # compute relevant metrics
  summarize(totalnewsbinary_SM_ref = max(visit_ng),
            totalnewscount_SM_ref = sum(visit_ng),
            log_news_SM_ref = log(totalnewscount_SM_ref),
            
            Average_domain_NewsG_Score_SM_ref = mean(domain_credibility_score, na.rm = TRUE),
            Count_Reliable_NewsG_Score_SM_ref =  log10(sum(domain_reliable_dummy)+1),
            Count_Unreliable_NewsG_Score_SM_ref = log10(sum(domain_unreliable_dummy)+1),
            Prop_Reliable_NewsG_Score_SM_ref = sum(domain_reliable_dummy)/totalnewscount_SM_ref,
            Prop_Unreliable_NewsG_Score_SM_ref = sum(domain_unreliable_dummy)/totalnewscount_SM_ref,
  )

pulse_agg <- as_tibble(pulse_agg)
names(pulse_agg)[1] <- "caseid"

write_csv(pulse_agg, "Data/pulse_vars_pre_ref_SM.csv")


mean(pulse_agg$Prop_Unreliable_NewsG_Score_SM_ref)
mean(pulse_agg$Average_domain_NewsG_Score_SM_ref,na.rm=T)



pulse_sum <- merge(pulse_sum,pulse_agg,by='caseid')




# need to merge in treatment indicator, survey weight

# get treatment start dates
w1 <- read_csv("Data/NYUU0017_w1_OUTPUT.csv")
w2 <- read_csv("Data/NYUU0017_w2_OUTPUT.csv")

# select only relevant URLs
pulse_subset <- desktop_All[desktop_All$used_at <= as.Date("2020-06-30"),]

w1 <- left_join(w1, select(w2, caseid = caseid_w1, caseid_pulse = caseid))


# left join with start dates
#pulse_subset <- lazy_dt(pulse_subset)
pulse_subset <- left_join(pulse_subset, select(w1, caseid = caseid_pulse, caseid_w1 = caseid, starttime))
pulse_subset <- left_join(pulse_subset, select(mediascores, domain, zeta))
pulse_subset <- as_tibble(pulse_subset)

# keep only visits after treatment
pulse_subset <- pulse_subset[pulse_subset$used_at > pulse_subset$starttime,]



#Amount of time consuming news with a newsguard score

head(pulse_subset)

# aggregate for each person
pulse_agg <- pulse_subset %>% 
  group_by(caseid_w1) %>% 
  # compute relevant metrics
  summarize(totalnewsbinary_dv_All_ref = max(visit_ng),
            totalnewscount_dv_All_ref = sum(visit_ng),
            log_news_dv_All_ref = log(totalnewscount_dv_All_ref),
            
            Average_domain_NewsG_Score_dv_All_ref = mean(domain_credibility_score, na.rm = TRUE),
            Count_Reliable_NewsG_Score_dv_All_ref =  log10(sum(domain_reliable_dummy)+1),
            Count_Unreliable_NewsG_Score_dv_All_ref = log10(sum(domain_unreliable_dummy)+1),
            Prop_Reliable_NewsG_Score_dv_All_ref = sum(domain_reliable_dummy)/totalnewscount_dv_All_ref,
            Prop_Unreliable_NewsG_Score_dv_All_ref = sum(domain_unreliable_dummy)/totalnewscount_dv_All_ref,
  )

pulse_agg <- as_tibble(pulse_agg)
names(pulse_agg)[1] <- "caseid"

write_csv(pulse_agg, "Data/pulse_vars_dv_ref_All.csv")


mean(pulse_agg$Prop_Unreliable_NewsG_Score_dv_All_ref)
mean(pulse_agg$Average_domain_NewsG_Score_dv_All_ref,na.rm=T)


pulse_sum <- merge(pulse_sum,pulse_agg,by='caseid')


# need to merge in treatment indicator, survey weight

w1 <- read_csv("Data/NYUU0017_w1_OUTPUT.csv")
w2 <- read_csv("Data/NYUU0017_w2_OUTPUT.csv")

# select only relevant URLs
pulse_subset <- desktop_SM[desktop_SM$used_at <= as.Date("2020-06-30"),]

w1 <- left_join(w1, select(w2, caseid = caseid_w1, caseid_pulse = caseid))


# left join with start dates
#pulse_subset <- lazy_dt(pulse_subset)
pulse_subset <- left_join(pulse_subset, select(w1, caseid = caseid_pulse, caseid_w1 = caseid, starttime))
pulse_subset <- left_join(pulse_subset, select(mediascores, domain, zeta))
pulse_subset <- as_tibble(pulse_subset)

# keep only visits after treatment
pulse_subset <- pulse_subset[pulse_subset$used_at > pulse_subset$starttime,]



#Amount of time consuming news with a newsguard score

head(pulse_subset)

# aggregate for each person
pulse_agg <- pulse_subset %>% 
  group_by(caseid_w1) %>% 
  # compute relevant metrics
  summarize(totalnewsbinary_dv_SM_ref = max(visit_ng),
            totalnewscount_dv_SM_ref = sum(visit_ng),
            log_news_dv_SM_ref = log(totalnewscount_dv_SM_ref),
            
            Average_domain_NewsG_Score_dv_SM_ref = mean(domain_credibility_score, na.rm = TRUE),
            Count_Reliable_NewsG_Score_dv_SM_ref =  log10(sum(domain_reliable_dummy)+1),
            Count_Unreliable_NewsG_Score_dv_SM_ref = log10(sum(domain_unreliable_dummy)+1),
            Prop_Reliable_NewsG_Score_dv_SM_ref = sum(domain_reliable_dummy)/totalnewscount_dv_SM_ref,
            Prop_Unreliable_NewsG_Score_dv_SM_ref = sum(domain_unreliable_dummy)/totalnewscount_dv_SM_ref,
  )

pulse_agg <- as_tibble(pulse_agg)
names(pulse_agg)[1] <- "caseid"

write_csv(pulse_agg, "Data/pulse_vars_dv_ref_SM.csv")


mean(pulse_agg$Prop_Unreliable_NewsG_Score_dv_SM_ref)
mean(pulse_agg$Average_domain_NewsG_Score_dv_SM_ref,na.rm=T)


pulse_sum <- merge(pulse_sum,pulse_agg,by='caseid')

# need to merge in treatment indicator, survey weight

w1 <- read_csv("Data/NYUU0017_w1_OUTPUT.csv")
w2 <- read_csv("Data/NYUU0017_w2_OUTPUT.csv")

# select only relevant URLs
pulse_subset <- desktop_All[desktop_All$used_at > as.Date("2020-06-30"),]

# left join with start dates
w1 <- left_join(w1, select(w2, caseid = caseid_w1, caseid_pulse = caseid))


# left join with start dates
#pulse_subset <- lazy_dt(pulse_subset)
pulse_subset <- left_join(pulse_subset, select(w1, caseid = caseid_pulse, caseid_w1 = caseid, starttime))
pulse_subset <- left_join(pulse_subset, select(mediascores, domain, zeta))
pulse_subset <- as_tibble(pulse_subset)

# keep only visits after treatment
pulse_subset <- pulse_subset[pulse_subset$used_at > pulse_subset$starttime,]

#Amount of time consuming news with a newsguard score

head(pulse_subset)


# aggregate for each person
pulse_agg <- pulse_subset %>% 
  group_by(caseid_w1) %>% 
  # compute relevant metrics
  summarize(totalnewsbinary_post_All_ref = max(visit_ng),
            totalnewscount_post_All_ref = sum(visit_ng),
            log_news_post_All_ref = log(totalnewscount_post_All_ref),
            
            Average_domain_NewsG_Score_post_All_ref = mean(domain_credibility_score, na.rm = TRUE),
            Count_Reliable_NewsG_Score_post_All_ref =  log10(sum(domain_reliable_dummy)+1),
            Count_Unreliable_NewsG_Score_post_All_ref = log10(sum(domain_unreliable_dummy)+1),
            Prop_Reliable_NewsG_Score_post_All_ref = sum(domain_reliable_dummy)/totalnewscount_post_All_ref,
            Prop_Unreliable_NewsG_Score_post_All_ref = sum(domain_unreliable_dummy)/totalnewscount_post_All_ref)

pulse_agg <- as_tibble(pulse_agg)
names(pulse_agg)[1] <- "caseid"

write_csv(pulse_agg, "Data/pulse_vars_post_ref_All.csv")


mean(pulse_agg$Prop_Unreliable_NewsG_Score_post_All_ref)
mean(pulse_agg$Average_domain_NewsG_Score_post_All_ref,na.rm=T)

pulse_sum <- merge(pulse_sum,pulse_agg,by='caseid')

# need to merge in treatment indicator, survey weight

w1 <- read_csv("Data/NYUU0017_w1_OUTPUT.csv")
w2 <- read_csv("Data/NYUU0017_w2_OUTPUT.csv")

# select only relevant URLs
pulse_subset <- desktop_SM[desktop_SM$used_at > as.Date("2020-06-30"),]

# left join with start dates
w1 <- left_join(w1, select(w2, caseid = caseid_w1, caseid_pulse = caseid))


# left join with start dates
#pulse_subset <- lazy_dt(pulse_subset)
pulse_subset <- left_join(pulse_subset, select(w1, caseid = caseid_pulse, caseid_w1 = caseid, starttime))
pulse_subset <- left_join(pulse_subset, select(mediascores, domain, zeta))
pulse_subset <- as_tibble(pulse_subset)

# keep only visits after treatment
pulse_subset <- pulse_subset[pulse_subset$used_at > pulse_subset$starttime,]

#Amount of time consuming news with a newsguard score

head(pulse_subset)


# aggregate for each person
pulse_agg <- pulse_subset %>% 
  group_by(caseid_w1) %>% 
  # compute relevant metrics
  summarize(totalnewsbinary_post_SM_ref = max(visit_ng),
            totalnewscount_post_SM_ref = sum(visit_ng),
            log_news_post_SM_ref = log(totalnewscount_post_SM_ref),
            
            Average_domain_NewsG_Score_post_SM_ref = mean(domain_credibility_score, na.rm = TRUE),
            Count_Reliable_NewsG_Score_post_SM_ref =  log10(sum(domain_reliable_dummy)+1),
            Count_Unreliable_NewsG_Score_post_SM_ref = log10(sum(domain_unreliable_dummy)+1),
            Prop_Reliable_NewsG_Score_post_SM_ref = sum(domain_reliable_dummy)/totalnewscount_post_SM_ref,
            Prop_Unreliable_NewsG_Score_post_SM_ref = sum(domain_unreliable_dummy)/totalnewscount_post_SM_ref)

pulse_agg <- as_tibble(pulse_agg)
names(pulse_agg)[1] <- "caseid"

write_csv(pulse_agg, "Data/pulse_vars_post_ref_SM.csv")


mean(pulse_agg$Prop_Unreliable_NewsG_Score_post_SM_ref)
mean(pulse_agg$Average_domain_NewsG_Score_post_SM_ref,na.rm=T)


pulse_sum <- merge(pulse_sum,pulse_agg,by='caseid')




write_csv(pulse_sum, "Data/pulse_vars_referral.csv")





# GROUP BY TREATMENT BELOW? -ag
p30 <- pulse %>% group_by() %>% summarize(pct_refer_by_fb = weighted.mean(refer_fb30, w = weight, na.rm = TRUE),
                                       pct_refer_by_goog = weighted.mean(refer_goog30, w = weight, na.rm = TRUE),
                                       pct_refer_by_tw = weighted.mean(refer_tw30, w = weight, na.rm = TRUE),
                                       pct_refer_by_email = weighted.mean(refer_email30, w = weight, na.rm = TRUE))
                                       # pct_refer_by_fb_se = sqrt(var(refer_fb30, na.rm = TRUE)),
                                       # pct_refer_by_goog_se = sqrt(var(refer_goog30, na.rm = TRUE)),
                                       # pct_refer_by_tw_se = sqrt(var(refer_tw30, na.rm = TRUE)))


library(tidyverse)
library(urltools)

# function to reduce URLs to canonical form
extract_canonical_urls <- function(urls){
  ytlinks <- grepl('youtube.com', urls)
  parameters(urls[!ytlinks]) <- NULL
  fragment(urls) <- NULL
  port(urls) <- NULL
  
  return(urls)
}

newsguard <- read_csv("Data/NewsGuard_Ratings.csv", col_types = list(Topics = col_character()))
trust_ng_domains <- unique(newsguard$Domain[which(newsguard$Rating == "T")])
untrust_ng_domains <- unique(newsguard$Domain[which(newsguard$Rating == "N")])
ng_domains <- unique(newsguard$Domain)

# Desktop ------------------------------------------------------------------

pulse_raw <- read_csv("Data/pulse_output/realityMine_web_2020-05-17_2020-07-14.csv")

# select only visits on desktop devices
desktop <- pulse_raw %>% filter(device_type == "Laptop/Desktop") %>%
  # keeping only relevant variables
  select(caseid, os_name, os_version,
         session_start_time, date, time, page_url_anonymized,
         page_domain, private_domain, predecessor_url_anonymized, succesor_url_anonymized,
         page_duration) %>% 
  # converting URLs to canonical URLs
  mutate(page_url = extract_canonical_urls(page_url_anonymized)) %>% 
  # removing SEQUENTIAL duplicates
  group_by(caseid, date) %>% 
  arrange(session_start_time) %>% 
  mutate(url_row = rep(cumsum(rle(page_url)$lengths), rle(page_url)$lengths),
         url_seq = 1:n()) %>% 
  ungroup() %>% 
  mutate(duplicate = ifelse(url_row != url_seq, 1, 0)) %>% 
  filter(duplicate == 0) %>% 
  select(-duplicate, -url_row, -url_seq)

# dates <- seq(as.Date("2018-09-01"), as.Date("2018-11-30"), by="day")

# Using web tracking data from before individuals are treated we analyze two weeks of web tracking data. 
# We count the number of news domains and the frequency of visits to create a value of online news consumption. 
# This is measured by the log(base10)+1 count of URLs from domains in the NewsGuard list and this value is assigned 
# to $News\_Cons\_Pulse_i$.

# cleaning data: 
desktop <- desktop %>% #filter(date >= (as.Date(dt)-2)) %>% 
    # cleaning page_domain variable
    mutate(page_domain = gsub("www.", "", page_domain)) %>% 
    # cleaning page_url variable
    mutate(page_url = gsub("https?://", "", page_url))
  
# adding visits to lib/cons/HP/FNC sites
desktop <- desktop %>% mutate(
    # visit_cons = ifelse(page_domain %in% cons_domains, 1, 0),
    # visit_lib = ifelse(page_domain %in% lib_domains, 1, 0),
    # visit_hp = ifelse(page_domain == "huffingtonpost.com", 1, 0),
    # visit_fn = ifelse(page_domain == "foxnews.com", 1, 0)
    visit_ng = ifelse(page_domain %in% ng_domains, 1, 0),
    domain_reliable_dummy = ifelse(page_domain %in% trust_ng_domains, 1, 0),
    domain_unreliable_dummy = ifelse(page_domain %in% untrust_ng_domains, 1, 0)
  )
  
#   # checking whether domain visit is news
# desktop <- desktop %>% mutate(
#     isNews = ifelse(page_domain %in% slant$domain, 1, 0)
#   ) %>% 
#     mutate(
#       isNews = ifelse(page_domain %in% c("amazon.com", "youtube.com", "twitter.com"), 0, isNews),
#       isNews = ifelse(page_domain %in% c("huffingtonpost.com", "foxnews.com"), 0, isNews)
#     )
  
# renaming domain variable
desktop <- desktop %>% mutate(domain = page_domain)
  
# dropping when domain is missing
desktop <- desktop %>% filter(!is.na(domain))
  
# dropping variables we will not use
desktop <- desktop %>% select(-os_version, -predecessor_url_anonymized, -succesor_url_anonymized)
  
library(dtplyr)
library(data.table)

# grab raw NewsGuard domain scores
newsguard <- newsguard[which(!duplicated(newsguard$Domain)),]
desktop_2 <- as.data.frame(desktop)

str(newsguard)



desktop_2 <- left_join(desktop_2, select(newsguard, domain = Domain, domain_credibility_score = Score))



desktop_3 <- as_tibble(desktop_2)
newsguard_2 <- as_tibble(newsguard)

save(desktop_3, file = "Data/pulse_desktop_clean.RData")




# Mobile ------------------------------------------------------------------

# pulse_mob <- read_csv("~/Downloads/pulse_output/realityMine_web_mobile_2020-05-17_2020-07-14.csv")

# select only visits on desktop devices
mobile <- pulse_raw %>% filter(device_type %in% c("Smartphone", "Tablet")) %>%
  # keeping only relevant variables
  select(caseid, os_name, os_version,
         session_start_time, date, time, page_url_anonymized,
         page_domain, private_domain, predecessor_url_anonymized, succesor_url_anonymized,
         page_duration) %>% 
  # converting URLs to canonical URLs
  mutate(page_url = extract_canonical_urls(page_url_anonymized)) %>% 
  # removing SEQUENTIAL duplicates
  group_by(caseid, date) %>% 
  arrange(session_start_time) %>% 
  mutate(url_row = rep(cumsum(rle(page_url)$lengths), rle(page_url)$lengths),
         url_seq = 1:n()) %>% 
  ungroup() %>% 
  mutate(duplicate = ifelse(url_row != url_seq, 1, 0)) %>% 
  filter(duplicate == 0) %>% 
  select(-duplicate, -url_row, -url_seq)

# dates <- seq(as.Date("2018-09-01"), as.Date("2018-11-30"), by="day")

# Using web tracking data from before individuals are treated we analyze two weeks of web tracking data. 
# We count the number of news domains and the frequency of visits to create a value of online news consumption. 
# This is measured by the log(base10)+1 count of URLs from domains in the NewsGuard list and this value is assigned 
# to $News\_Cons\_Pulse_i$.

# cleaning data: 
mobile <- mobile %>% #filter(date >= (as.Date(dt)-2)) %>% 
  # cleaning page_domain variable
  mutate(page_domain = gsub("www.", "", page_domain)) %>% 
  # cleaning page_url variable
  mutate(page_url = gsub("https?://", "", page_url))

# adding visits to lib/cons/HP/FNC sites
mobile <- mobile %>% mutate(
  # visit_cons = ifelse(page_domain %in% cons_domains, 1, 0),
  # visit_lib = ifelse(page_domain %in% lib_domains, 1, 0),
  # visit_hp = ifelse(page_domain == "huffingtonpost.com", 1, 0),
  # visit_fn = ifelse(page_domain == "foxnews.com", 1, 0)
  visit_ng = ifelse(page_domain %in% ng_domains, 1, 0),
  domain_reliable_dummy = ifelse(page_domain %in% trust_ng_domains, 1, 0),
  domain_unreliable_dummy = ifelse(page_domain %in% untrust_ng_domains, 1, 0)
)

#   # checking whether domain visit is news
# mobile <- mobile %>% mutate(
#     isNews = ifelse(page_domain %in% slant$domain, 1, 0)
#   ) %>% 
#     mutate(
#       isNews = ifelse(page_domain %in% c("amazon.com", "youtube.com", "twitter.com"), 0, isNews),
#       isNews = ifelse(page_domain %in% c("huffingtonpost.com", "foxnews.com"), 0, isNews)
#     )

# renaming domain variable
mobile <- mobile %>% mutate(domain = page_domain)

# dropping when domain is missing
mobile <- mobile %>% filter(!is.na(domain))

# dropping variables we will not use
mobile <- mobile %>% select(-os_version, -predecessor_url_anonymized, -succesor_url_anonymized)

# grab raw NewsGuard domain scores
mobile_2 <- as.data.frame(mobile)
mobile_2 <- left_join(mobile_2, select(newsguard, domain = Domain, domain_credibility_score = Score))
mobile_2 <- as_tibble(mobile_2)

save(mobile_2, file = "Data/pulse_mobile_clean.RData")



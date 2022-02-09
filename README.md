
# Title: News credibility labels have limited average effects on news diet quality and fail to reduce misperceptions
# Authors: Kevin Aslett, Andrew Guess, Jonathan Nagler, Richard Bonneau, and Joshua A. Tucker

# Abstract: 
As the primary arena for viral misinformation shifts toward transnational threats such as the Covid-19 pandemic, the search continues for scalable, lasting countermeasures compatible with principles of transparency and free expression. To advance scientific understanding and inform future interventions, we conducted a randomized field experiment evaluating the impact of source credibility labels embedded in users’ social feeds and search results pages. By combining representative surveys (N = 3,337) and digital trace data (N = 946) from a subset of respondents, we provide a rare ecologically valid test of such an intervention on both attitudes and behavior. On average across the sample, we are unable to detect changes in real-world consumption of news from low-quality sources after three weeks, and we can rule out even small effects on perceived accuracy of popular misinformation spread about the Black Lives Matter movement and Covid-19. However, we present analysis which suggests an increase in news diet quality among the heaviest consumers of misinformation in our sample. We discuss the implications of our findings for practical questions about designing interventions to counteract online misinformation.

## Instructions:

There are five directories:

### (1) Code: This directory holds 4 R files that produce clean the survey data and produce important figures and tables in the main document and the supplementary methods and materials:

#### (1.1) Clean_And_Merge_Data.R: Cleans the raw survey data and creates a clean survey datafile: "Clean_NewsGuard_Survey_Study.csv"

#### (1.2) Final_Figures_in_Main_Paper.R: Produces figures used in the main text.

#### (1.3) Final_Figures_Tables_in_Supplementary_Materials.R: Produces tables figures and tables for the supplementary materials and methods.

#### (1.4) functions.R: Creates some functions needed in the other R files.

### (2) Data: Contains all data needed to run the four R files in the "Code" directory, except for the NewsGuard scores, which we cannot make publicly available due to our licensing agreement with them.

#### (2.1) NYUU0017_w1_OUTPUT.csv: Raw survey data from the 1st wave survey.

#### (2.2) NYUU0017_w2_OUTPUT.csv: Raw survey data from the 2nd wave survey. 

#### (2.3) Compliance_Check_Data.csv: Compliance check data for the NewsGuard Web Extension

#### (2.4) Clean_NewsGuard_Digital_Trace_Data.csv: Combined Survey and Digital trace data from the desktops of respondents. 

#### (2.5) Clean_Mobile_Digital_Trace_Data.csv: Combined Survey and Digital trace data from the mobile devices of respondents. 

#### (2.6) pulse_vars_post.csv: Digital trace data from the desktops of respondents during the post-treatment period (July 1, 2020 - July 13, 2020)

#### (2.7) pulse_vars_dv.csv: Digital trace data from the desktops of respondents during the post-treatment period (day/time of extension installation - July 13)

#### (2.8) pulse_vars_pre.csv: Digital trace data from the desktops of respondents during the pre-treatment period (May 16, 2020 - day/time of extension installation). 

#### (2.9) pulse_vars_mobile_post.csv: Digital trace data from the mobile devices of respondents during the post-treatment period (July 1, 2020 - July 13, 2020)

#### (2.10) pulse_vars_mobile_dv.csv: Digital trace data from the mobile devices of respondents during the post-treatment period (day/time of extension installation - July 13)

#### (2.11) pulse_vars_mobile_pre.csv: Digital trace data from the mobile devices of respondents during the pre-treatment period (May 16, 2020 - day/time of extension installation). 

#### (2.12) pulse_vars_post_ref_SM.csv: Digital trace data from the desktops of respondents during the post-treatment period (July 1, 2020 - July 13, 2020). Only contains URL views that are referrals from social media platforms (Facebook and Twitter).

#### (2.13) pulse_vars_dv_ref_SM.csv: Digital trace data from the desktops of respondents during the post-treatment period (day/time of extension installation - July 13). Only contains URL views that are referrals from social media platforms (Facebook and Twitter).

#### (2.14) pulse_vars_pre_ref_SM.csv: Digital trace data from the desktops of respondents during the pre-treatment period (May 16, 2020 - day/time of extension installation). Only contains URL views that are referrals from social media platforms (Facebook and Twitter).

#### (2.15) pulse_vars_post_ref_All.csv: Digital trace data from the desktops of respondents during the post-treatment period (July 1, 2020 - July 13, 2020). Only contains URL views that are referrals from Google search engines and social media platforms (Facebook and Twitter).

#### (2.16) pulse_vars_dv_ref_All.csv: Digital trace data from the desktops of respondents during the post-treatment period (day/time of extension installation - July 13). Only contains URL views that are referrals from Google search engines and social media platforms (Facebook and Twitter).

#### (2.17) pulse_vars_pre_ref_All.csv: Digital trace data from the desktops of respondents during the pre-treatment period (May 16, 2020 - day/time of extension installation). Only contains URL views that are referrals from Google search engines  and social media platforms (Facebook and Twitter).

#### (2.18) pulsebyday.csv: Digital trace data from the desktops of respondents aggregated by day of the study.

#### (2.19) pulsebyday_mobile.csv: Digital trace data from the mobile devices of respondents aggregated by day of the study.

#### (2.20) Total_visit_data.csv: Google Search Engine visists, time spent on Facebook, time spent on Twitter, total visits to news sites with NewsGuard scores.

#### (2.21) Clean_NewsGuard_Survey_Study.csv: Cleaned survey data from wave 1 and wave 2.

### (3) Figures: All the figures produced by files int the "Code" directory are located here.

### (4) Tables: All the tables (in .txt files) produced by files in the "Code" directory are located here.

### (5) Codebooks: All the codebooks are located here for the raw survey data are located here: 

#### (5.1) NYUU0017_codebook.pdf

#### (5.2) NYUU0017_w2_codebook.pdf

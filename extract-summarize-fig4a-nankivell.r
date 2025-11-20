#Script goal: extract information from Figure 4a in
#and summarize in 6-month bins

#Data are located in this csv
#iMVI-incidence-by-dsa-fig4a-nankivell-csv-for-r

#I measured the inches using powerpoint.

# Load data-----
#Load data and then summarize by 6-month intervals

library(tidyverse)
getwd()
mvi_by_dsa_by_time_bin = read_csv("iMVI-incidence-by-dsa-fig4a-nankivell-csv-for-r.csv") %>% 
  mutate(
    #make an indicator for before or after 6 months.
    #4-6 months is the 6th row down
    row_index=row_number(),
    before_six_mos=case_when(
      row_index<=6~1,
      TRUE~0 #else
    )
  )

# Overall numbers-----
#what do these add up to? It should be 202.
#If not exactly, that's okay. The ratio is what matters
totals_over_follow_up=mvi_by_dsa_by_time_bin %>% 
  mutate(total=1) %>% 
  group_by(total) %>% 
  summarise(
    #use the _tot subscript to differentiate from the below
    #stratum-specific totals  
    n_tot_mvi_gt2=sum(total_mvi_gt2),
    n_tot_dsa_neg_blue=sum(dsa_neg_blue),
    n_tot_dsa_pos_red=sum(dsa_pos_red)) %>% 
  ungroup() %>% 
  mutate(
    #proportion DSA-negative of total
    #Also available from Figure 1
    prop_tot_dsa_neg=n_tot_dsa_neg_blue/n_tot_mvi_gt2,
    prop_tot_imvi=prop_tot_dsa_neg*(50/70),
  ) %>% 
  dplyr::select(-total)

totals_over_follow_up

#okay, 195. almost

#what's the overall ratio of dsa-neg to dsa-pos?


#sum number of cases by the six-month
mvi_by_dsa_by_six_mos=mvi_by_dsa_by_time_bin %>% 
  group_by(before_six_mos) %>% 
  summarise(
    n_mvi_gt2=sum(total_mvi_gt2,na.rm=T),
    n_dsa_pos_red=sum(dsa_pos_red,na.rm=T),
    n_dsa_neg_blue=sum(dsa_neg_blue,na.rm=T)
  ) %>% 
  #now calculate the proportion DSA-negative before and after six months
  ungroup() %>% 
  
  #sort it so before six months is first
  #for calculations below
  arrange(desc(before_six_mos)) %>% 
  mutate(
    prop_dsa_neg=n_dsa_neg_blue/n_mvi_gt2,
    
    #From Figure 1, of the 70 total DSA-neg, 50 are C4d-negative,
    #which is the official dx of iMVI.
    #So we could assume a constant proportion of 50/70 as c4d-negative
    #though it doesn't matter because the relative share of DSA-neg before
    #and after 6 months is the critical piece. if c4d-negative share is the same,
    #it won't change the ratio.
    #This is also an assumption and may not hold
    prop_imvi=prop_dsa_neg*(50/70),
    
    #now, let's calculate the relative ratio of dsa-negative before six months
    #compared with after
    #Look down the dataset using lead
    ratio_dsa_neg_before_six_mos=prop_dsa_neg/lead(prop_dsa_neg)
  ) %>% 
  #now, append the overall totals over follow-up
  #and calculate relatively how much higher or lower the dsa-negative share
  #is before and after six months
  bind_cols(totals_over_follow_up) %>% 
  mutate(
    ratio_dsa_neg_vs_total=prop_dsa_neg/prop_tot_dsa_neg,
    #should be the same, but check
    ratio_imvi_vs_total=prop_imvi/prop_tot_imvi
  )
  
  

mvi_by_dsa_by_six_mos %>% View()
#So the answers:
mvi_by_dsa_by_six_mos %>% 
  filter(before_six_mos==1) %>% 
  dplyr::select(ratio_dsa_neg_vs_total) %>% 
  pull()

#Before 6 months, multiply the ratio from Sablik by 1.4
#After Sablik, multiply the ratio from Sablik by 0.6178514
mvi_by_dsa_by_six_mos %>% 
  filter(before_six_mos==0) %>% 
  dplyr::select(ratio_dsa_neg_vs_total) %>% 
  pull()


#write this to a CSV for easier pasting into Word.
write.csv(mvi_by_dsa_by_six_mos,"mvi_by_dsa_by_six_mos.csv")

mvi_by_dsa_by_six_mos %>% View()
  

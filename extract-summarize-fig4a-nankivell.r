#Script goal: extract information from Figure 4a in
#and summarize in 6-month bins

#Data are located in this csv
#iMVI-incidence-by-dsa-fig4a-nankivell-csv-for-r

#I measured the inches using powerpoint.

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

#what do these add up to? It should be 202.
#If not exactly, that's okay. The ratio is what matters
mvi_by_dsa_by_time_bin %>% 
  mutate(total=1) %>% 
  group_by(total) %>% 
  summarise(total_mvi_gt2=sum(total_mvi_gt2))

#okay, 195. almost


#sum number of cases by the six-month
mvi_by_dsa_by_six_mos=mvi_by_dsa_by_time_bin %>% 
  group_by(before_six_mos) %>% 
  summarise(
    total_mvi_gt2=sum(total_mvi_gt2,na.rm=T),
    dsa_pos_red=sum(dsa_pos_red,na.rm=T),
    dsa_neg_blue=sum(dsa_neg_blue,na.rm=T)
  ) %>% 
  #now calculate the proportion DSA-negative before and after six months
  ungroup() %>% 
  
  #sort it so before six months is first
  #for calculations below
  arrange(desc(before_six_mos)) %>% 
  mutate(
    dsa_neg_prop=dsa_neg_blue/total_mvi_gt2,
    
    #From Figure 1, of the 70 total DSA-neg, 50 are C4d-negative,
    #which is the official dx of iMVI.
    #So we could assume a constant proportion of 50/70 as c4d-negative
    #though it doesn't matter because the relative share of DSA-neg before
    #and after 6 months is the critical piece. if c4d-negative share is the same,
    #it won't change the ratio
    imvi_prop=dsa_neg_prop*(50/70),
    
    #now, let's calculate the relative ratio of dsa-negative before six months
    #compared with after
    #Look down the dataset using lead
    ratio_dsa_neg_before_six_mos=dsa_neg_prop/lead(dsa_neg_prop)
  )
  

#So the answer is that iMVI is 2.285932 as likely before six months
#as after

#write this to a CSV for easier pasting into Word.
write.csv(mvi_by_dsa_by_six_mos,"mvi_by_dsa_by_six_mos.csv")

mvi_by_dsa_by_six_mos %>% View()
  

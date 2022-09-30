###
### this file computes the correlations between skills and performance (various indicators)

# correlation: skills and total points
sk_pt <- summarised %>% 
  group_by(repetition, IPregime) %>% 
  group_modify(~tidy(cor.test(.$skill, .$points))) %>% 
  mutate(correlation = "skills and total points") %>% 
  select(correlation, repetition, IPregime, r = estimate, p.value)

# correlation: skills and net royalties
sk_netroy <- summarised %>% 
  group_by(repetition, IPregime) %>% 
  group_modify(~tidy(cor.test(.$skill, .$royalties)))%>% 
  mutate(correlation = "skills and net royalties") %>% 
  filter(!is.na(estimate)) %>% 
  select(correlation, repetition, IPregime, r = estimate, p.value)

# correlation: skills and incoming royalties
sk_royin <- summarised %>% 
  group_by(repetition, IPregime) %>% 
  group_modify(~tidy(cor.test(.$skill, .$royalties_in)))%>% 
  mutate(correlation = "skills and royalty inflow") %>% 
  filter(!is.na(estimate)) %>% 
  select(correlation, repetition, IPregime, r = estimate, p.value)

# export table
bind_rows(sk_pt, sk_netroy, sk_royin) %>% 
  select(-correlation) %>% 
  mutate(r = round(r,3), p.value = round(p.value, 3)) %>% 
  kable(caption = "Correlations of skills and performance", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  pack_rows(start_row = 1, end_row = 3, group_label = "Skills and points") %>% 
  pack_rows(start_row = 4, end_row = 5, group_label = "Skills and net royalties") %>% 
  pack_rows(start_row = 6, end_row = 7, group_label = "Skills and incoming royalties") %>% 
  save_kable("Tables/Correlations_skills_performance.pdf")

##cleanup
rm(sk_pt, sk_royin, sk_netroy)

####
####
#### Generating group transition by vote statistics and tests (section 4.2) 

# stats
stats_by_group <- groupdiff %>% 
  group_by(IPregime, vote) %>% 
  summarise(N_groups = n(), mean_change = mean(transition), perc_change = 100*(mean(`2`) - mean(`1`))/mean(`1`) )

# tests
tests <- groupdiff %>% 
  group_by(IPregime, vote) %>% 
  group_modify(~tidy(t.test(.$transition))) %>% 
  select(IPregime, vote, t_stat = statistic, p.value)

# cohens' d
cohens <- grouped %>% 
  group_by(groupID) %>% 
  mutate(IPregime = last(IPregime)) %>% 
  group_by(IPregime, vote) %>% 
  group_modify(~as.data.frame(cohen.d(points~repetition, data = .)$estimate)) %>% 
  rename(cohens_d = `cohen.d(points ~ repetition, data = .)$estimate`)

stats_out <- stats_by_group %>% 
  left_join(tests) %>% 
  left_join(cohens)

stats_out %>% 
  kable(caption = "Group points statistics and tests -- by vote and final IP regime", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("Tables/Group_points_vote.pdf")

## cleanup
rm(cohens, grouplastIP, stats, stats_by_group, stats_out, tests)


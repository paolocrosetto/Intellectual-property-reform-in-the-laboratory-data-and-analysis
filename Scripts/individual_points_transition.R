####
####
#### Generating individual point statistics and tests (section 4.1) 

indiv_points <- summarised %>% 
  group_by(subjectID) %>% 
  mutate(IPregime = last(IPregime)) %>% 
  select(subjectID, IPregime, repetition, points) %>% 
  spread(repetition, points) %>% 
  mutate(transition = `2` - `1`) 

indiv_stats <- indiv_points %>% 
  group_by(IPregime) %>% 
  summarise(N_groups = n(), mean_change = mean(transition), perc_change = 100*(mean(`2`) - mean(`1`))/mean(`1`) )

indiv_test <- indiv_points %>% 
  group_by(IPregime) %>% 
  group_modify(~tidy(t.test(.$transition))) %>% 
  select(IPregime, t_stat = statistic, p.value)

indiv_stats %>%
  left_join(indiv_test,  by = "IPregime") %>% 
  kable(caption = "Individual-level points statistics and tests", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("Tables/Individual_points_statistics.pdf")

## cleaning up
rm(indiv_points, indiv_stats, indiv_test)


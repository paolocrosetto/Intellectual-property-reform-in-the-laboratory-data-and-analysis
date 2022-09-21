###
###
### Analysis of the impact of vote outcomes on points, individual-level

stats <- summarised %>% 
  group_by(subjectID) %>% 
  mutate(IPregime = last(IPregime)) %>% 
  filter(IPregime == "noIP") %>% 
  select(points, repetition, vote) %>% 
  spread(repetition, points) %>% 
  mutate(diff = `2` - `1`) %>% 
  ungroup() %>% 
  group_modify(~tidy(t.test(.$diff~.$vote)))

stats %>% 
  kable(caption = "Individual points statistics and tests -- by vote and final IP regime", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("Tables/Individual_points_vote.pdf")

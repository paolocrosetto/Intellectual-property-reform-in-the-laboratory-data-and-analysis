####
####
#### Generating group point statistics and tests (section 4.1)

# generating metrics by group
groupdiff <- grouped %>% 
  select(groupID, vote, info, repetition, IPregime, points, skills, luck, greed, royaversion) %>% 
  ungroup() %>% 
  mutate(groupID = as.factor(groupID)) %>% 
  group_by(groupID) %>% 
  mutate(IPregime = last(IPregime)) %>%
  mutate(luckdiff = last(luck) - luck) %>% 
  mutate(luckdiff = first(luckdiff)) %>% 
  select(-luck) %>% 
  spread(repetition, points) %>% 
  mutate(transition = `2` - `1`)

# statistics by IP regime
stats_by_group <- groupdiff %>% group_by(IPregime) %>% 
  summarise(N_groups = n(), mean_change = mean(transition), perc_change = 100*(mean(`2`) - mean(`1`))/mean(`1`) )

# statistics for control only
stats_control <- groupdiff %>% 
  filter(vote == 'NONVOTE') %>% 
  group_by(IPregime) %>% 
  summarise(N_groups = n(), mean_change = mean(transition), perc_change = 100*(mean(`2`) - mean(`1`))/mean(`1`) ) %>% 
  mutate(IPregime = "Control")
  
# merging the stats

stats_by_group <- stats_by_group %>% 
  bind_rows(stats_control)

# t-test by IP regime
tests_by_group <- groupdiff %>% 
  group_by(IPregime) %>% 
  group_modify(~tidy(t.test(.$transition))) %>% 
  select(IPregime, t_stat = statistic, p.value)

# t-test for control only
tests_control <- groupdiff %>% 
  filter(vote == "NONVOTE") %>% 
  ungroup() %>% 
  group_modify(~tidy(t.test(.$transition))) %>% 
  mutate(IPregime = "Control") %>% 
  select(IPregime, t_stat = statistic, p.value)

# binding the tests
tests_by_group <- tests_by_group %>% 
  bind_rows(tests_control)

# cohen's d by IP regime


# ancillary dataset where only the "last" IP regime (second repetition) is taken into account
grouped %>% 
  group_by(groupID) %>% 
  mutate(IPregime = last(IPregime)) -> grouplastIP

## just those transitioning to IP
d_ip <- cohen.d(points~repetition, data = grouplastIP %>% filter(IPregime == "IP"))$estimate

## just those transitioning to noIP
d_noip <- cohen.d(points~repetition, data = grouplastIP %>% filter(IPregime == "noIP"))$estimate

## just control
d_control <- cohen.d(points~IPregime, data = grouped %>% filter(vote == "NONVOTE"))$estimate


cohens <- tibble(IPregime = c("IP", "noIP", "Control"), cohens_d = c(d_ip, d_noip, d_control))


point_transitions <- stats_by_group %>%
  left_join(tests_by_group,  by = "IPregime") %>% 
  left_join(cohens, by = "IPregime")

point_transitions %>% 
  kable(caption = "Group points statistics and tests", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("Tables/Group_points_statistics.pdf")

## cleaning up
rm(d_ip, d_noip, d_control, 
   tests_by_group, tests_control, stats_by_group, 
   stats_control, point_transitions, cohens, grouplastIP)
  

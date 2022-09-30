###
###
### This file merges the extendibility data with actions
###
### To check the referees' concern that "best" players were actually forward looking
### i.e., creating roots that had high potential. 


### data
ext <- read_csv("Data/extendibility.csv") %>% 
  filter(dictionary == "French") %>% 
  select(new_word = root, extendibility)

# merging with the actions dataset
actions <- actions %>% 
  left_join(ext, by = "new_word")

# merging with the summarised dataset
ext <- actions %>% 
  select(subjectID, extendibility) %>% 
  group_by(subjectID) %>% 
  summarise(ext = mean(extendibility, na.rm = T)) %>% 
  left_join(summarised)


## correlations
cordf <- ext %>% 
  filter(!is.nan(ext)) %>% 
  select(subjectID, IPregime, repetition, skill, ext, points, votechr, royalties_in, royalties) %>% 
  distinct()

# does EPI correlate with points?
epi_pt <- cordf %>% 
  group_by(IPregime, repetition) %>% 
  group_modify(~tidy(cor.test(.$ext, .$points))) %>% 
  mutate(type = "epi-point")

# with roy in? 
epi_royin <- cordf %>% 
  group_by(IPregime, repetition) %>% 
  group_modify(~tidy(cor.test(.$ext, .$royalties_in))) %>% 
  mutate(type = "epi-royin")

# with skills in initial game?
epi_skills <- cordf %>% 
  group_by(IPregime, repetition) %>% 
  select(subjectID, ext, skill) %>% 
  distinct() %>% 
  group_modify(~tidy(cor.test(.$ext, .$skill))) %>% 
  mutate(type = "epi-skills")


# exporting correlation table
bind_rows(epi_pt, epi_royin, epi_skills) %>% 
  select(type, IPregime, repetition, r = estimate, p.value) %>% 
  filter(!is.na(r)) %>% 
  select(-type) %>% 
  mutate(r = round(r,3), p.value = round(p.value, 3)) %>% 
  kable(caption = "Correlations of extendibility potential with skills and performance", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  pack_rows(start_row = 1, end_row = 3, group_label = "EPI and points") %>% 
  pack_rows(start_row = 4, end_row = 5, group_label = "EPI and incoming royalties") %>% 
  pack_rows(start_row = 6, end_row = 8, group_label = "EPI and skills") %>% 
  save_kable("Tables/Correlations_Extendibility_skills_performance.pdf")

# does EPI differ by treatment?
treat <- cordf %>% 
  filter(repetition == 2) %>% 
  group_modify(~tidy(t.test(ext~IPregime, data = .)))

# does EPI differ by vote?
vot <- cordf %>% 
  filter(repetition == 1) %>% 
  filter(votechr != "novote") %>% 
  group_modify(~tidy(t.test(ext~votechr, data = .)))

# exporting t-test of differences
rbind(treat, vot) %>% 
  select(estimate1, estimate2, p.value) %>% 
  mutate(estimate1 = round(estimate1, 2),
         estimate2 = round(estimate2, 2),
         p.value = round(p.value, 3)) %>% 
  kable(caption = "EPI by treatment and vote", format = "latex", booktabs = "T", col.names = c("", "", "p.value")) %>% 
  kable_styling(latex_options = "scale_down") %>% 
  pack_rows(start_row = 1, end_row = 1, group_label = "EPI, vote vs novote") %>% 
  pack_rows(start_row = 2, end_row = 2, group_label = "EPI, keep IP vs switch to noIP") %>% 
  save_kable("Tables/EPI_by_treatment.pdf")

# cleanup
rm(epi_pt, epi_royin, epi_skills, ext, cordf, vot, treat)

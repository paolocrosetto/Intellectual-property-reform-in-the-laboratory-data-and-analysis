###
### computing correlations between skills and voting

# skills and vote
mean_test <- summarised %>% 
  filter(vote == "VOTE" & repetition == 1) %>% 
  select(subjectID, Npoints, votechr) %>% 
  group_modify(~tidy(t.test(Npoints~votechr, data = .))) %>% 
  select(IP = estimate1, noIP = estimate2, p.value) %>% 
  mutate(IP = round(IP,2), noIP = round(noIP, 2), p.value = round(p.value, 3))

cohens <- cohen.d(Npoints~votechr, data = summarised %>% filter(vote == "VOTE" & repetition == 2))$estimate

mean_test %>% 
  bind_cols("cohens_d" = cohens) %>% 
  mutate(cohens_d = round(cohens_d, 3)) %>% 
  kable(caption = "Points in preliminary task by vote", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("Tables/Skills_and_vote.pdf")

# skills and net royalty gain
mean_test <- summarised %>% 
  filter(vote == "VOTE" & repetition == 1) %>% 
  select(subjectID, royalties, votechr) %>% 
  group_modify(~tidy(t.test(royalties~votechr, data = .))) %>% 
  select(IP = estimate1, noIP = estimate2, p.value) %>% 
  mutate(IP = round(IP,2), noIP = round(noIP, 2), p.value = round(p.value, 3))

cohens <- -cohen.d(royalties~votechr, data = summarised %>% filter(vote == "VOTE" & repetition == 1))$estimate


mean_test %>% 
  bind_cols("cohens_d" = cohens) %>% 
  mutate(cohens_d = round(cohens_d, 3)) %>% 
  kable(caption = "Net royalty in repettion 1 by vote", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("Tables/Royalty_balance_and_vote.pdf")


# skills in repetition 3 across groups with IP and noIP
mean_test <- summarised %>% 
  filter(vote == "VOTE") %>% 
  filter(repetition == 2) %>% 
  select(skill, IPregime) %>% 
  ungroup() %>% 
  group_modify(~tidy(t.test(skill~IPregime, data = .))) %>% 
  select(IP = estimate1, noIP = estimate2, p.value) %>% 
  mutate(IP = round(IP,2), noIP = round(noIP, 2), p.value = round(p.value, 3))

cohens <- -cohen.d(skill~IPregime, data = summarised %>% filter(vote == "VOTE" & repetition == 2))$estimate


mean_test %>% 
  bind_cols("cohens_d" = cohens) %>% 
  mutate(cohens_d = round(cohens_d, 3)) %>% 
  kable(caption = "Normalized skills in repetiton 2 by IP regime", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("Tables/Skills_rep2_by_IPregime.pdf")

rm(mean_test, cohens)


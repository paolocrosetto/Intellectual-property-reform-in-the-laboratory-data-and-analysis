### compurint vote shares and vote results
###
### by treatment

# compute vote shares
votes <- summarised %>% 
  select(subjectID, votechr, info) %>% 
  filter(votechr != "novote") %>% 
  group_by(info, votechr) %>% 
  summarise(N = n()) %>% 
  mutate(tot = sum(N), 
         share = round(100*N/tot,2)) %>% 
  select(info, votechr, share) %>% 
  pivot_wider(names_from = votechr, values_from = share)


tablevote <- summarised %>% 
  select(subjectID, votechr, info) %>% 
  select(subjectID, votechr, info) %>% 
  filter(votechr != "novote")

test <- fisher.test(table(tablevote$info, tablevote$votechr)) %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 4)) %>% 
  select(`Fisher exact p-value` = p.value)

# export table
votes %>% 
  cbind(test) %>% 
  kable(caption = "Voting results by treatment", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("Tables/Vote_shares.pdf")

# compute number of losers
losers <- summarised %>% 
  filter(repetition == 2) %$%
  table(.$info, .$voteresult) %>% 
  as.data.frame() %>% 
  rename(treatment = Var1, result = Var2) %>% 
  filter(result != "novote") %>% 
  group_by(treatment) %>% 
  mutate(tot = sum(Freq)) %>% 
  arrange(treatment)

# export table
losers %>% 
  kable(caption = "Number of subjects voting for IP and losing the vote", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("Tables/Vote_losers.pdf")

## number of losers by group
losers_group <- summarised %>% 
  filter(vote == "VOTE") %>% 
  select(subjectID, groupID, votechr, info, IPregime, repetition) %>% 
  group_by(subjectID) %>% 
  mutate(IPregime = last(IPregime)) %>% 
  filter(repetition == 2) %>% 
  mutate(votenum = if_else(votechr == "IP", 1, 0)) %>% 
  group_by(groupID, info, IPregime) %>% 
  summarise(votedIP = sum(votenum)) %>% 
  group_by(IPregime, info, votedIP) %>% 
  tally() %>% 
  spread(votedIP, n) %>% 
  filter(IPregime == "noIP") %>% 
  select(-`3`)

# export table
losers_group %>% 
  kable(caption = "Number of groups with a given number of losers", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  add_header_above(c("", "", "# of losers" = 3)) %>% 
  save_kable("Tables/Vote_losers_by_group.pdf")

# cleanup
rm(votes, tablevote, test, losers, losers_group)
  
  

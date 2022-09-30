### compurint vote shares
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

# cleanup
rm(votes, tablevote, test)
  
  

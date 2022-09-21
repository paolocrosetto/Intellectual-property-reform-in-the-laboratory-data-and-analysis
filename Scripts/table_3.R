####
####
#### this file recreates Table 3 of the paper

### regression results, by groups and at the individual level, of the transition gain by voting condition

group_reg <- groupdiff %>% 
  filter(IPregime == "noIP") %>% 
  lm(transition~vote + skills, data = .)

individ_reg <- summarised %>% 
  group_by(subjectID) %>% 
  mutate(IPregime = last(IPregime)) %>% 
  filter(IPregime == "noIP") %>% 
  select(points, repetition, vote, skill) %>% 
  spread(repetition, points) %>% 
  mutate(diff = `2` - `1`) %>% 
  rename(skills = skill) %>% 
  lm(diff ~ vote + skills, data = .)


huxreg("Group data" = group_reg, "Individual data" = individ_reg,
       coefs = c("No vote" = "(Intercept)",
                 "Vote" = "voteVOTE",
                 "Word skills" = "skills"),
       stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, `°` = 0.1),
       error_pos = 'same', number_format = 2) %>% 
  quick_pdf(file = "Tables/Table_3.pdf")

## cleaning up
rm(individ_reg, group_reg)

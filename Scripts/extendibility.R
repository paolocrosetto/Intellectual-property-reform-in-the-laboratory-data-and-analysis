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

actions <- actions %>% 
  left_join(ext, by = "new_word")

ext <- actions %>% 
  select(subjectID, extendibility) %>% 
  group_by(subjectID) %>% 
  summarise(ext = mean(extendibility, na.rm = T)) %>% 
  left_join(summarised)

ext %>% filter(repetition == 1) %>% 
  ggplot() +
  aes(x=votechr, y = ext)+
  stat_summary()

ext %>% 
  select(subjectID, skill, ext) %>% 
  distinct() %>% 
  ggplot(aes(skill, ext)) +
  geom_point()+
  geom_smooth(method = "lm")


## correlations
cordf <- ext %>% 
  filter(!is.nan(ext)) %>% 
  select(subjectID, IPregime, repetition, skill, ext, points, votechr, royalties_in, royalties) %>% 
  distinct()

# does EPI correlate with points?
cordf %>% 
  group_by(IPregime, repetition) %>% 
  group_modify(~tidy(cor.test(.$ext, .$points)))

# with roy in? 
cordf %>% 
  group_modify(~tidy(cor.test(.$ext, .$royalties_in)))

# with skills in initial game?
cordf %>% 
  select(subjectID, ext, skill) %>% 
  distinct() %>% 
  group_modify(~tidy(cor.test(.$ext, .$skill)))

# does EPI differ by IPregime?
lm(ext~IPregime*repetition, data = cordf) %>% 
  tidy() %>% 
  mutate(p.value = round(p.value, 3))

# does EPI differ by treatment?
cordf %>% 
  filter(repetition == 2) %>% 
  group_modify(~tidy(t.test(ext~IPregime, data = .)))

# does EPI differ by vote?
cordf %>% 
  filter(repetition == 1) %>% 
  filter(votechr != "novote") %>% 
  group_modify(~tidy(t.test(ext~votechr, data = .)))

# does EPI across control and voting?
cordf %>% 
  filter(repetition == 1) %>% 
  group_by(votechr) %>% 
  summarise(m = mean(ext))
  group_modify(~tidy(t.test(ext~novote, data = .)))

data <- ext %>% 
  select(subjectID, points, skill, ext, luck, greed, royalty_aversion, votechr) %>% 
  filter(!is.nan(ext)) %>% 
  distinct()

data %>% 
  filter(!is.nan(ext)) %>% 
  psych::pairs.panels()

data %>% 
  group_by(votechr) %>% 
  summarise(mean(ext))

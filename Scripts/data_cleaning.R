###
###
###


### Data cleaning

### this file generates 3 main datasets: {summarised}, {grouped} and {actions}.

### 1. [summarised]: a dataset summarising the final results for each player in the experiment; 
### 2. [grouped]: a group_level summary of dataset 1.
### 3. [actions]: a dataset with one row per period per player, detailing actions and the players' choiceset and options


### TODO: merge extendibility with actions.

### summarised data
summarised <- read_csv("Data/summary_dataset.csv")

summarised <- summarised %>% 
  mutate(royalties = royalties_in - royalties_out) %>% 
  mutate(royration = royalties/points)

### individual choicesets
choicesets <- read_csv("Data/choicesets.csv")


### period-level data
actions <- read_csv("Data/round_level_data.csv")

# merging actions with choiceset
actions <- actions %>% left_join(choicesets, by = c("subjectID", "repetition", "period"))

# adding group ID to actions
actions <- actions %>% mutate(groupID = sessionID*10 + group) 

# adding group id to summary
groups <- actions %>% select(subjectID, groupID) %>% distinct()

summarised <- summarised %>% left_join(groups, by = "subjectID")

# adding treatment (VOTE and INFO conditions) to actions
treatment <- summarised %>% select(subjectID, votetreat = vote, info) %>% distinct()
actions <- actions %>% left_join(treatment, by = "subjectID")


## adding data from the intial word task
wordtask <- read_csv("Data/word_task_data.csv")

summarised <- summarised %>% left_join(wordtask, by="subjectID")
actions <- actions %>% left_join(wordtask, by = "subjectID")

##cleaning
rm(choicesets,groups, treatment, wordtask)



## computing skills, luck, greed, royalty aversion of individuals


#indicator of **luck**
luck <- actions %>% 
  select(subjectID, repetition, period, meancs) %>% 
  filter(period == 1) %>% 
  group_by(repetition) %>% 
  select(subjectID, init_potential = meancs) %>% 
  mutate(luck = 100*init_potential/max(init_potential, na.rm = T)) %>%  #make the indicator relative to best, 0-100
  select(subjectID, luck)

# indicator of **royalty_aversion**
roy_aversion <- actions %>% 
  mutate(extension = str_length(letters_added) != 3) %>% 
  filter(extension == TRUE) %>% 
  select(subjectID, repetition, period, payoff, royalties_spent) %>% 
  mutate(roy_spent_share = royalties_spent / (payoff + royalties_spent)) %>% 
  filter(repetition == 1) %>% 
  group_by(subjectID) %>% 
  summarise(royalty_aversion = 100*(1 - mean(roy_spent_share)))

# voting

skill_greed <- summarised %>% 
  filter(repetition == 1) %>% 
  mutate(skill = 100*Npoints/max(Npoints, na.rm = T)) %>% 
  select(subjectID, skill, greed = percasked)

pers_charac <- skill_greed %>% 
  left_join(roy_aversion) %>% 
  left_join(luck)


## merging with all concerned datasets

summarised <- summarised %>% left_join(pers_charac)
actions <- actions %>% left_join(pers_charac)

rm(pers_charac, roy_aversion, skill_greed, luck)



## group-level data
grouped <- actions %>% 
  group_by(groupID, IPregime, vote = votetreat, info, repetition) %>%
  mutate(wordvalue = payoff + royalties_spent,
         roundpayoff = payoff + royalties_earned) %>% 
  summarise(points = sum(roundpayoff, na.rm = T),
            skills = mean(skill),
            royaversion = mean(royalty_aversion),
            luck = mean(luck),
            greed = mean(greed),
            Nw = sum(!is.na(new_word)),
            Next = sum(str_length(letters_added)!= 3, na.rm = T),
            Nroot = sum(str_length(letters_added)== 3, na.rm = T),
            ExtRoot = Next/Nroot,
            value = mean(wordvalue, na.rm = T),
            length = mean(str_length(new_word), na.rm = T),
            mincs = mean(mincs, na.rm = T),
            meancs = mean(meancs, na.rm = T),
            maxcs = mean(maxcs, na.rm = T)
  )
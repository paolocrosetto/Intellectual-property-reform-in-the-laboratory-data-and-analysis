###
###
###


### Data cleaning

### this file generates 3 main datasets:

### 1. a dataset summarising the final results for each player in the experiment; 
### 2. a group_level summary of dataset 1.
### 3. a dataset with one row per period per player, detailing actions and the players' choiceset and options


### summarised data
summary <- read_csv("Data/summary_dataset.csv")

df <- df %>% 
  mutate(royalties = royalties_in - royalties_out) %>% 
  mutate(royration = royalties/points)

### importing the choiceset dataset, cleaning and merging
cs1 <- read_csv("choiceset_computed_0_4500.csv")
cs2 <- read_csv("choiceset_computed_4500_end.csv")
csbug <- read_csv("choiceset_computed_NA.csv")

### there is one line present in both datasets, deleting it from cs2
cs2 <- cs2 %>% filter(...1 != 4499)

### adding rows from the two distinct datasets
cs <- bind_rows(cs1,cs2)

## correcting the hand = NA bug
## recap: sometmes people had letters N and A in their letter set; in those cases, R + python treat this as missing (NA) while it is a string ("NA")
## this was fixed by re-running the choiceset script for those 14 lines. 
## so now we just need to insert it here

## 1. remove from old dataset
cs <- cs %>% filter(!is.na(hand))

## 2. import and bind_row
cs <- bind_rows(cs, csbug)


### cleaning from unneeded vraibels
cs <- cs %>% select(-...1, -choice, -hand)



## reading in main data
md1 <- read_csv("scrabble_raw.csv")
md2 <- read_csv("scrabble_raw2019.csv")

md <- bind_rows(md1, md2)

# merging with choiceset
md <- md %>% left_join(cs, by = c("subjectID", "repetition", "period"))

# adding group ID
md <- md %>% mutate(groupID = sessionID*10 + group) 

groups <- md %>% select(subjectID, groupID) %>% distinct()

df <- df %>% left_join(groups, by = "subjectID")

# adding VOTE and INFO conditions to subjects in md
treat <- df %>% select(subjectID, votetreat = vote, info) %>% distinct()
md <- md %>% left_join(treat, by = "subjectID")

##cleaning
rm(cs1, cs2, md1, md2, df2019, csbug, cs, groups, treat)

## adding data from the intial word task
#integrating the wordtask data
wt <- read_csv("wordtask.csv")
wt19 <- read_csv("wordtask2019.csv")
wt <- rbind(wt, wt19)
df <- df %>% left_join(wt, by="subjectID")
rm(wt, wt19)
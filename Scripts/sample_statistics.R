### generates sample statistics (section "Experimental procedures") 

### age and gender 

## data
survey <- read_csv("Data/final_survey.csv")

## age
age <- survey %>% 
  select(age) %>% 
  summarise(mean_age = mean(age, na.rm = T), sd_age = sd(age, na.rm = T))


## gender
gender <- survey %>% 
  group_by(gender) %>% 
  tally() %>% 
  mutate(N = sum(n),
         share = n/N) %>% 
  filter(gender == "f") %>% 
  select(share_female = share)

### Payoff data

payoff <- read_csv("Data/payoff_data.csv")

payoff <- payoff %>% 
  summarise(mean_payoff = mean(payoff), sd_payoff = sd(payoff), min_payoff = min(payoff), max_payoff = max(payoff))


sample_stats <- bind_cols(age, gender, payoff)

sample_stats %>% 
  kable(caption = "Sample statistics", format = "latex", booktabs = "T") %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("Tables/Sample_statistics.pdf")

## cleaning up
rm(age, gender, payoff, sample_stats, survey)
          
### generates sample statistics (section "Experimental procedures") 

### age and gender 

## data
df <- read_csv("Data/final_survey.csv")

## age
df %>% 
  select(age) %>% 
  summarise(mean(age, na.rm = T), sd(age, na.rm = T))


## gender
df %>% 
  group_by(gender) %>% 
  tally() %>% 
  mutate(N = sum(n),
         share = n/N)

### Payoff data

df <- read_csv("Data/payoff_data.csv")

df %>% 
  summarise(p = mean(payoff), s = sd(payoff), min = min(payoff), max = max(payoff))


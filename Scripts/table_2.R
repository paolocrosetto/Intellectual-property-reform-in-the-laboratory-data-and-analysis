###
###
### Generates Table 2 of the manuscript and saves it in /Tables

regnovote <- grouped %>% 
  filter(vote == "NONVOTE") %>% 
  lm(points~IPregime  + skills, data = .) 

regall <- grouped %>% 
  lm(points~repetition*IPregime  + skills, data = .) 

huxreg("All groups" = regall, "Control only" = regnovote, 
       coefs = c("Repetition 1" = "(Intercept)",
                 "Kept IP" = "repetition",
                 "Transition to noIP" = "IPregimenoIP",
                 "Word skills" = "skills"),
       stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, `°` = 0.1),
       error_pos = 'same', number_format = 2) %>% 
  quick_pdf(file = "Tables/Table_2.pdf")

## cleaning up
rm(regall, regnovote)

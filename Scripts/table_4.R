###
###
### Creates Table 4 of the paper -- determinants of vote and royalties

regdf <- summarised %>% 
  filter(repetition == 1 & vote == "VOTE") %>% 
  mutate(votedIP = votechr == "IP")


points <- regdf %>%  lm(points ~ skill + luck + greed + royalty_aversion, data = .) 
royout <- regdf %>% lm(royalties_out ~ skill + luck + greed + royalty_aversion, data = .) 
royin <- regdf %>%  lm(royalties_in ~ skill + luck + greed + royalty_aversion, data = .) 
roy <- regdf %>% lm(royalties ~ greed + skill + royalty_aversion + luck, data = .) 
votebare <- glm(votedIP ~ points +  royalties, data = regdf, family = binomial(link = "probit"))
vote <- glm(votedIP ~ points + skill + luck + greed +  royalty_aversion + royalties, data = regdf, family = binomial(link = "probit"))

table4 <- huxreg("Points OLS" = points, 
       "Royalties OUT" = royout, 
       "Royalties IN" = royin, 
       "Royalties BALANCE" = roy, 
       "Vote for IP" = votebare,
       "Vote for IP" = vote, 
       coefs = c('constant'='(Intercept)',  "skill" = 'skill', 
                 "luck" = 'luck', "greed" = 'greed',
                 "royalty aversion" = 'royalty_aversion',
                 "points" = "points", "royalty balance" = "royalties"))

table4 %>% quick_pdf(file = "Tables/Table_4.pdf")

## cleaning
rm(table4, vote, votebare, roy, royin, royout, points, regdf)

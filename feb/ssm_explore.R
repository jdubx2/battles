library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

df <- read.csv('feb/ssm.csv', stringsAsFactors = F)

df <- df %>% 
  gather(yr, status, -c(State,abbrev)) %>% 
  mutate(yr = as.numeric(str_replace(yr, 'X', '')))

df %>% 
  ggplot(aes(x = yr, y = abbrev))+
  geom_tile(aes(fill = status))

joins <- data.frame(expand.grid(unique(df$yr),unique(df$status)), share = 0) %>% 
  rename(yr = Var1, status = Var2) %>% 
  mutate(status = as.character(status))

df2 <- df %>% 
  group_by(status, yr) %>% 
  summarise(n = n(),
            share = round(n/50,2)) %>% 
  select(status,yr,share)


df3 <- bind_rows(df2, joins) %>% ungroup() %>% 
  group_by(status,yr) %>% 
  summarise(share = sum(share)) %>% 
  mutate(yr = paste0('X',yr),
         lpos = ifelse(status == 'No Law',1,
                       ifelse(status == 'Statutory Ban', 2,
                              ifelse(status == 'Constitutional Ban', 3, 4)))) %>% 
  spread(yr,share)

library(jsonlite)

json <- toJSON(df3)

write(json, 'feb/shares.json')

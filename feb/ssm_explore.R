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

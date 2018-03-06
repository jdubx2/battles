
library(dplyr)
library(ggplot2)

setwd("~/Repos/battles/mar")

df <- read.csv('data/hygdata_v3.csv', stringsAsFactors = F)

df2 <- df %>% select(id, proper, dist, rv, mag,absmag,spect,ci,con)

df2 %>% filter(con != '', dist < 100000, mag >=7) %>% 
  group_by(con) %>% mutate(avg_dist = mean(dist)) %>% 
  arrange(desc(avg_dist)) %>% mutate(rownum = row_number()) %>% 
  ggplot(aes(x=reorder(con,rownum), y = dist)) + geom_jitter(size = .5, alpha = .7) +
  coord_flip()

ci_vec <- df2 %>% filter(con != '', dist < 100000, mag >=7, !(is.na(ci))) %>% select(ci)

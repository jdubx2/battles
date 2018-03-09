
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


library(jsonlite)

# write(toJSON(ci_vec), 'data/ci.json')

grad <- as.data.frame(jsonlite::fromJSON('data/gradient.json'))

df3 <- df2 %>% 
  filter(con != '', dist < 100000, mag >=7, !(is.na(ci))) %>% rowwise() %>% 
  mutate(hex = grad$hex[which.min(abs(grad$bv - ci))])


col <- as.character(df3$hex)
names(col) <- as.character(df3$hex)

  df3 %>% group_by(con) %>% mutate(avg_dist = mean(dist)) %>% 
  arrange(desc(avg_dist)) %>% mutate(rownum = row_number()) %>% 
  ggplot(aes(x=reorder(con,rownum), y = dist, color = hex)) + 
    geom_jitter(size = .85, alpha = .25) +
    scale_color_manual(values = col) +
    coord_flip()+
    guides(color = F) +
    theme(plot.background = element_rect(fill = 'gray3', color ='gray3'),
          panel.background = element_rect(fill = 'gray3', color ='gray3'),
          panel.grid = element_blank())

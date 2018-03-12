
library(dplyr)
library(ggplot2)
library(jsonlite)
library(stringr)
library(extrafont)

setwd("~/Repos/battles/mar")

df <- read.csv('data/hygdata_v3.csv', stringsAsFactors = F)

grad <- as.data.frame(jsonlite::fromJSON('data/gradient.json'))

install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)

NISTdegTOradian(180)
NISTradianTOdeg(pi)
#-------------------#
spect_vec <- c('O','B','A','F','G','K','M')

df2 <- df %>% select(id, proper, dist, rv, mag, absmag,spect,ci,con,lum,x,y, ra, dec)

df3 <- df2 %>% 
  mutate(spect1 = toupper(str_sub(spect,1,1))) %>% 
  mutate(x2 = 1 * cos(NISTdegTOradian(dec)) * cos(NISTdegTOradian(ra*15)),
         y2 = 1 * cos(NISTdegTOradian(dec)) * sin(NISTdegTOradian(ra*15)),
         z2 = dist * sin(NISTdegTOradian(dec))) %>% 
  filter(dist < 100000, !(is.na(ci)), spect1 %in% spect_vec) %>% rowwise() %>% 
  mutate(hex = grad$hex[which.min(abs(grad$bv - ci))])

col <- as.character(df3$hex)
names(col) <- as.character(df3$hex)

# top_con <- df3 %>% group_by(con) %>% 
#   summarise(n = n()) %>% arrange(desc(n)) %>% 
#   slice(1:12)
  
df3 %>% 
  group_by(spect1) %>% 
  summarise(avg_mag = mean(mag),
            avg_absmag = mean(absmag),
            avg_lum = mean(lum))
  
  df3 %>% 
    mutate(lum = ifelse(lum > 5000,5000,lum),
           sz = ifelse(lum > 1500, 1500,lum)) %>% 
    mutate(spect2 = ifelse(spect1 %in% c('O','B'), 'O/B', spect1),
           spect2 = paste0('Class ', spect2),
           spect2 = factor(spect2, 
                           levels = c('Class O/B', 'Class A', 'Class F', 'Class G', 'Class K', 'Class M'))) %>% 
    ggplot(aes(x=x2, y = y2, color = hex)) + 
      geom_point(aes(alpha = lum, size = sz)) +
      scale_color_manual(values = col) +
      scale_size_continuous(range = c(.005,1.03))+
      scale_alpha_continuous(range = c(.085,1)) +
      coord_flip()+
      guides(color = F) +
      theme(plot.background = element_rect(fill = 'gray3', color ='gray3'),
            panel.background = element_rect(fill = 'gray3', color ='gray3'),
            panel.grid = element_blank(),
            strip.background = element_rect(color = 'gray3', fill = 'gray3'),
            strip.text = element_text(color = 'gray90', family = 'Consolas', size = 23),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(size = 30, family = 'Consolas', color = 'gray90', hjust = .5),
            plot.subtitle = element_text(size = 20, family = 'Consolas', color = 'gray90', hjust = .5)) +
      facet_wrap(~spect2) +
    guides(size = F, alpha = F)+
    labs(title = 'Stars by Spectral Class', subtitle = '100k+ Stars from the HYG Database')
  
  
  
  df4 <- df3 %>% filter(con =='Psc') %>% 
    mutate(x2 = .1 * cos(NISTdegTOradian(dec)) * cos(NISTdegTOradian(ra*15)),
           y2 = .1 * cos(NISTdegTOradian(dec)) * sin(NISTdegTOradian(ra*15)),
           z2 = .1 * sin(NISTdegTOradian(dec)))
  
  library(scatterplot3d)
  
  scatterplot3d(df4$x2, df4$y2, df4$z2, angle = -170)

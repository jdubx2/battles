
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
  
  p0 <- df3 %>% 
    mutate(lum = ifelse(lum > 5000,5000,lum),
           sz = ifelse(lum > 1700, 1700,lum)) %>% 
    mutate(spect2 = ifelse(spect1 %in% c('O','B'), 'O/B', spect1),
           spect2 = paste0('Class ', spect2),
           spect2 = factor(spect2, 
                           levels = c('Class O/B', 'Class A', 'Class F', 'Class G', 'Class K', 'Class M'))) %>% 
    ggplot(aes(x=x2, y = y2, color = hex)) + 
      geom_point(aes(alpha = lum, size = sz)) +
      scale_color_manual(values = col) +
      scale_size_continuous(range = c(.005,.99))+
      scale_alpha_continuous(range = c(.08,1)) +
      coord_flip()+
      guides(color = F) +
      theme(plot.background = element_rect(fill = 'gray3', color ='gray3'),
            panel.background = element_rect(fill = 'gray3', color ='gray3'),
            panel.grid = element_blank(),
            strip.background = element_rect(color = 'gray3', fill = 'gray3'),
            strip.text = element_text(color = 'gray90', family = 'Consolas', size = 19),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(size = 28, family = 'Consolas', color = 'gray90', hjust = .5),
            plot.subtitle = element_text(size = 19, family = 'Consolas', color = 'gray90', hjust = .5),
            plot.margin = margin(1,1,4.5,1,unit = 'cm')) +
      facet_wrap(~spect2, nrow=1) +
    guides(size = F, alpha = F)+
    labs(title = 'Spectral Classification of Stars')
  
  ggsave(file="stars.png", plot=p0, width=14.8, height=5.3, bg = 'transparent')
  
temps_df <- data.frame(class = factor(c('O', 'B', 'A', 'F', 'G', 'K', 'M'), ordered = T),
                       temp = rev(c(3500,5000,6000,7500,11000,25000,40000)),
                       share = c(.0003,.13, .6,3,7.6, 12.1,76.45),
                       hex = c("#bdf0ff", "#ddf2ff","#fff0ff","#ffebd1","#ffe49b","#ffcf49","#ffbe28"))
  
col2 <- as.character(temps_df$hex)
names(col2) <- as.character(temps_df$hex)



library(cowplot)

p1 <- temps_df %>% 
  ggplot(aes(x = reorder(class, temp), y = temp, fill = hex))+
  geom_col(alpha = .9)+
  scale_fill_manual(values = col2)+
  theme(plot.background = element_rect(fill = 'gray3', color ='gray3'),
        panel.background = element_rect(fill = 'gray3', color ='gray3'),
        panel.grid = element_blank(),
        strip.background = element_rect(color = 'gray3', fill = 'gray3'),
        strip.text = element_text(color = 'gray90', family = 'Consolas', size = 19),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  guides(fill = F)+
  coord_flip()+
  scale_y_reverse(expand = c(0,0))

p2 <- temps_df %>% 
  ggplot(aes(x = reorder(class, -share), y = share, fill = hex))+
  geom_col(alpha = .9)+
  scale_fill_manual(values = col2)+
  theme(plot.background = element_rect(fill = 'gray3', color ='gray3'),
        panel.background = element_rect(fill = 'gray3', color ='gray3'),
        panel.grid = element_blank(),
        strip.background = element_rect(color = 'gray3', fill = 'gray3'),
        strip.text = element_text(color = 'gray90', family = 'Consolas', size = 19),
        axis.text.y = element_text(color = 'gray85', family = 'Consolas', size = 21),
        axis.text.x = element_blank(),
        axis.ticks.y = element_line(color= 'white'),
        axis.title = element_blank())+
  guides(fill = F)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))


p4 <- plot_grid(p1,p2)

ggsave(file="bars.svg", plot=p4, width=18.6, height=1.9, bg = 'transparent')


grad %>% 
  ggplot(aes(x = bv, y = 1, fill = hex))+
  geom_col()+
  scale_fill_manual(values = col) +
  guides(fill = F) +
  theme(plot.background = element_rect(fill = 'gray3', color ='gray3'),
        panel.background = element_rect(fill = 'gray3', color ='gray3'),
        panel.grid = element_blank(),
        strip.background = element_rect(color = 'gray3', fill = 'gray3'),
        strip.text = element_text(color = 'gray90', family = 'Consolas', size = 19),
        axis.text = element_text(color='white'),
        axis.ticks = element_line(color= 'white'),
        axis.title = element_blank())+
  scale_x_continuous(breaks = seq(-1,2,.2))+
  scale_y_continuous(expand =c(0,0))
  
  
  df4 <- df3 %>% filter(con =='Psc') %>% 
    mutate(x2 = .1 * cos(NISTdegTOradian(dec)) * cos(NISTdegTOradian(ra*15)),
           y2 = .1 * cos(NISTdegTOradian(dec)) * sin(NISTdegTOradian(ra*15)),
           z2 = .1 * sin(NISTdegTOradian(dec)))
  
  library(scatterplot3d)
  
  scatterplot3d(df4$x2, df4$y2, df4$z2, angle = -170)

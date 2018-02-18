df <- read_excel("C:/Users/WEISJ059/Documents/Jesse/dib/algae.xlsx")

df <- gather(df, lux, gr, -c(Species,Temp))
df$gr <- gsub('\\.\\.','\\.',df$gr)
df <- mutate(df, gr = as.numeric(gr))


plot2 <- df %>% 
  mutate(lux = ifelse(lux == '5000', 'Growth at 5k Lux', 'Growth at 2.5k Lux')) %>% 
  ggplot(aes(x = Temp, y = gr, color = lux))+
  #geom_line(size = 1)+
  geom_point(shape = 15, alpha = .4, size = 6) +
  facet_wrap(~Species,ncol = 6) +
  scale_color_manual(values = c('dodgerblue2','orange2'))+
  theme(panel.background = element_rect(fill = 'gray10', color = 'gray10'),
        plot.background = element_rect(fill = 'gray10',color='gray10'),
        text = element_text(family = 'Calibri', color = 'gray70'),
        axis.text = element_text(family = 'Calibri', color = 'gray70'),
        strip.background = element_rect(fill='gray20'),
        strip.text = element_text(face = 'italic'),
        plot.title = element_text(hjust = .5, margin = margin(t = 0, r = 0, b = 40, l = 0), 
                                  size = 21, family = 'Calibri Light', face = 'bold'),
        axis.title.x = element_text(family = 'Calibri Light', size = 17, 
                                    margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(family = 'Calibri Light', size = 17, 
                                    margin = margin(t = 0, r = 12, b = 0, l = 0)),
        # axis.line.x = element_line(color = 'gray30'),
        # axis.line.y = element_line(color = 'gray30'),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        # axis.ticks = element_line(color = 'gray30'),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = 'gray15', size = .3),
        legend.position = 'top')
             

ggsave(file="C:/Users/WEISJ059/Documents/Jesse/dib/facets.svg", plot=plot, width=13.8, height=8, bg = "transparent")

ggsave(file="C:/Users/WEISJ059/Documents/Jesse/dib/riblegend.svg", plot=plot2, width=13.8, height=8, bg = "transparent")

#-----------------------------------------------------------#

temp <- filter(df3, Species == 'Navicula incerta')

df2 <- df %>% 
  mutate(lux = paste0('l',lux)) %>%
  spread(lux,gr)

df3 <- df2 %>% 
  group_by(Species) %>% 
  mutate(diff25 = l2500 - lag(l2500),
         diff50 = l5000 - lag(l5000),
         diffTemp = Temp - lag(Temp),
         slope25 = diff25/diffTemp,
         slope50 = diff50/diffTemp,
         intcpt25 = l2500 - slope25 * Temp,
         intcpt50 = l5000 - slope50 * Temp,
         x2 = (intcpt25 - intcpt50)/(slope50 - slope25),
         y3 = slope25 * x2 + intcpt25,
         x2 = ifelse(x2 > Temp | x2 < lag(Temp), NA, x2),
         y3 = ifelse(x2 > Temp | x2 < lag(Temp), NA, y3),
         y4 = y3,
         seg_type = ifelse(l2500 > l5000, 'low','high'))

break_points <- df3 %>% filter(!(is.na(x2))) %>% 
  select(Species, x = x2, ymin = y3, ymax = y4)

high_ribbon <- df3 %>% filter(seg_type == 'high') %>% 
  select(Species, x = Temp, ymin = l2500, ymax = l5000)

high_ribbon <- bind_rows(high_ribbon,break_points)

low_ribbon <- df3 %>% filter(seg_type == 'low') %>% 
  select(Species, x = Temp, ymin = l5000, ymax = l2500)

low_ribbon <- bind_rows(low_ribbon,break_points)


plot <- df3 %>% 
  ggplot()+
  geom_hline(yintercept = 0, linetype='dashed', color = 'gray70', size = .3, alpha = .7) +
  geom_line(aes(x = Temp, y = l2500), color = 'dodgerblue2', size = 1)+
  geom_line(aes(x = Temp, y = l5000), color = 'orange2', size = 1) +
  geom_ribbon(data = high_ribbon, aes(x = x, ymin = ymin, ymax = ymax), fill = 'orange2', alpha = .4) +
  geom_ribbon(data = low_ribbon, aes(x = x, ymin = ymin, ymax = ymax), fill = 'dodgerblue2', alpha = .4) +
  scale_x_continuous(breaks = seq(5,30,5)) +
  scale_y_continuous(limits = c(-.7,1.2), breaks = seq(-.6,1.2,.4)) +
  facet_wrap(~Species,ncol=6) +
  theme(panel.background = element_rect(fill = 'gray10', color = 'gray10'),
        plot.background = element_rect(fill = 'gray10',color='gray10'),
        text = element_text(family = 'Calibri', color = 'gray70'),
        axis.text = element_text(family = 'Calibri', color = 'gray70'),
        strip.background = element_rect(fill='gray20'),
        strip.text = element_text(face = 'italic'),
        plot.title = element_text(hjust = .5, margin = margin(t = 0, r = 0, b = 40, l = 0), 
                                  size = 21, family = 'Calibri Light', face = 'bold'),
        axis.title.x = element_text(family = 'Calibri Light', size = 17, 
                                    margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(family = 'Calibri Light', size = 17, 
                                    margin = margin(t = 0, r = 12, b = 0, l = 0)),
        # axis.line.x = element_line(color = 'gray30'),
        # axis.line.y = element_line(color = 'gray30'),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        # axis.ticks = element_line(color = 'gray30'),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = 'gray15', size = .3)) +
  labs(x = 'Temperature °C', y = 'Divisions per Day', title = 'Specific Growth Rates of Algae')

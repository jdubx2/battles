library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(rlang)
library(purrr)

df <- read_excel('the-office-lines.xlsx')

smry <- function(df,col1){
  col1 <- enquo(col1)
  
  df %>% group_by(!! col1) %>% 
    tally() %>% arrange(desc(n))
}

# speakers <- smry(df,speaker)
# 
# speakers <- mutate(speakers, 
#                    spkr_list = str_split(speaker,'([[:space:]](and|&)[[:space:]]|,)'))

df2 <- df %>% filter(deleted == F) %>% 
  mutate(speaker = str_split(speaker,'([[:space:]](and|&)[[:space:]]|,)')) %>% 
  unnest() %>% group_by(season,episode,scene,speaker) %>% summarise()

keep_names <- df2 %>% 
  group_by(speaker) %>% 
  summarise(spk_count = n()) %>% arrange(desc(spk_count)) %>% 
  filter(spk_count >=80)

df3 <- df2 %>% 
  inner_join(keep_names) %>% 
  mutate(ses_id = paste0(season,'|',episode,'|',scene)) %>% 
  group_by(ses_id, ses_count = n()) %>% ungroup() %>% filter(ses_count != 1)


ses_index_df <- df3 %>% 
  mutate(row_n = row_number()) %>% 
  group_by(ses_id) %>% 
  mutate(min_row = min(row_n),
         max_row = max(row_n)) %>% 
  group_by(ses_id,min_row,max_row) %>% 
  summarise()

get_combos <- function(min_idx, max_idx) {
  
  filtered <- df3[min_idx:max_idx,]
  combo_matrix <- combn(filtered$speaker, 2)
  
  combo_df <- as.data.frame(t(combo_matrix), stringsAsFactors = F)
  
  return(combo_df)
}

combo_list <- list()
i = 1

for(row_idx in seq_len(nrow(ses_index_df))){
  
  min <- as.numeric(ses_index_df[row_idx, 2])
  max <- as.numeric(ses_index_df[row_idx, 3])
  
  if(max > min){
    combo_list[[i]] <- get_combos(min,max)
    i <- i + 1
  }
}
combo_df_final <- bind_rows(combo_list)

#ensure alphabetical order c1 va c2
combo_df_final <- combo_df_final %>% 
  mutate(V3 = ifelse(V1<V2,V1,V2),
         V4 = ifelse(V3==V1,V2,V1))

edges <- combo_df_final %>%
  group_by(V3, V4) %>%
  tally() %>% arrange(desc(n))

library(jsonlite)

edge_df <- edges %>% rename(source=V3,target=V4,value=n)
node_df <- df3 %>% group_by(speaker,spk_count) %>% summarise() %>% rename(id=speaker,value=spk_count)

saveRDS(edge_df,'edges.rds')
saveRDS(node_df,'nodes.rds')

write_json(list(nodes=node_df,links=edge_df),'office_graph.json')

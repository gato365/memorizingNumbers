library(tidyverse)
library(readxl)
library(jsonlite)
# setwd("C:/Users/james/OneDrive/Documents/Important_Files/Life")
e_number_df = read_xlsx('emans_info.xlsx', sheet = 'e') #%>% 
# data.frame(row.names = 1)

odd_e_number_df = e_number_df %>% 
  mutate(number = as.numeric(str_remove(set,'set-'))) %>% 
  filter(number %% 2 != 0 )
even_e_number_df = e_number_df  %>% 
  anti_join(odd_e_number_df) %>%  
  select(-set)


df = bind_cols(odd_e_number_df,even_e_number_df) %>%  
  select(-contains('set'),-number) %>% 
  unite('Merged', `col-1...2`:`col-4...10`,remove =TRUE, sep = ' - ') %>% 
  mutate(Merged = str_remove_all(Merged,'\'')) %>% 
  mutate(labeled = c(rep('A',5),rep('B',5),rep('C',5),rep('D',5),rep('E',5),rep('F',5) ),
         set = c(rep(paste0("set ",1:5),5),paste0("set ",1:5) ))




nested_list <- df %>%
  group_split(labeled) %>%
  set_names(str_c("label_", seq_along(.))) %>%
  map(~ .x %>%
        select(-labeled) %>%
        split(.$set) %>%
        map(~ .$Merged)) %>%
  as.list()

json <- toJSON(nested_list, pretty = TRUE)
write_json(json, "e_as_json.json")

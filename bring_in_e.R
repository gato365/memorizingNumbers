library(tidyverse)
library(readxl)
setwd("C:/Users/james/OneDrive/Documents/Important_Files/Life")
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
  unite('Merged', `col-1...2`:`col-4...10`,remove =TRUE, sep = '') %>% 
  mutate(Merged = str_remove_all(Merged,'\'')) %>% 
  mutate(labeled = c(rep('A',6),rep('B',6),rep('C',6),rep('D',4),rep('E',4),rep('F',2) ) )

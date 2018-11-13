library(rvest)
library(tidyverse)

url <- c("https://enr.electionsfl.org/ALA/2012/Summary/","https://enr.electionsfl.org/BAK/Summary/2000/",
         "https://enr.electionsfl.org/BAY/1986/Summary/","https://enr.electionsfl.org/BRA/Summary/2010/",
         "https://enr.electionsfl.org/BRE/1955/Summary/","https://enr.electionsfl.org/BRO/Summary/1985/",
         "https://enr.electionsfl.org/CAL/Summary/2024/","https://enr.electionsfl.org/CHA/Summary/1968/",
         "https://enr.electionsfl.org/CIT/Summary/1965/","https://enr.electionsfl.org/CLA/1956/Summary/",
         "https://enr.electionsfl.org/CLL/1994/Summary/","https://enr.electionsfl.org/CLM/Summary/1966/",
         "https://enr.electionsfl.org/DES/Summary/2001/","https://enr.electionsfl.org/DIX/Summary/2019/",
         "https://enr.electionsfl.org/DUV/Summary/1964/","https://enr.electionsfl.org/ESC/1953/Summary/",
         "https://enr.electionsfl.org/FLA/1963/Summary/","https://enr.electionsfl.org/FRA/Summary/1992/",
         "https://enr.electionsfl.org/GAD/Summary/2018/","https://enr.electionsfl.org/GIL/Summary/2002/",
         "https://enr.electionsfl.org/GLA/Summary/1998/","https://enr.electionsfl.org/GUL/2014/Summary/",
         "https://enr.electionsfl.org/HAM/Summary/2020/","https://enr.electionsfl.org/HAR/Summary/2016/",
         "https://enr.electionsfl.org/HEN/Summary/2026/","http://enr.electionsfl.org/HER/Summary/1977",
         "https://enr.electionsfl.org/HIG/Summary/1978/","https://enr.electionsfl.org/HIL/Summary/1974/",
         "https://enr.electionsfl.org/HOL/Summary/2004/","https://enr.electionsfl.org/IND/Summary/1972/",
         "https://enr.electionsfl.org/JAC/Summary/1960/","https://enr.electionsfl.org/JEF/Summary/2007/",
         "https://enr.electionsfl.org/LAF/Summary/2015/"
         
)
#create empty list to store dataframes
df_list <- list()

for (i in seq_along(url)){
  #extract data from each link
  race <- read_html(url[i]) %>% 
    html_nodes("#content") %>% 
    html_text()
  
  #tidy the text
  text <- race %>%
    str_replace_all("\r\n", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("%","") %>% 
    str_replace_all(",","") %>% 
    str_replace_all("\\(","") %>% 
    str_replace_all("\\)","") %>% 
    tolower()
  
  votes <- str_extract_all(text, "rick scott rep\\s\\d+\\.\\d+\\s\\d+\\sbill nelson dem\\s\\d+\\.\\d+\\s\\d+") 
  candidate <- str_extract_all(votes, "[[:alpha:] ]{2,}") %>% unlist()
  turnout <- str_extract_all(votes,"\\d+\\.\\d+\\s\\d+") %>% unlist()
  
  #populate list
  df_list[[i]] <- data.frame(candidate = candidate, turnout = turnout) %>% 
    mutate(county = str_extract(url[i], "[[:upper:]]+")) %>% #get abbreviated county name
    separate(turnout, c('percent', 'total'), sep=" ") #separate turnout
}

#create dataframe
df <- do.call(rbind, my_list)



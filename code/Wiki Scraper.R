library(tidyverse)
library(data.table)
library(XML)
library(httr)
setwd("C:/Users/slapt/OneDrive/Documents/Samotsvety/Proj Base Rate/Wiki Scrape")

# import list of urls and table indices
# url: link to webpage
# group_ind: index number for a group of tables
#   must share a url
# table_no: specifies the html table element to be selected
# desc: freetext description of the group
# notes: misc
wiki_page_set = fread("ref/wiki_table_reference.csv") %>%
  as.data.frame() %>%
  filter(!is.na(table_no))

# function to clean up entries and remove empty columns
clean_table = function(x){
  return(x %>%
           mutate(across(everything(),trimws)) %>%
           na_if("") %>%
           select_if(~sum(!is.na(.)) > 0))
}

# function to import tables as specified in wiki_page_set
# grp = a group_index from wiki_page_set
get_tables = function(grp){
  tmp = wiki_page_set %>%
    filter(group_index == grp)
  url_in = tmp %>% pull(url) %>% unique()
  
  table_seq = tmp %>%
    pull(table_no)
  
  # read in the webpage data
  page_info = GET(url_in) %>%
    content("text") %>%
    readHTMLTable(doc = .) 
  
  # select tables one-by-one and glue together
  out = page_info %>%
    .[[table_seq[[1]]]] %>%
    clean_table()
  if(length(table_seq) > 1){
    for(i in table_seq[c(2:length(table_seq))]){
      tmp2 = page_info %>%
        .[[i]] %>%
        clean_table()
      out = rbind(
        out,
        tmp2
      )
    }
  }
  
  return(out)
}

# check that every group reads in properly
for(i in unique(wiki_page_set$group_index)){
  print(i)
  tmp = get_tables(i)
}




# Manually check a page to find the right indices
url_tmp = "https://en.wikipedia.org/wiki/List_of_assassinations"
out = GET(url_tmp) %>%
  content("text") %>%
  readHTMLTable(doc = .)


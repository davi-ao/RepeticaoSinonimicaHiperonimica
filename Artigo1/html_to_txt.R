library(tidyverse)
library(rvest)

html = read_html('Artigo1/body.html')

tibble(id = html %>%
         html_elements('p') %>%
         html_attr('id'),
       text = html %>%
         html_elements('p') %>%
         html_text2()) %>%
  filter(str_detect(id, '^p\\d{4}$')) %>%
    .$text

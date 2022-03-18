# Carregar pacotes necessários
library(tidyverse)
library(pdftools)

folder = 'Artigo1/ScienceDirect_articles_10Mar2022_12-19-53.718/'
paths = list.files(folder)

for (i in 1:length(paths)) {
  article_text = pdf_text(paste0(folder, paths[i]))
  
  write_file(article_text %>% str_c(collapse = ''), 
             paste0('Artigo1/corpus/article', 
                    str_pad(toString(i), 3, pad = '0'),
                    '.txt'))
}

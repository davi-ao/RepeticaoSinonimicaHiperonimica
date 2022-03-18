############################################################
# TRATAMENTO DA REPETIÇÃO LEXICAL SINONÍICA E HIPERONÍMICA #
############################################################

# Carregar pacotes utilizados
library(tidyverse)
library(igraph)

# CONFIGURAÇÃO INICIAL ---------------------------------------------------------

# Carregar lista de lemas de Měchura (2018)
# O lema é a forma canonica de um lexema
# Um lexema é uma "família de palavras" formado por um lema e suas flexões
lemas = read_file('lemmatization-pt.txt') %>%
  str_split('\\n') %>%
  unlist() %>%
  tibble(lemas = .) %>%
  separate(lemas, into = c('lema', 'flexao'), sep = '\\t') %>%
  filter(!is.na(flexao)) %>%
  mutate(lema = lema %>% str_remove_all('\\r'),
         flexao = flexao %>% str_remove_all('\\r')) %>% 
  rename(tokens = flexao)

# Carregar lista de sinonimos do PAPEL
sinonimos = read_file('PAPEL/relacoes_final_SINONIMIA.txt') %>%
  str_split('\n') %>%
  unlist() %>%
  tibble(sinonimo = .) %>%
  separate(sinonimo, c('sinonimo1', 'classe', 'sinonimo2'), sep = ' ') %>%
  filter(!is.na(sinonimo2)) %>%
  mutate(classe = classe %>% str_match('_(.+)_') %>% .[,2])

# Gerar tabela considerando direcao sinonimo1->sinonimo2
sinonimos1 = sinonimos %>%
  rename(lema = sinonimo1)

# Gerar tabela considerando direcaõ sinonimo2->sinonimo1
sinonimos2 = sinonimos %>%
  rename(lema = sinonimo2) %>%
  select(-classe)

# Carregar lista de hiperonimos do PAPEL
hiperonimos = read_file('PAPEL/relacoes_final_HIPERONIMIA.txt') %>%
  str_split('\n') %>%
  unlist() %>%
  tibble(hiperonimo = .) %>%
  separate(hiperonimo, c('hiperonimo', 'relacao', 'hiponimo'), sep = ' ') %>%
  filter(!is.na(hiponimo)) %>%
  select(hiperonimo, hiponimo) %>%
  rename(lema = hiponimo)

# Stopwords: artigos, preposições, conjunções
stopwords = c('o', 'a', 'os', 'as', 'de', 'do', 'da', 'em', 'no', 'na', 'e', 
              'ou')
# FUNÇÕES DE TRATAMENTO DOS TEXTOS E GERAÇÃO DE REDES --------------------------

# Funcao de tratamento do texto
tratar_texto = function(texto) {
  texto_tratado = texto %>%
    # Separa os parágrafos
    str_split('\\n') %>%
    unlist() %>%
    tibble(paragraphs = .) %>%
    mutate(p = row_number()) %>%
    # Remove as linhas do link e do título
    filter(p > 2) %>%
    # Remove "parágrafos" vazios
    filter(paragraphs != '') %>%
    # Separa os períoldos de acordo com Caldeira et al. (2006)
    separate_rows(paragraphs, sep = '[.:?!…]') %>%
    # Faz o tratamento inicial de acordo com Pereira et al. (2011)
    mutate(paragraphs = paragraphs %>% 
             str_remove_all('[““””’,;\\(\\)\\[\\]—]') %>% #R2
             # Remoção de pronomes oblíquos
             str_remove_all(
               '(-me|-te|-os|-as|-o|-a|-se|-lhes|-lhe|-los|-las|-lo|-la)')) %>% 
    mutate(paragraphs = paragraphs %>% str_replace_all('0', 'zero'), #R5
           paragraphs = paragraphs %>% str_replace_all('1', 'um'),
           paragraphs = paragraphs %>% str_replace_all('2', 'dois'),
           paragraphs = paragraphs %>% str_replace_all('3', 'três'),
           paragraphs = paragraphs %>% str_replace_all('4', 'quatro'),
           paragraphs = paragraphs %>% str_replace_all('5', 'cinco'),
           paragraphs = paragraphs %>% str_replace_all('6', 'seis'),
           paragraphs = paragraphs %>% str_replace_all('7', 'sete'),
           paragraphs = paragraphs %>% str_replace_all('8', 'oito'),
           paragraphs = paragraphs %>% str_replace_all('9', 'nove')) %>%
    filter(!paragraphs %in% c('\r', ' ', '')) %>%
    mutate(paragraphs = paragraphs %>% str_trim()) %>%
    mutate(p = row_number()) %>%
    # Separa os tokens
    separate_rows(paragraphs, sep = '\\s+') %>%
    rename(tokens = paragraphs) %>%
    filter(tokens != '') %>%
    mutate(tokens = tokens %>% str_to_lower()) %>%
    # Substitui flexões por lemas e remove stopwords
    left_join(lemas, by = 'tokens') %>%
    mutate(lema = ifelse(is.na(lema), tokens, lema)) %>%
    # Remove P, Conj e Artigos, de acordo com Caldeira et al. (2006)
    filter(!lema %in% stopwords)
  
  # Adiciona sinônimos
  texto_sinonimos = texto_tratado %>%
    left_join(sinonimos1, by = 'lema')
  
  # Inclui sinônimos adicionais e hiperonimos
  texto_hiperonimos = texto_sinonimos %>%
    left_join(sinonimos2, by = 'lema') %>%
    left_join(hiperonimos, by = 'lema')
  
  # Conecta hiperônimos
  
  texto_hiperonimos
}

# Funcao para organizar tabela para criacao das redes com adição de arestas
preparar_para_rede = function(texto_tratado) {
  texto_redes = texto_tratado %>%
    pivot_longer(c(sinonimo1, sinonimo2, hiperonimo), 
                 names_to = 'tipo', 
                 values_to = 'relacao') %>%
    group_by(texto, relacao) %>%
    mutate(r = cur_group_id()) %>%
    ungroup() %>%
    mutate(r = ifelse(is.na(relacao), NA, r)) %>%
    select(texto, p, lema, r) %>%
    distinct(texto, p, lema, r) %>%
    group_by(texto, r) %>%
    mutate(rn = ifelse(is.na(r), NA, n())) %>%
    ungroup()
  
  texto_redes
}

# Função para gerar rede de cliques de lexemas sobrepostos
gerar_rede_final = function(texto_tratado) {
  rede_lexemas = texto_tratado %>% 
    select(texto, p, lema) %>%
    group_by(texto, p, lema) %>%
    top_n(1) %>%
    ungroup() %>%
    rename(Source = lema) %>%
    mutate(Target = Source) %>%
    group_by(texto, p) %>%
    expand(Source, Target) %>%
    mutate(Type = 'undirected',
           Weight = 1) %>%
    ungroup() %>%
    filter(Target != Source)
  
  rede_lexemas
}

# Função para gerar rede de cliques no estado inicial
gerar_rede_0 = function(rede_final) {
  # Gerar rede de cliques de lexemas inicial
  rede_0 = rede_final %>%
    mutate(Source = paste0(Source, p),
           Target = paste0(Target, p))
  
  rede_0
}

# Função para converter formato de lista de aretas para formato .net
salvar_net = function(rede_arestas, arquivo) {
  vertices_tbl = rede_arestas %>% 
    .$Source %>% 
    unique() %>% 
    tibble(vertices = .) %>% 
    mutate(id = row_number())
  vertices_inicio = paste('*Vertices', vertices_tbl %>% nrow())
  vertices = paste0(1:(vertices_tbl %>% nrow()), 
                    ' "', 
                    vertices_tbl$vertices, 
                    '"')
  edges_inicio = '*Edges'
  edges_conexoes = paste(rede_arestas %>% 
                           rename(vertices = Source) %>% 
                           left_join(vertices_tbl, by='vertices') %>%
                           .$id,
                         rede_arestas %>% 
                           rename(vertices = Target) %>% 
                           left_join(vertices_tbl, by='vertices') %>%
                           .$id)
  
  if (file.exists(arquivo)) {
    file.remove(arquivo)
  }
  
  write(vertices_inicio, arquivo)
  
  for (i in 1:length(vertices)) {
    write(vertices[i], arquivo, append=T)
  }
  
  write(edges_inicio, arquivo, append = T)
  
  for (i in 1:length(edges_conexoes)) {
    write(edges_conexoes[i], arquivo, append = T)
  }
}

# Construir tabela de arestas adicionais com base nas relacões de sinonímia
# e hiperonímia
gerar_arestas_relacoes = function(texto_tratado, texto_rede) {
  arestas_relacoes = texto_tratado %>%
    pivot_longer(c(sinonimo1, sinonimo2, hiperonimo), 
                 names_to = 'tipo', 
                 values_to = 'relacao') %>%
    select(texto, p, lema) %>%
    left_join(texto_tratado %>%
                pivot_longer(c(sinonimo1, sinonimo2, hiperonimo), 
                             names_to = 'tipo', 
                             values_to = 'relacao') %>%
                select(texto, lema, relacao) %>%
                rename(lema = relacao, relacao = lema) %>%
                filter(!is.na(lema)),
              by = c('texto', 'lema')) %>%
    filter(!is.na(relacao)) %>%
    distinct(texto, p, lema, relacao) %>%
    rename(Source = lema, Target = relacao) %>%
    mutate(p = 0, Type = 'undirected', Weight = 1)
  
  arestas_relacoes = arestas_relacoes %>%
    bind_rows(texto_rede %>%
                filter(rn > 1) %>%
                group_by(texto, r) %>%
                rename(Source = lema) %>%
                mutate(Target = Source) %>%
                expand(Source, Target) %>%
                filter(Source != Target) %>% 
                mutate(p = 0, Type = 'undirected', Weight = 1) %>%
                ungroup() %>%
                select(-r)) %>%
    distinct_all()
  
  arestas_relacoes
}

# ANÁLISE DE TEXTOS DO CORPUS --------------------------------------------------

# Lista de textos do corpus
corpus = c('corpus/2020_q6.txt',
           'corpus/2020_q8.txt',
           'corpus/2020_q9.txt',
           'corpus/2020_q10.txt', 
           'corpus/2020_q13.txt',
           'corpus/2020_q17.txt',
           'corpus/2020_q20.txt',
           'corpus/2020_q28.txt',
           'corpus/2020_q31.txt',
           'corpus/2020_q35.txt',
           'corpus/2020_q42.txt',
           'corpus/2020_q43.txt')

corpus_news = c('noticia1.txt',
                'noticia2.txt',
                'noticia3.txt')

# Carregar textos
textos = sapply(corpus_news, read_file) %>% as_tibble()

# Definir tabela inicial de textos tratados
textos_tratados = tibble(texto = integer(), tokens = character(), p = integer())

# Realizar tratamento inicial dos textos
for (r in 1:nrow(textos)) {
  textos_tratados = textos_tratados %>% bind_rows(
    textos[r,] %>% 
      tratar_texto() %>%
      mutate(texto = r)
  )
}

# Preparar para construção das redes
textos_redes = preparar_para_rede(textos_tratados)

# Gerar redes
# Redes de cliques de lexemas sobrepostos
redes_finais = gerar_rede_final(textos_redes)

# Redes de cliques de lexemas iniciais (antes da sobreposição)
redes_0 = gerar_rede_0(redes_finais)

# Gerar arestas das relações de sinonímia e hiperonímia
arestas_relacoes = gerar_arestas_relacoes(textos_tratados, textos_redes)

# Adicionar arestas de relações às redes
redes_arestas_finais = redes_finais %>%
  bind_rows(arestas_relacoes)

# Redes de cliques com sinonímia e hiperonímia (antes da sobreposição)
redes_arestas_0 = redes_0

# Converter formato de arestas em formato .net (Pajek)
# Redes de cliques de lexemas iniciais
for (t in 1:(redes_0$texto %>% unique() %>% length())) {
  salvar_net(redes_0 %>% filter(texto == t), paste0('redes/rede_0_', t, '.net'))
}

# Redes de cliques de lexemas finais
for (t in 1:(redes_finais$texto %>% unique() %>% length())) {
  salvar_net(redes_finais %>% 
               filter(texto == t), 
             paste0('redes_news/news_', t, '.net'))
}

# Redes de cliques com sinonímia e hiperonímia iniciais
for (t in 1:(redes_arestas_0$texto %>% unique() %>% length())) {
  salvar_net(redes_arestas_0 %>% 
               filter(texto == t), 
             paste0('redes/rede_arestas_0_', t, '.net'))
}

# Redes de cliques com sinonímia e hiperonímia finais
for (t in 1:(redes_arestas_finais$texto %>% unique() %>% length())) {
  salvar_net(redes_arestas_finais %>% 
               filter(texto == t), 
             paste0('redes/rede_arestas_finais_', t, '.net'))
}

# MÉTRICAS ---------------------------------------------------------------------
redes_tbl = tibble(arquivos = list.files('redes')) %>%
  mutate(texto = arquivos %>% str_match('_(.{1,2})\\.') %>% .[,2],
         tipo = ifelse(arquivos %>% 
                         str_starts('rede_arestas'), 
                       'arestas',
                       'lexemas'),
         momento = ifelse(arquivos %>%
                            str_detect('_0_'),
                          'inicial',
                          'final'))

redes_list = lapply(redes_tbl$arquivos, function(a) {
  read_graph(paste0('redes/', a), format = 'pajek') %>% simplify()
})

redes_tbl = redes_tbl %>%
  # Ordem (numero de vertices)
  mutate(n = sapply(redes_list, gorder),
         # Tamanho (número de arestas)
         e = sapply(redes_list, gsize),
         # Número de componentes
         c = sapply(redes_list, count_components))

# Número de cliques
n_cliques = textos_redes %>%
  group_by(texto, p) %>%
  count() %>%
  group_by(texto) %>%
  count() %>%
  mutate(texto = texto %>% as.character()) %>%
  rename(q = n)

redes_tbl = redes_tbl %>%
  left_join(n_cliques, join_by = 'texto')

# Densidades e fragmentações
redes_tbl = redes_tbl %>%
         # Densidade
  mutate(delta = (2 * e)/(n * (n - 1)),
         # Fragmentação
         F_clique = (c - 1)/ (q - 1))

redes_tbl = redes_tbl %>% 
  select(-arquivos) %>% 
  pivot_wider(names_from = momento, 
              values_from = c(n, e, c, q, delta, F_clique))

# Densidades normalizadas
redes_tbl = redes_tbl %>%
  mutate(delta_norm = (delta_final - delta_inicial)/(1 - delta_inicial),
         v_delta = (delta_final - delta_inicial)/delta_inicial)

# Salvar em csv
write_csv(redes_tbl, 'resultados.csv')

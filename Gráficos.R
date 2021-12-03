library(tidyverse)
library(ggthemes)

theme_set(theme_bw() + theme_hc())

resultados = read_csv('resultados.csv')

g = resultados %>%
  select(tipo, v_delta, F_clique_final) %>%
  mutate(tipo = ifelse(tipo == 'arestas', 
                       'Com tratamento', 
                       'Sem tratamento') %>%
           factor(levels = c('Sem tratamento', 'Com tratamento'))) %>%
  pivot_longer(-tipo, names_to = 'Índice', values_to = 'Valor') %>%
  mutate(Índice = ifelse(Índice == 'v_delta',
                          'Variação de Densidade',
                          'Fragmentação (cliques)')) %>%
  group_by(tipo, Índice) %>%
  summarize(Média = mean(Valor),
            ErroPadrão = sd(Valor)/sqrt(n()),
            Min = Média - 1.96 * ErroPadrão,
            Max = Média + 1.96 * ErroPadrão) %>%
  ggplot(aes(tipo, 
             Média, 
             label = round(Média, 3),
             group = Índice, 
             ymin = Min, 
             ymax = Max)) + 
  geom_line(aes(color = Índice)) + 
  geom_point(aes(color = Índice)) +
  geom_errorbar(aes(color = Índice), width = .05) + 
  ylim(c(0, 1)) + 
  geom_text(nudge_x = .1, nudge_y = .05) +
  xlab('Grupo')

ggsave('figura1.png', 
       plot = g, 
       device = 'png', 
       width = 16, 
       height = 9, 
       units = 'cm')
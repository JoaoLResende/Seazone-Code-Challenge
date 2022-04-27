library(wordcloud2)
library(tidytext)
library(googlesheets4)
library(googledrive)# library(snakecase)
# library(stringi)
# library(ggraph)
# library(igraph)
# library(tm)
# #library(widyr)
# library(janitor)
# library(tidygraph)
# library(lexiconPT)
library(tidyverse)

# palavras <- as.data.frame(c("aberto", "bom relacionamento", "aceita críticas", "falta liderança","compreensivo",
#               "leve","tranquilo", "evita crise","foco em execução","muito operacional", "oferece autonomia",
#               "próximo", "interresado no outro", "receptivo com as pessoas", "falta acompanhamento",
#               "falta direcionamento", "procrastinador", "atraso na distriuição das tarefas")) %>%
#   rename("asd" = 1) %>%
#   mutate(freq = 1)

palavras <- read_sheet("1RzXnDT-xiZI06LH297T_WZ-ygS86SDN51jK5fZ27V6E")%>%
  pivot_longer(names_to = "novo", values_to = "valores", cols = 2:37) %>%
  select(1,3) %>%
  rename("Pessoa" = 1,
         "Palavra" = 2)

# pablo <- as.data.frame(c("boa capacidade técnica", "fala o que pensa","experiente", "versátil", "referência técnica",
#                          "turrão", "comunicação ríspida", "ausência de visão holística")) %>%
#     rename("Palavra" = 1) %>%
#     mutate(total = 1)

William <- palavras %>%
  filter(Pessoa == "William") %>%
    filter(!is.na(Palavra)) %>%
    select(Palavra) %>%
    group_by(Palavra) %>%
    count() %>%
    summarise(total = sum(n))



wordcloud2(data = William, color = 'random-dark', size = 0.1, rotateRatio = 1) #Torta
wordcloud2(data = William, color = 'random-dark', size = 0.3, rotateRatio = 0) #Reto

Claudio <- palavras %>%
  filter(Pessoa == "Cláudio") %>%
  filter(!is.na(Palavra)) %>%
  select(Palavra) %>%
  group_by(Palavra) %>%
  count() %>%
  summarise(total = sum(n))



wordcloud2(data = Claudio, color = 'random-dark', size = 0.2, rotateRatio = 1) #Torta
wordcloud2(data = Claudio, color = 'random-dark', size = 0.3, rotateRatio = 0)


Alexandre <- palavras %>%
  filter(Pessoa == "Alexandre") %>%
  filter(!is.na(Palavra)) %>%
  select(Palavra) %>%
  group_by(Palavra) %>%
  count() %>%
  summarise(total = sum(n))



wordcloud2(data = Alexandre, color = 'random-dark', size = 0.2, rotateRatio = 1) #Torta
wordcloud2(data = Alexandre, color = 'random-dark', size = 0.3, rotateRatio = 0)

Bruno <- palavras %>%
  filter(Pessoa == "Bruno") %>%
  filter(!is.na(Palavra)) %>%
  select(Palavra) %>%
  group_by(Palavra) %>%
  count() %>%
  summarise(total = sum(n))



wordcloud2(data = Bruno, color = 'random-dark', size = 0.2, rotateRatio = 1) #Torta
wordcloud2(data = Bruno, color = 'random-dark', size = 0.3, rotateRatio = 0)

#
# Rodrigo <- palavras %>%
#   filter(Pessoa == "Rodrigo") %>%
#   filter(!is.na(Palavra)) %>%
#   select(Palavra) %>%
#   group_by(Palavra) %>%
#   count() %>%
#   summarise(total = sum(n))
#
#
#
# wordcloud2(data = Rodrigo, color = 'random-dark', size = 0.2, rotateRatio = 1) #Torta
# wordcloud2(data = Rodrigo, color = 'random-dark', size = 0.2, rotateRatio = 0)
#
# wordcloud2(data = pablo, color = 'random-dark', size = 0.2, rotateRatio = 0)
#
#

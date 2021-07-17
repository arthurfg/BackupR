## Arthur de Figueiredo Gusmão - 03/09/2020
### Bible data viz

#### Pacotes ####
library(tidyverse)
library(tidytext)
library(widyr)
library(ggraph)
library(tidygraph)
library(ggtext)
library(rsvg)
extrafont::font_import("",prompt = FALSE)
extrafont::fonts()

extrafont::loadfonts(device = "win", quiet = TRUE)
library(showtext)
font_add_google("Montserrat","montserrat")
font_add_google("Gochi Hand", "gochi")
showtext_auto()
font_add(family = "ibm", regular = "C:/Users/tarci/OneDrive/Documentos/Arthur/test_repo/IBMPlexSerif-Bold.ttf")


chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')
appetizer <- chopped %>%
  select(id = season_episode,appetizer) %>%
  unnest_tokens(word,appetizer,token = "regex",  pattern = "[,\n]") %>%
  mutate(word = str_trim(word, side = "both"))

#Extract and convert the ingredients: entree
entree <- chopped %>%
  select(id = season_episode,entree) %>%
  unnest_tokens(word,entree,token = "regex", pattern = "[,\n]") %>%
  mutate(word = str_trim(word, side = "both"))
#Extract and convert the ingredients: desert
dessert <- chopped %>%
  select(id = season_episode,dessert) %>%
  unnest_tokens(word,dessert,token = "regex",  pattern = "[,\n]") %>%
  mutate(word = str_trim(word, side = "both"))

words <- bind_rows(appetizer,entree,dessert) %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
  filter(n > 5) %>%
  igraph::graph_from_data_frame() 


bible <- cross_references 
bible[3] <- NULL

bible <- bible %>%
  igraph::graph_from_data_frame()

p1 <- ggraph(bible, layout = 'linear') + 
  geom_edge_arc(aes(color=as.factor(Votes), width=Votes), alpha=.2) 
p1

p1 <- ggraph(words, layout = 'linear') + 
  geom_edge_arc(aes(color=as.factor(n), width=n), alpha=.2) +
  theme_void() +
  scale_color_manual(values = rainbow(5)) +
  geom_node_text(aes(label = name), vjust = 1,
                 hjust = 1, angle = 90, size = 3,
                 color = 'white', alpha = .8) +
  scale_edge_width(range = c(.5,1.5))


cookExample <- readr::read_csv("https://raw.githubusercontent.com/KellyTall/Hellomister_DataBlog/master/cookExample.csv")

cook_edge <- cookExample %>% 
  mutate(from = 1770) %>% 
  select(from, Dedication_Year, Figure, Monument_Type) %>% 
  rename(to=Dedication_Year) %>% 
  mutate(to=as.numeric(to)) %>% 
  group_by(Figure, Monument_Type, from, to) %>% 
  summarise(weight = n()) %>% 
  na.omit()

cook_tidy <- tbl_graph(edges=cook_edge, directed = TRUE)

library(grid)
text_high <- textGrob("Highest\nvalue", gp=gpar(fontsize=8, fontface="bold"))


bible_tidy <- tbl_graph(edges = bible_2, directed = T)

cook1 <- ggraph(cook_tidy, layout = 'linear') + 
  geom_edge_arc()+
  annotate('text', x = 1770, y = -1, label = 'Gen 1.1', size = 6, angle = 90)

cook1                        

bible <- cross_references %>%
  filter(str_detect(From.Verse,'Gen'))
 bible$Votes <- NULL
 
showtext.begin()
 p1 <- ggraph(bible_tidy, layout = 'linear') + 
   geom_edge_arc(alpha=.6,aes(color = as.factor(to))) +
   theme_void() +
   scale_color_manual(values = rainbow(10)) +
   geom_node_text(aes(label = name), vjust = 0,
                  hjust = 1, angle = 45, size = 0.3,
                  color = 'white', family = "Helvetica", alpha = .8) +
   scale_edge_width(range = c(.5,1.5))+
   geom_vline(xintercept=c(1,427,722,1069), colour="white", alpha = .4)+
   geom_text(aes(x=1, label="\nGen1.1", y=-400), colour="white", angle=90, size=2.5)+
   geom_text(aes(x=427, label="\nGen2.1", y=-400), colour="white", angle=90, size=2.5)+
   geom_text(aes(x=722, label="\nGen3.1", y=-400), colour="white", angle=90, size=2.5)+
   geom_text(aes(x=1069, label="\nGen4.1", y=-400), colour="white", angle=90, size=2.5)+
   #geom_text(aes(x=2385, label="\nGen1.1", y=-400), colour="white", angle=90, size=3)+
   #annotate('text', x = 1, y = 0, label = 'Gen 1.1', size = 6, angle = 90)+
   labs(title = "A beleza da palavra :)",
        subtitle = "Referências cruzadas encontradas em Gênesis 1-5",
        caption = "Dados da 'Open Bible' (www.openbible.info/labs/cross-references)
       Visualização por Arthur Gusmão (Twitter @ttuquinho)
       Código disponível em github.com/tucoooo/test_repo") + 
   theme(legend.position = 'none',
         plot.background = element_rect(fill='#1c2733', color = '#1c2733'),
         panel.background = element_rect(fill='#1c2733', color = '#1c2733'),
         plot.title = element_text(color = "white", family = "montserrat", size = 24, hjust = 0.5, face='bold'),
         plot.subtitle = element_text(color = "white", family = "ibm", size = 13, hjust = 0.5),
         plot.caption = element_text(color = "white", family = "ibm", size = 10, hjust = 0.95, face = 'bold'),
         plot.margin=unit(c(0.5,0.5,1,0),"cm"))
p1 

biblia <- bibllia
biblia$X1 <- NULL

bible_2 <- bible[1:1493,]

regexp <- "[[:digit:]]+"

# process string
str_extract("Gen.2.5", regexp)

bible_2 <- bible_2 %>%
  mutate(cap = str_extract(From.Verse,regexp))

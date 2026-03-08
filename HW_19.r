rm(list = ls())

library(tidyverse)
library(udpipe)
library(igraph)
library(scales)

caesar_subset <- udpipe_read_conllu(
  "https://github.com/locusclassicus/text_analysis_2024/raw/main/files/bg_latinpipe.conllu"
) |>
  filter(upos == "NOUN", !is.na(lemma)) |>
  mutate(lemma = str_to_lower(lemma))

edges <- cooccurrence(
  x = caesar_subset,
  term = "lemma",
  group = c("doc_id", "sentence_id")
) |>
  as_tibble() |>
  filter(cooc >= 35) |>
  arrange(desc(cooc)) |>
  slice_head(n = 20) |>
  transmute(from = term1, to = term2, weight = cooc)

nodes <- caesar_subset |>
  count(lemma, name = "freq") |>
  rename(name = lemma)

g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
g <- delete_vertices(g, degree(g) == 0)

V(g)$degree <- degree(g)
V(g)$size <- rescale(V(g)$degree, to = c(18, 34))
V(g)$label <- V(g)$name

E(g)$width <- rescale(E(g)$weight, to = c(1.5, 7))
E(g)$color <- case_when(
  E(g)$weight >= 60 ~ "#7A0019",
  E(g)$weight >= 45 ~ "#CC0000",
  TRUE ~ "#F4A6B5"
)

# V = Vertices (вершины, узлы графа)
# E = Edges (ребра, связи)

set.seed(21092024)
lay <- layout_with_kk(g)

png("caesar_network.png", width = 1700, height = 1200, res = 180)

plot(
  g,
  layout = lay,
  vertex.size = V(g)$size,
  vertex.label = V(g)$label,
  vertex.label.cex = 1,
  vertex.label.color = "black",
  vertex.label.dist = 0.5,
  vertex.color = "#D4AF37",
  vertex.frame.color = "black",
  edge.width = E(g)$width,
  edge.color = E(g)$color,
  main = "⚜ Сеть совместной встречаемости существительных в Записках Цезаря ⚜"
)

legend(
  "bottomleft",
  legend = c("Сильная связь 😎", "Средняя связь 🙂", "Слабая связь 🥺👉👈"),
  col = c("#7A0019", "#CC0000", "#F4A6B5"),
  lwd = 4,
  bty = "n"
)

dev.off()


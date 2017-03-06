
library(igraph)
library(tidyverse)
library(corrr)
library(ggraph)



d <- mtcars

cors <- d %>% 
  correlate %>% 
  stretch

gr <- cors %>%
  filter(! is.na(r)) %>% 
  mutate(r = abs(r)) %>% 
  graph_from_data_frame(directed = FALSE)

ggraph(gr, layout = 'drl') +
  geom_edge_link(aes(alpha = abs(r), width = abs(r), color=r)) +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = T) +
  scale_edge_color_gradient2() +
  coord_equal() +
  theme_graph()



# ------------------


e <- tribble(
  ~x, ~y,
  1,2,
  2,3,
  3,4,
  3,5,
  3,6,
  2,7,
  1,8,
  8,9,
  9,10,
  9,11,
  11,12
)
v <- tribble(
  ~name, ~depth,
  1,0,
  2,1,
  3,2,
  4,3,
  5,3,
  6,3,
  7,2,
  8,1,
  9,2,
  10,3,
  11,3,
  12,4
)

gr <- graph_from_data_frame(e, vertices = v, directed = T)

ggraph(gr, layout='partition', circular = T) +
  theme_graph() +
  coord_equal() +
  geom_node_arc_bar(aes(fill=depth))

ggraph(gr, layout='partition') +
  theme_graph() +
  coord_equal() +
  geom_node_tile(aes(fill=depth)) +
  scale_y_reverse()

ggraph(gr, layout='partition') +
  theme_graph() +
  coord_equal() +
  geom_edge_link() +
  geom_node_point(size = 7) +
  geom_node_text(aes(label = name), col='white') +
  scale_y_reverse()


ggraph(gr, layout='circlepack', weight = 'depth') +
  theme_graph() +
  coord_equal() +
  geom_node_circle(aes(fill = depth)) +
  geom_node_text(aes(label = name, size = max(depth) - depth))

ggraph(gr, layout='circlepack', weight = 'depth') +
  theme_graph() +
  coord_equal() +
  geom_edge_link() +
  geom_node_point(aes(size = max(depth) - depth))

ggraph(gr, layout='dendrogram') +
  theme_graph() +
  coord_equal() +
  geom_edge_diagonal()


######

dd <- as.dendrogram(agnes(iris[-5], diss = F, method = 'ward'))

ggraph(dd, layout='dendrogram') +
  theme_graph() +
  geom_edge_diagonal()

ggraph(dd, layout='dendrogram', repel = TRUE) +
  theme_graph() +
  geom_edge_diagonal()

ggraph(dd, layout='dendrogram', repel = T) +
  theme_graph() +
  geom_edge_elbow() +
  scale_y_reverse() +
  coord_flip()

ggraph(dd, layout='dendrogram') +
  theme_graph() +
  geom_edge_link()

ggraph(dd, layout='dendrogram', circular = TRUE) +
  coord_equal() +
  theme_graph() +
  geom_edge_elbow()


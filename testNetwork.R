library(ape)
library(tanggle)
library(phangorn)
library(targets)
library(tidyverse)
library(ggrepel)

d <-
  tar_read(d) %>%
  # subset to behavioural decisions
  dplyr::select(starts_with("pun"), strategy) %>%
  # reduce to 403 unique vectors with counts
  group_by_at(vars(starts_with("pun"))) %>%
  summarise(
    count = n(),
    strategy = unique(strategy),
    .groups = "drop"
    ) %>%
  # keep groups with more than 1 count
  filter(count > 1)
# n is number of unique patterns of behaviour
n <- nrow(d)
# calculate hamming distance matrix
dist <- matrix(NA, nrow = n, ncol = n)
for (i in 1:n) {
  for (j in 1:n) {
    dist[i,j] <- sum(d[i,1:12] != d[j,1:12])
  }
}
rownames(dist) <- colnames(dist) <- 1:n
# create neighbour net
net <- neighborNet(dist)
# plot splits graph
p <- 
  ggsplitnet(
    net,
    colour = "grey90",
    linewidth = 0.05
    )
# labels for strategies
labelStrategies <- 
  c(
    "Anti-punish"                    = "Anti-punish\n(000000000000)",
    "Seek advantageous inequity"     = "Seek AI\n(001010000000)",
    "Egalitarian"                    = "Egalitarian\n(000000001010)",
    "Avoid disadvantageous inequity" = "Avoid DI\n(000000001000)",
    "Retributive"                    = "Retributive\n(101010101000)",
    "Deterrent"                      = "Deterrent\n(101000101000)",
    "Norm-enforcing"                 = "Norm-enforcing\n(101000101010)",
    "Competitive"                    = "Competitive\n(111111001111)"
  )
# add the data
pd <- 
  tibble(
    tip = as.character(1:n),
    Count = d$count,
    strategy = ifelse(d$strategy == "N/A", "", labelStrategies[as.character(d$strategy)]),
    exact = strategy != ""
  ) %>%
  left_join(p$data, by = c("tip" = "label"))
p <-
  p +
  geom_point(
    data = pd,
    aes(
      x = x,
      y = y,
      size = Count,
      colour = exact
    )
  ) +
  geom_text_repel(
    data = pd,
    aes(label = strategy),
    colour = "red",
    size = 3.5,
    seed = 1
  ) +
  scale_colour_discrete(type = c("black","red")) +
  scale_size_continuous(breaks = c(10, 100, 500)) +
  guides(colour = "none") +
  theme(legend.position = c(0.9, 0.15))

  
ggsave(p, filename = "splitsGraph.pdf", width = 7, height = 7)

#annotate("text", x = -3.7, y = -2.4, label = "Anti-punish\n(000000000000)"   , colour = "red", size = 3) +
#annotate("text", x = -0.4, y = -3.5, label = "Seek AI\n(001010000000)"       , colour = "red", size = 3) +
#annotate("text", x =  2.9, y = -1.2, label = "Egalitarian\n(000000001010)"   , colour = "red", size = 3) +
#annotate("text", x =  3.0, y =  0.0, label = "Avoid DI\n(000000001000)"      , colour = "red", size = 3) +
#annotate("text", x =  3.2, y =  1.6, label = "Retributive\n(101010101000)"   , colour = "red", size = 3) +
#annotate("text", x =  3.1, y =  2.5, label = "Deterrent\n(101000101000)"     , colour = "red", size = 3) +
#annotate("text", x =  1.9, y =  2.9, label = "Norm-enforcing\n(101000101010)", colour = "red", size = 3) +
#annotate("text", x =  1.0, y =  7.4, label = "Competitive\n(111111001111)"   , colour = "red", size = 3) +
  

## multidimensional scaling of distances
#mds <- cmdscale(dist, k = 2)
## plot
#d %>%
#  mutate(
#    mds1 = mds[,1],
#    mds2 = mds[,2]
#  ) %>%
#  unite(
#    col = "pattern",
#    starts_with("pun"),
#    sep = ""
#  ) %>%
#  mutate(
#    exact = ifelse(strategy == "N/A", FALSE, TRUE),
#    strategy = ifelse(strategy == "N/A", NA, as.character(strategy))
#    ) %>%
#  ggplot(aes(x = mds1, y = mds2, colour = exact, label = strategy)) +
#  geom_point(aes(size = log1p(count))) +
#  geom_text_repel(box.padding = 1) +
#  scale_color_discrete(type = c("grey85","red")) +
#  theme_blank() +
#  theme(legend.position = "none")

#library(phangorn)
#library(tanggle)
#library(tidyverse)
#set.seed(1)
#n <- 20
## get data frame
#d <- data.frame(
#  x1 = sample(0:1, size = n, prob = c(0.3, 0.7), replace = TRUE),
#  x2 = sample(0:1, size = n, prob = c(0.3, 0.7), replace = TRUE),
#  y1 = sample(0:1, size = n, prob = c(0.3, 0.7), replace = TRUE),
#  y2 = sample(0:1, size = n, prob = c(0.3, 0.7), replace = TRUE),
#  z1 = sample(0:1, size = n, prob = c(0.3, 0.7), replace = TRUE),
#  z2 = sample(0:1, size = n, prob = c(0.3, 0.7), replace = TRUE)
#) %>%
#  # reduce to unique vectors with count
#  group_by_all() %>%
#  summarise(count = n(), .groups = "drop")
## n_unique = number of rows in d
#n_unique <- nrow(d)
## calculate hamming distance matrix
#dist <- matrix(NA, nrow = n_unique, ncol = n_unique)
#for (i in 1:n_unique) {
#  for (j in 1:n_unique) {
#    dist[i,j] <- sum(d[i,] != d[j,])
#  }
#}
#rownames(dist) <- colnames(dist) <- 1:n_unique
## multidimensional scaling of distances
#mds <- cmdscale(dist, k = 2)
#
#d %>%
#  mutate(mds1 = mds[,1], mds2 = mds[,2]) %>%
#  ggplot(aes(x = mds1, y = mds2, size = count)) +
#  geom_point() +
#  theme_blank()

## hierarchical clustering on distance matrix
#hc <- hclust(as.dist(dist))
#plot(hc)

# minimum spanning tree
#library(igraph)
#library(ggnetwork)
#dist %>%
#  # get adjacency matrix (minimum spanning tree)
#  ape::mst() %>%
#  # convert to graph
#  graph_from_adjacency_matrix(mode = "undirected") %>%
#  # plot
#  ggnetwork() %>%
#  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#  geom_edges() +
#  geom_nodes() +
#  geom_nodetext_repel(aes(label = name))


## get neighbour net
#net <- neighborNet(dist)
## plot splits graph
#d <- 
#  d %>%
#  rownames_to_column("node") %>% 
#  transmute(
#    label = as.integer(node),
#    count = count
#    )
#p <-
#  ggsplitnet(net) %<+% d +
#  geom_tippoint(aes(colour = count))

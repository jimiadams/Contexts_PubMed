################################################################
# This script pulls in data for disambiguation routine (step 2), 
# builds coauthorship networks, and identifies communities
################################################################

library(dplyr)

load("data/author_long2.Rda")

# Let's build a network
  library(igraph)
  m <- pma2 %>% select(pmid, nid)
  
  auth_num <- as.data.frame(table(m$nid))
  
  m$pmid <-  paste0("m",m$pmid)
  
  g <- graph.data.frame(m)

  V(g)$type <- FALSE
  V(g)$type[V(g)$name%in%m$pmid] <- TRUE
  
  V(g)$type <- bipartite.mapping(g)$type
  
# Creating the author projection
  proj_g <- bipartite_projection(g, multiplicity = TRUE)
  ag <- proj_g$proj2
  V(ag)$size <- auth_num$Freq #adding n-pubs as Vertex attribute

###################################################################
# Clustering and plotting the authors graph (here we're using the 2 biggest clusters)
  aclust <- clusters(ag)
  csize <- table(table(aclust$membership))

# Grabbing just the largest connected component
  alcc <- induced.subgraph(ag, V(ag)[which(aclust$membership == which.max(aclust$csize))])
  alcc <- simplify(alcc, remove.multiple=F) # removing loops



library(graphlayouts)
  

scomm <- cluster_fast_greedy(alcc)

scomm2 <- cut_at(scomm, 5)

sizes(scomm)

V(alcc)$color <- scomm2+1


library(ggraph)


play <- layout_with_pmds(alcc, pivots=100)
  
ggraph(alcc,layout = play)+
  geom_edge_link0(edge_colour = "grey92")+
  geom_node_point(col=V(alcc)$color,size=1)+
  theme_graph()


#find backbone. use disparity_filter function included in scripts

bg <- disparity_filter(alcc, alpha=.2)

bclust <- components(bg)

bglcc <- induced.subgraph(bg, V(bg)[which(bclust$membership == which.max(bclust$csize))])
bglcc <- simplify(bglcc, remove.multiple=F) # removing loops

bcomm <- cluster_fast_greedy(bglcc)

bcomm2 <- cut_at(bcomm, 8)


V(bglcc)$community <- bcomm2

colrs <- c("cadetblue2" , "darkseagreen", "goldenrod1", "tomato2", "darkolivegreen1", "darkorchid3", "blue", "pink3")

V(bglcc)$color <- colrs[V(bglcc)$community] 

bg_plot <- ggraph(bglcc,layout = "stress")+
  geom_edge_link0(edge_colour = "grey92")+
  geom_node_point(col=V(bglcc)$color,size=1)+
  theme_graph()

ggsave(bg_plot, file="figs_contexts/databackbone.jpg")

#now merge backbone communities back into bigger graph

bname <- as.numeric(V(bglcc)$name)
bcolor <- V(bglcc)$color
bcommunity <- V(bglcc)$community

bmerge <- as.data.frame(cbind(bname, bcolor, bcommunity))

bmerge$bname <- as.numeric(bmerge$bname)

aname <- as.data.frame(as.numeric(V(alcc)$name))

names(aname)[1] <- "name"

amerge <- left_join(aname, bmerge, (by=c("name"="bname")))

library(tidyr)

amerge$bcolr <- amerge$bcolor %>% replace_na("grey66")
amerge$alpha <- ifelse(amerge$bcolr=="grey66", .01, 1)


V(alcc)$bcolor <- amerge$bcolr

talcc_plot <- ggraph(alcc,layout = play)+
  geom_edge_link0(edge_colour = "grey92", alpha=.01)+
  geom_node_point(col=V(alcc)$bcolor, alpha=amerge$alpha, size=1)+
  theme_graph()

ggsave("figs_contexts/transp_lot.jpg", talcc_plot)

library(cowplot)

p <- plot_grid(talcc_plot, bg_plot, labels="AUTO")

ggsave("figs_contexts/panel.jpg", p)

#save the amerge file for merging back in with pma2 for more info about each community

save(amerge, file = "data/commemb.Rda")



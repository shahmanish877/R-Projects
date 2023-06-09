library(tidyverse)
library(igraph)

load("D:\\R programming runs\\Assisgnments\\Assignment 3\\Manish Shah - termDocMatrix.rdata")

str(termDocMatrix)
# It is a term document matrix with Terms & Values(Docs)

# first 10 row & 10 col
termDocMatrix[1:10, 1:10]

# tdm to matrix
term_matrix <- as.matrix(termDocMatrix)

#Change this matrix to a Boolean matrix 
term_matrix[term_matrix >= 1] <- 1


#Transform this matrix to a term-term adjacency
term_adj_matrix <- term_matrix %*% t(term_matrix)
term_adj_matrix[1:10, 1:10]

g <- graph.adjacency(term_adj_matrix, mode = "undirected", weighted = T)

#simplify - remove loops
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

#vertex labels with names of g,
V(g)$label <- V(g)$name

#set degree with the degree of g
V(g)$degree <- degree(g)

degree_all <- degree(g, mode = "all")
degree_in <- degree(g, mode = "in")
degree_out <- degree(g, mode = "out")


#diameter of g
diameter(g)

#edge density
edge_density(g)

#reciprocity
reciprocity(g)

#closeness
closeness(g)

#betweeness
betweenness(g)


hist(degree_all, main = "Node Degree Histogram", xlab = "Node Degree")

set.seed(16)
plot(g, vertex.size = degree_all*0.4)


plot(g, layout=layout.fruchterman.reingold)
plot(g, layout=layout.kamada.kawai)


hub <- hub_score(g)
authority <- authority.score(g)


# Plot hub scores
par(mfrow=c(1,2))
plot(hub$vector)
plot(authority$vector)
par(mfrow=c(1,1))

# Detect communities
fc <- cluster_fast_greedy(g)

# Plot communities
plot(fc, g, vertex.color = fc$membership, main = 'Community Detection')


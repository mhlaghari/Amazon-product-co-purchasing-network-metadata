library(igraph)
library(tidyverse)
library(dplyr)
library(skimr)
library(ggplot2)
library(stringr)
library(scales)
library(psych)


graph1 <- read.graph(file="/Users/mhlaghari/Downloads/data 3/graph_subset_rank1000.txt", format ="ncol")

#1355 nodes; 2611 edges
graph1

#to verify nodes and edges
cat("Nodes: ", vcount(graph1),"\n") #for nodes
cat("Edges: ", ecount(graph1)) #for edges

#Get nodes
V(graph1)

#Get Edges
E(graph1)

#is the graph directed?
is.directed(graph1)

plot.igraph(graph1,layout=layout.kamada.kawai,
            edge.width= 2,
            edge.color="darkgreen", 
            vertex.color="red", 
            vertex.size=2, 
            vertex.label.color="black", 
            vertex.label.cex=0.6, 
            vertex.label.dist=2)

graph2 <- read.graph(file="/Users/mhlaghari/Downloads/data 3/graph_subset_rank1000_cc.txt", format ="ncol")
graph2

# Is the graph directed?
is.directed(graph2)

cat("Nodes: ", vcount(graph2),"\n") #for nodes
cat("Edges: ", ecount(graph2)) #for edges

plot.igraph(graph2,layout=layout.kamada.kawai,
            edge.width= 2,
            edge.color="darkgreen", 
            vertex.color="gold", 
            vertex.size=3, 
            vertex.label.color="black", 
            vertex.label.cex=0.6, 
            vertex.label.dist=2)

data <- read.csv('/Users/mhlaghari/Downloads/data 3/graph_complete.txt', sep = ' ', header = FALSE)
id <- read.csv('/Users/mhlaghari/Downloads/data 3/id_to_titles.txt', sep=' ')

# create a directed graph
graph3 <- read.graph(file="/Users/mhlaghari/Downloads/data 3/graph_complete.txt", format ="ncol")
graph3 <- as.directed(graph3)
graph3

# Is the graph directed?
is.directed(graph3)

cat("Nodes: ", vcount(graph3),"\n") #for nodes
cat("Edges: ", ecount(graph3)) #for edges

#method 1
#Step a
out1 <- table(data$V1)

#Step b
out_degree <- data.frame(table(out1))
out_degree$out1<- as.numeric(out_degree$out1)

#Step c
ggplot(out_degree ,aes(x=out1, y=Freq)) +
  geom_point()+ geom_line()+
  labs(x='similar products', y='number of nodes')+
  ggtitle("Out-degree distibution")


# step a
in1 <- table(data$V2)

# step b
in_degree <- data.frame(table(in1))
in_degree$in1<- as.numeric(in_degree$in1)

# step c
ggplot(in_degree ,aes(x=in1, y=Freq), group=1) +
  geom_point()+ geom_line()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x='similar products', y='number of nodes')+
  ggtitle("In-degree distibution")

# plot previous graph by transforming x-axis to log scale

ggplot(in_degree ,aes(x=log(in1), y=Freq), group=1) +
  geom_point()+ geom_line()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x='similar products', y='number of nodes')+
  ggtitle("In-degree distibution (log scale)")

paste("The Average number of inbound co-purchase links is:",round(mean(in1), 1))
paste("The Standard deviation of inbound co-purchase links is:",round(sd(in1), 1))
paste("The maximum of inbound co-purchase links is:", max(in1))

in1_df <- data.frame(in1)
in1_df$Var1<- as.numeric(in1_df$Var1)

# join in-degree data frame with Title 
in_deg_title <- in1_df %>%inner_join(id, by = c("Var1" = "id"))

# find top 10 product with the most inbound links
top10 <-as.data.frame(head(in_deg_title[order(in_deg_title$`Freq`, decreasing = T),],n=10))

paste("The name of top 10 products with most inbound co-purchase are : ")
paste(top10$title, sep = '\n')

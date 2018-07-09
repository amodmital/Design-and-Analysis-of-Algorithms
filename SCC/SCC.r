#Assignemtnt to count the minimum cut for a undirected graph using Krager min cut algorithm

library("data.table")

input_file<-"SCC_test.txt"


edge<-read.table(input_file)

# for mediumDG problem
#  edge$V1<-edge$V1+1
#  edge$V2<-edge$V2+1


# to create adjacency list
  edge_t<-tapply(edge$V2,edge$V1,function(x) c(x))
  adjlist<-data.frame(index=as.integer(rownames(edge_t)))
  adjlist$edge<-vector(mode="list",length(length(edge_t)))  
  for (x in 1:length(edge_t))
    {
      adjlist$edge[[x]]<-edge_t[[x]]
    }

#setup the dataframe node - containing the start and end index of every node in the edge list
  #rm(node)
  n<-max(edge$V1,edge$V2)
  node<-data.frame(index=c(1:n))
  node$explored<-vector(mode="logical",length = n)  

  #old code
    node_freq<-as.data.frame(table(edge[1]))
    names(node_freq)<-c("index","freq")
    node_freq$end<-cumsum(node_freq$freq)
    n_freq<-length(node_freq$index)
    node_freq$start<-rep(1,n_freq)
  
  
  
    node_freq<-data.table(node_freq)
    node_freq[,start:= c(1,end[seq_len(.N-1)]+1)]
  
  node<-merge(node, node_freq, by = 'index', all.x=TRUE)


d<-list(s=1, pass = 1, node=node, edge=edge)


dfs<-function(x)
{
  if(is.na(x$node$start[x$s]))
  {x$node$explored[x$s]<-TRUE}
  
  if (x$node$explored[x$s])
    {return(x)}  
  else
  {
    x$node$explored[x$s]<-TRUE
    for(y in x$node$start[x$s]:x$node$end[x$s])
    {
      x$s<-x$edge[y,2]
      x<-dfs(x)
    }
    return(x)
  }
}

dfsw<-function(x)
{
  for (z in min(x$node$index):max(x$node$index))
  {
    x$s<-z
    x<-dfs(x)
  }
  return(x)
}

a<-dfsw(d)

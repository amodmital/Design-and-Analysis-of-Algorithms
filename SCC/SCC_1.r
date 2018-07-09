library("data.table", lib.loc="C:/Program Files/R/R-3.0.1/library")


edge<-read.table("C:/Sabre/Internal/Design and Analysis of Algorithms/SCC/mediumDG.txt")

# for mediumDG problem
  edge$V1<-edge$V1+1
  edge$V2<-edge$V2+1


# to create adjacency list
  edge_t<-tapply(edge$V2,edge$V1,function(x) c(x))
  adjlist<-data.frame(index=as.integer(rownames(edge_t)),edge=edge_t)
  
#setup the dataframe node - containing the start and end index of every node in the edge list
  rm(node)
  n<-max(edge$V1,edge$V2)
  node<-data.frame(index=c(1:n))
  node$explored<-vector(mode="logical",length = n)  

  #old code
    #node_freq<-as.data.frame(table(edge[1]))
    #names(node_freq)<-c("index","freq")
    #node_freq$end<-cumsum(node_freq$freq)
    #n_freq<-length(node_freq$index)
    #node_freq$start<-rep(1,n_freq)
  
  
  
    #node_freq<-data.table(node_freq)
    #node_freq[,start:= c(1,end[seq_len(.N-1)]+1)]
  
  node<-merge(node, adjlist, by = 'index', all.x=TRUE)


#d<-list(s=1, pass = 1, node=node)


dfs_1<-function(x,s)
{
  if(is.na(x$edge[[s]][1]))
  {x$explored[s]<-TRUE
    #cat("No outgoing links",s,"\n")
  }
  
  if (x$explored[s])
    {return(x)}  
  else
  {
    x$explored[s]<-TRUE
    #cat("Explored",s,"\n")
    vec_loop<-x$edge[[s]]
    for(y in 1:length(vec_loop))
    {
      s<-vec_loop[y]
      x<-dfs_1(x,s)
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
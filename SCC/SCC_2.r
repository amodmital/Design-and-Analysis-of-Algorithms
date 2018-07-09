library("data.table")
#assumption: no nodes indexed as 0

edge<-read.table("/Users/amod/SCC.txt")
 
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
node$edge<-list(c(2,3),c(3,4),4,NA,NA,NA,c(3,8,5,9),NA,10,NA)

dfs<-function(x,s)
{
  # remove linkages to already explored nodes from exploration vector
    s<-s*!x$explored[s]
    s<-s[s!=0]
  
  # set nodes with no outgoing links as explored
  #  node$explored[s]<-is.na(node$edge[s])
  #  cat("No outgoing links",s,"\n")
  
  # exit if no more links to traverse
  if (length(s)!=0)
  {
    x$explored[s]<-TRUE
    #cat("Explored",s,"\n")
    vec_loop<-unique(unlist(x$edge[s]))
    # remove nodes with no outgoing links
      vec_loop<-vec_loop[!is.na(vec_loop)]
    
    # exit if not more links to traverse
    if (length(vec_loop)!=0)
    {
      #option 0:
      x<-dfs(x,vec_loop)
      
      #option 1:
      #x_b<-lapply(vec_loop, dfs, x=x)
      #x_c<-as.data.frame(lapply(x_b, function(x) (x$explored)))
      #x$explored<-as.logical(apply(x_c,1,max))
    }
    
  }
  x  
}

system.time(a<-dfs(node,node$index))
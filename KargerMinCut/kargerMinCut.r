
#Assignemtnt to count the minimum cut for a undirected graph using Krager min cut algorithm

library("data.table")

input_file<-"kargerMinCut.txt"

d1 <- strsplit(readLines(input_file),"\t")
d1<-lapply(d1,as.numeric)

d<-list(mincut=0,grid=lapply(d1,function(x) x[2:length(x)]),vertex=lapply(d1,function(x) x[[1]]))



kargerMinCut<-function(x)
{
    if(length(x$vertex)==2)
    {
        x$mincut<-length(x$grid[[1]])
        return(x)
    }
        
    #m contains 2m (twice the number of edges)
    m<-sapply(x$grid,function(x) length(x))
    #rn is the random edge to be used for contraction
    rn<-sample(1:sum(m),1)
    
    #remove the edge
        #get coordinates of link to be removed
        cum_m<-cumsum(m)
        i<-1 #row of element
        repeat
        {
            if(rn>cum_m[i])
            {
                i<-i+1    
            }
            else
            {
                j<-rn-max(cum_m[i-1],0) #column of element
                break 
            }
        }
        keep<-x$vertex[[i]]
        v_keep<-i
        drop<-x$grid[[i]][j]
        for (z in 1:length(x$vertex))
        {if (x$vertex[[z]]==drop)
            break
        }
        v_drop<-z
    
        #copy links from dropped vertex to kept vertex
        x$grid[[v_keep]]<-c(x$grid[[v_keep]],x$grid[[v_drop]])
    
        #change references to dropped vertex to kept vertex
        x$grid<-lapply(x$grid,function(x) sapply(x, function(x) if(x==drop) keep else x[[1]]))
    
    #remove loops
    x$grid[[v_keep]]<-x$grid[[v_keep]][!x$grid[[v_keep]] %in% c(keep)]
    
    
    #contract the graph
    x$vertex[[v_drop]]<-NULL
    x$grid[[v_drop]]<-NULL
    
    #recursive call on contracted graph
    x<-kargerMinCut(x)
    
    return(x)

}


l<-c(1:100)
for(i in 1:100)
{
	l[i]<-kargerMinCut(d)$mincut
}

min(l)

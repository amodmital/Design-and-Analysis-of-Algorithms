#Assignemtnt to counting number of inversions in an input file which has a sequence of integers

input_file<-"IntegerArray.txt"

d1<-c(read.table(input_file,header=FALSE)[[1]])

#function for sorting an array
sortarray<-function(x)
{
    l_x<-length(x)
    if (l_x==1)
        return (x)
    else 
        {
            if (l_x%%2==0)
                n1<-l_x/2
            else
                n1<-(l_x+1)/2
            
            a1<-sortarray(x[1:n1])
            a2<-sortarray(x[(n1+1):l_x])
             
            i<-1
            j<-1
            tmp<-vector(mode="integer",l_x)
            for(k in 1:l_x)
            {
                if((a1[min(i,n1)]<=a2[min(j,l_x-n1)] & i<=n1) | j>l_x-n1)
                {
                    tmp[k]<-a1[i]
                    i<-i+1
                }
                else
                {
                    tmp[k]<-a2[j]
                    j<-j+1
                }
            }
            return(tmp)
        }
}

d2<-sortarray(d1)





#function for Counting Inversions

d1<-list(0,c(read.table(input_file,header=FALSE)[[1]]))

countinv<-function(x)
{
    l_x<-length(x[[2]])
    if (l_x==1)
        return (x)
    else 
    {
        if (l_x%%2==0)
            n1<-l_x/2
        else
            n1<-(l_x+1)/2
        
        
        a1<-countinv(list(x[[1]],x[[2]][1:n1]))
        a2<-countinv(list(x[[1]],x[[2]][(n1+1):l_x]))
        
        i<-1
        j<-1
        tmp<-list(a1[[1]]+a2[[1]],vector(mode="integer",l_x))
        for(k in 1:l_x)
        {
            if((a1[[2]][min(i,n1)]<=a2[[2]][min(j,l_x-n1)] & i<=n1) | j>l_x-n1)
            {
                tmp[[2]][k]<-a1[[2]][i]
                i<-i+1
            }
            else
            {
                tmp[[2]][k]<-a2[[2]][j]
                tmp[[1]]<-tmp[[1]]+n1-i+1
                j<-j+1
            }
        }
        return(tmp)
    }
}

d2<-countinv(d1)

d2[1]

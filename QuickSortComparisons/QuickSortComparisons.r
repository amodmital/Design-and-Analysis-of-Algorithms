#Assignemtnt to count the number of comparisons used to sort an array of integers using quicksort
# part 1 - using first element of array as pivot element
# part 2 - using last element of array as pivot element

library("data.table")

input_file<-"QuickSort.txt"


d1<-list(comparisons=0,array=c(read.table(input_file,header=FALSE)[[1]]))

qsortcomp<-function(x, start_x, end_x)
{
    if(start_x == end_x)
        return(x)
    else
    {
        #accumulate number of comparisons
        x$comparisons<-x$comparisons + (end_x - start_x)
        
        #assigning the pivot
        mid_x<-floor((start_x+end_x)/2)
        
        t<-data.table(v_x=c(start_x,mid_x,end_x),v=c(x$array[start_x],x$array[mid_x],x$array[end_x]))
        setkey(t,v)
        pivot_x<-t$v_x[2]
        #pivot_x<-end_x
        
        #swaping first element with the pivot
            tmp<-x$array[start_x]
            x$array[start_x]<-x$array[pivot_x]
            x$array[pivot_x]<-tmp
        
        #initialize parition index
        i<-start_x
        
        #single pass of array
        j<-start_x
        repeat
        {
            j<-j+1
            if(x$array[j]<=x$array[start_x])
            {
                i<-i+1
                #swap elements at posistions i and j
                    tmp<-x$array[j]
                    x$array[j]<-x$array[i]
                    x$array[i]<-tmp
            }
            
            if(j==end_x)
            {break}
        }
        
        #swaping element i with the start_x (which stores pivot)
            tmp<-x$array[start_x]
            x$array[start_x]<-x$array[i]
            x$array[i]<-tmp
  
        #recursive calls based on pivot in position i
        if((i-start_x-1)>=0)
            x<-qsortcomp(x, start_x, i-1)
        if((end_x-i-1)>=0)
            x<-qsortcomp(x,i+1,end_x)
    }
    return(x)
}


d2<-qsortcomp(d1,1,length(d1$array))
d2$comparisons
#162085
#164123


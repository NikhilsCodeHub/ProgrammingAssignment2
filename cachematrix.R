## This file defines 2 functions to generate and cache Inverse of Matrix.
##  Nikhil W
##
## makeCacheMatrix --- Creates a List Object of Matrix and it's Inverse.
## cacheSolve --- Based on the input Matrix object, checks if Inverse exists. 
##      If not then calculates the Inverse of the Matrix and caches it.


## makeCacheMatrix --- Creates a List Object of Matrix and it's Inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize local variable type Matrix to NULL
    ## Define get, set, getInverse and setInverse function    
    mInv <- NULL
    set<- function(mNew) {
        x<<-mNew
        mInv<<-NULL
    }
    get<- function(){x}
    setInverse<-function(xInv){
        mInv<<-xInv
    }
    getInverse<-function(){mInv}
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

 
## cacheSolve --- Based on the input Matrix object, checks if Inverse exists. 
##      If not then calculates the Inverse of the Matrix and caches it.

cacheSolve <- function(x, ...) {
    ## Set local variable to the cached inverse matrix.
    ## Check if the local variable is NULL
    ##      If Yes, then calculate the Inverse of matrix and Return result
    ##          calculate the inverse of Matrix object.
    ##          Call setInverse to store the Inverse inthe object.
    ## Return the local variable Matrix that is the inverse of 'x'
    
    mInv<-x$getInverse()
    if(!is.null(mInv)){
        message("Getting Cached Inverse Matrix.")
        return(mInv)    
    }
    data<-x$get()
    dataInv<-solve(data)
    x$setInverse(dataInv)
    dataInv
}

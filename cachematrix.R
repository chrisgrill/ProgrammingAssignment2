## Functions to facilitate caching the inverse of a matrix

## makeCacheMatrix-accepts a matrix or creates one if one is not passed. Returns a list of functions.

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    ##Set x to a new matrix
    set<-function(y){
     x<<-y
     i<<-NULL
    }
    ##Get matrix x
    get<-function() x
    ##Set the calculated inverse
    setinverse<-function(inverse){
     i<<-inverse   
    }
    ##Get the cahed inverse
    getinverse<-function() i
    ##Return the list of functions
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## If inverse is already stored, use it. Otherwise calculate it and store it

cacheSolve <- function(x, ...) {
    ##Attempt to get inverse
    i<-x$getinverse()
    ##If the return is not null, use the cached data
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    ##Otherwise calculate inverse and store it
    matrix<-x$get()
    i<-solve(matrix)
    x$setinverse(i)
    i
}

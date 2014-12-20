## makeCacheMatrix and cacheSolve compute the inverse of input matrix and store it in cache memory
## in order to avoid repeated computations of inverse matrix.

## This function creates a special "matrix" object that can cache its inverse. In particular, makeCacheMatrix 
## creates a list, which contains four functions: set, get, setinverse, getinverse, 
## and nullifies previous inverse matrix, stored in cache memory. 
## Function "set" assigns values of matrix y to values of matrix x. 
## Function "get" returns matrix x. 
## Function "setinverse" computes the inverse matrix of x by means of solve() function.
## Function "getinverse" returns the inverse matrix of x from cache memory.


makeCacheMatrix <- function(x = matrix()) {
                inv<-NULL
                set <- function(y){
                        x<<-y
                        inv<<- NULL
          
        }
                
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of special "matrix", created by makeCacheMatrix. First, it checks if such matrix
## was already computed and input matrix did not change. In this case it returns matrix stored in cache. 
## Otherwise, cacheSolve computes the inverse matrix and stores it in cache memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <-x$getinverse()
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

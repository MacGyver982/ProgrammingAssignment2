## The makeCacheMatrix and cacheSolve functions combine to create
## a program for solving the inverse of a matrix and storing that
## solution in a cache which can be used without recalculating the
## inverse every time.


## makeCacheMatrix creates a list containing a function that
## stores and retrieves a Matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        #Set the initial value of the inverse to NULL
        i <- NULL
        
        #Sets a new value for the matrix and resets the inverse to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        #Retrieves the original matrix
        get <- function() x
        
        #Sets i to the solution calculated in cacheSolve
        setSolve <- function(solve) i <<- solve
        
        #Retrives the inverse of the matrix
        getSolve <- function() i
        
        #Creates the list
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve checks the list created by makeCacheMatrix to see if the
## inverse of the matrix exist in the cache. If it does not exist,
## the inverse is calculated. If it does exist, then the cached inverse
## is retrieved.

cacheSolve <- function(x, ...) {
        ## Retrieves the inverse of the matrix from the cache
        i <- x$getSolve()
        
        ## Checks to see if the inverse has been caclulated and stored
        ## If it is not Null, then returns matrix inverse from cache
        if(!is.null(i)) {
                message("Retrieving Matrix Inverse from Cache")
                return(i)
        }
        
        ## If i is Null, then use solve to create inverse of x
        data <- x$get()
        i <- solve(data, ...)
        
        ## Stores calculated inverse in cache
        x$setSolve(i)
        
        ## Returns inverse
        i
}

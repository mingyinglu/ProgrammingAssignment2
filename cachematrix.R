## Pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ##Initialize
        i <- NULL
        
        ##Set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        
        ##Get the matrix
        get <- function() {
                x
        }
        
        ##Set the inverse of the matrix
        setinverse <- function(inverse) i <<- inverse
        
        ##Get the inverse of the matrix
        getinverse <- function() i
        
        ##Return a list
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by
##"makeCacheMatrix" above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ##Return the inverse if its already set
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ##Get the matrix 
        data <- x$get()
        
        ##Calculate the inverse
        i <- solve(data, ...)
        
        ##Set the inverse to the object
        x$setinverse(i)
        
        ##Return the matrix
        i
}

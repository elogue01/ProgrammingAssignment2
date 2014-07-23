## These functions create a matrix where inverse is cachable, calculates 
## the inverse but if previously calculated retrieves it from cache

## Function creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		 i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Function calculates the inverse of special "matrix" from 
## makeCacheMatrix but if inverse is already calculated will
## return inverse from cache

cacheSolve <- function(x, ...) {
		 i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Returns a matrix that is the inverse of 'x'
}

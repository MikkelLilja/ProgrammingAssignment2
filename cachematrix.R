## Matrix inversion can be tough on the computing speed.
## Lets create a pair of functions that only computes the inverse, when it
## hasn't already been computed and cached.

## This first function creates a 'matrix' that is not really a matrix but a
## list containing four functions to get and set the appropriate values

makeCacheMatrix <- function(M = matrix()) {
        i <- NULL
        set <- function(A) {
                M <<- A
                i <<- NULL
        }
        get <- function() M
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This second function computes the matrix inverse of the 'matrix' returned
## from the first function, unless it has already been computed and cached.

cacheSolve <- function(M, ...) {
        i <- M$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- M$get()
        i <- solve(data, ...)
        M$setInverse(i)
        i
}

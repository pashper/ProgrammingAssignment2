## Providing two functions.  The first will take a matrix as an arguement and 
## provide the ability to set and return the matrix and the cached inverse of
## the matrix.  The second function will take in the matrix from the first and
## check the cache and return it if available. If the cache is not available,
## it will calculate the inverse of the provided matrix, utilze the function
## provided in the first to set the cache, and return the inverse.

## This function will take in a matrix and return a special matrix providing
## functions that can be called against that matrix

makeCacheMatrix <- function(x = matrix()) {
        # Clear Cache
        m <- NULL
        # Set the matrix to a new matrix and clear Cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # Provide ability to return the current matrix
        get <- function() x
        
        # Provide ability to set the Inverse in Cache
        setmatrix <- function(matrix) m <<- matrix
        
        # Provide ability to get the Inverse from Cache
        getmatrix <- function() m
        
        # Create and return list
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function will chech the cache and if available, return the inverse from
## cache.  If the cache is not available, it will set the inverse in cache and
## return the inverse.

cacheSolve <- function(x, ...) {
        # Check the Cache and if available return the Cache
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # If the cache is empty create the Inverse and place in Cache
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

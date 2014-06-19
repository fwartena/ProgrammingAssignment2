## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. This assignment is to write a pair of functions that
## cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse. The created object has the following members
## set: store the matrix in the special object
## get: retrieve the matrix from the special object
## setInverse: store the inverse of the matrix in the special object
## getInverse: retrieve the inverse of the matrix from the special object

makeCacheMatrix <- function(x = matrix()) {
    ## use the variable i to cache the inverse of the matrix
    ## initialize it to NULL as the inverse has not been calculated yet
		i <- NULL
    
    ## store the matrix using the set function
		set <- function(y) {
        x <<- y
        ## when the matrix is updated the inverse is reset to NULL
        ## as it needs to be recalculated when it is used next time
        i <<- NULL
    }
		
    ## retrieve the matrix using the get function
    get <- function() x
		
    ## set the inverse of the matrix using the setInverse function
    setInverse <- function(inverse) i <<- inverse
    
    ## get the inverse of the matrix using the getInverse function
		getInverse <- function() i
        
		list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## created by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## retrieve the inverse of 'x' and store it in i
		i <- x$getInverse()
    
    ## check if the returned value is not NULL
		if(!is.null(i)) {
        message("getting cached data")
        ## Return a matrix that is the inverse of 'x'
				return(i)
    }
		
    ## if the returned value is NULL the inverse needs to be calculated
		## retrieve the matrix and store it in data
    data <- x$get()
		
    ## calculate the inverse of the matrix and store it in i
		## this assumes the matrix is invertible as described in the assignment
    i <- solve(data, ...)
    
    ## store the inverted matrix in the cache using the setInverse function
		x$setInverse(i)
    
    ## Return a matrix that is the inverse of 'x'
		i
}

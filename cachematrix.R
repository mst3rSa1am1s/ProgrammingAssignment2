## Comment from assignment:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not
## discuss here). Your assignment is to write a pair of functions that cache
## the inverse of a matrix.

## makeCacheMatrix: Creates a matrix capable of caching the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inverseSolution <- NULL
        setDefinition <- function(y) {
                x <<- y
                inverseSolution <<- NULL
        }
        getDefinition <- function() x
        setInverse <- function(inverse) inverseSolution <<- inverse
        getInverse <- function() inverseSolution
        list(setDefinition = setDefinition,
             getDefinition = getDefinition,
             setInverse = setInverse,
             getInverse = getInverse)
} 

## Comment from assignment:
## cacheSolve: This function computes the inverse of the special "matrix"  
## returned by makeCacheMatrix above. If the inverse has already been  
## calculated (and the matrix has not changed), then the cachesolve should  
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseSolution <- x$getInverse()
        if (!is.null(inverseSolution)) {
                message("One moment R-Guru, while I fetch your cached data")
                return(inverseSolution)
        }
        matrixToSolve <- x$getDefinition()
        inverseSolution <- solve(matrixToSolve, ...)
        x$setInverse(inverseSolution)
        inverseSolution
}

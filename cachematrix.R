## This is my solution for the "Inverse Matrix Cache" assignment
## 
## ## What this class does:
##   - It contains two functions
##      * One for creating a special "matrix object" where one can cache the 
##        inverse of the matrix (makeCacheMatrix)
##      * And one for calculating, setting and retreiving the cached inverse
##        (cacheSolve)
##        
## Notes: 
##   - Personally I think 8 space indents are way overkill. I prefer 2. 
##     So I compromised and made it 4. :)
##   - The overall design of the functions in this assignment is slightly
##     flawed, from a programmatic point of view. If the user does not use
##     the cacheSolve function, but instead operates directly on the
##     makeCacheMatrix, it is possible to set an invalid inverse using the
##     'set' method. This functionality should be hidden from the user
##     by internalising the cacheSolve function within makeCacheMatrix.


## makeCacheMatrix contains a function that will take a square (and invertible)
## matrix 'x'
##
## It returns a list of actions, described below:
##  - getOriginalMatrix: will return the original matrix 'x'
##  - setOriginalMatrix: will replace the original matrix 'x' with a new matrix
##    'newX'. It will also clear any cached inverse.
##  - setCachedInverse: will add the inverse of 'x' to the cache
##  - getCachedInverse: will return the cached inverse of 'x' if it has been
##    set, otherwise it will return NULL
makeCacheMatrix <- function(x = matrix()) {
    thisInverse <- NULL
    
    getOriginalMatrix <- function() x
    setOriginalMatrix <- function(newX) {
        thisInverse <<- NULL
        x <<- newX
    }
    
    getCachedInverse <- function() thisInverse
    setCachedInverse <- function(inv) thisInverse <<- inv
    
    list(getOriginalMatrix = getOriginalMatrix,
     setCachedInverse = setCachedInverse,
     setCachedInverse = setCachedInverse,
     getCachedInverse = getCachedInverse)
}


## cacheSolve contains a function that takes an instance of the list created
## by 'makeCacheMatrix'
##
## It will return the inverse of the matrix in 'makeCacheMatrix'.
##  - If an inverse of the given matrix has been stored, the cached inverse will
##    be returned.
##  - If no inverse has been cached, the inverse will first be calculated and
##    stored in this instance of 'makeCacheMatrix', for faster retreival at a
##    later point
##  - If the matrix is not square or invertible, running cacheSolve will result
##    in an error
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getCachedInverse()
    if (!is.null(inverse)) {
        message("Inverse already cached, will not re-calculate")
    } else {
        data <- x$getOriginalMatrix()
        inverse <- solve(data, ...)
        x$setCachedInverse(inverse)
    }
    
    inverse
}

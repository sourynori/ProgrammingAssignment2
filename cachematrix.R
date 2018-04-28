## Code to save matrix and its inverse in the cache and call it whenever required.
## 

## Creates Matrix, sets the matrix, gets the matrix, sets inverse of a mtrix and gets inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    mI <- NULL
    set <- function(y) {
        if(nrow(y)!=ncol(y))
        {return(message("Please provide a square matrix."))}
        
        x <<- y
        mI <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse_matrix) mI <<- inverse_matrix
    getInverse <- function() mI
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Gets inverse of a matrix set in the cache. If not null, returns the inverse.
## If null, gets the set matrix from the cache, solves for inverse and returns the niverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    mI <- x$getInverse()
    if(!is.null(mI)) {
        message("getting cached data")
        return(mI)
    }
    data <- x$get()
    mI <- solve(data, ...)
    x$setInverse(mI)
    mI
}

## As matrix inversion can be computationaly intensive operation, 
## these functions create a cacheable "matrix" that can compute and
## store its inverse matrix.

## Function makeCacheMatrix creates a special "matrix", which is really a list
## of functions.
## Example:
##      m <- makeCacheMatrix(matrix(1:4,2,2))
##      m$get()

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(m) {
        x <<- m
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solution) inverse <<- solution 
    getinverse <- function() inverse
    ## set and get the value of its inverse matrix
 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function CacheSolve returns inverse matrix, cached if previously computed.
## Matrix provided as argument must be invertible.
## Example:
##      m <- makeCacheMatrix(matrix(1:4,2,2))
##      cacheSolve(m)

cacheSolve <- function(x) {
    solution <- x$getinverse()
    if(!is.null(solution)) {
        message("getting cached inverse matrix")
        return(solution)
    }
    ## if the cache is empty, the inverse matrix will be computed and stored.
    data <- x$get()
    solution <- solve(data)
    x$setinverse(solution)
    solution
}
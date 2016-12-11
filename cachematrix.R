## The following functions provide the ability to calculate inversed matrix while 
## using scope caching for the regular and inversed matrix 

# special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setinverse <- function(solve) m <<- solve
    getinverse <- function()m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#computes the inverse of the special "matrix" 
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

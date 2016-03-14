## This function helps to store a matrix and creates its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function actually creates the inverse of the matrix the function above has stored in cache
cachesolve <-function(x, ...) {
	## Invert the given matrix of x
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("loading the data from cache")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}
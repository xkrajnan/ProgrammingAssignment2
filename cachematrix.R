## Implementation of a matrix inversion with caching. The first function
## creates the cache structure, the second uses this structure to save
## the inverted matrix for later retrieval.

## The function creates a 4-element list containing getters and setters
## for the matrix value and the cached inversion.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(y) inv <<- y
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function first checks, whether there is a cached value. If not NULL,
## then the value is returned. Otherwise, the matrix inversion is computed
## via solve() function, the value is cached and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

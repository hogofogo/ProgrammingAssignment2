## matrix inversion is resource hungry and it might make sense to
## cache the inverse of a matrix rather than recalculate it.
## the below functions cache the inverse of a matrix


## makeCacheMatrix returns a list containing four functions: set, get, set the inverse, and get the inverse

makeCacheMatrix <- function(x = matrix()) {

		inv <- NULL
		set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}



## cacheSolve returns an inverse of the matrix to makeCacheMatrix\
## step1: check if the inverse has already been computed and stored.
## if yes, get the cached result
## if no, compute the inverse and set the value in the cache

cacheSolve <- function(x, ...) {
## x is output of makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
       inv <- x$getinv()
       if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv
}


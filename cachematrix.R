# MakeCacheMatrix caches the inverse of a matrix because
# matrix inversion is a costly computation and there may be benefit
# to caching the inverse of a matrix rather than computing it repeatedly.

# MakeCacheMatrix creates a special "matrix" object that can cache its inverse
# set the an initial value of a matrix
 
makeCacheMatrix <- function(x = matrix()) {

# set an initial value for the inverse of the matrix
	inv <- NULL
	
#  sets a value of the matrix
				
	set <- function(y) {
		inv <<- y
		inv <<- NULL
	}
# get the value of the matrix
				
	get <-function() x
	
# set the value of the inverse of the matrix
				
	setinverse <- function(inverse) inv <<- inverse
	
# get the value of the inverse of the matrix
	 			
	 	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The cacheSolve function computes the inverse of a special "matrix" object
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cache solve should retrieve the 
# inverse from the cache. 
# This exercie function assumes that the matrix is always invertible.


cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
# It first checks if the inverse has already been computed.
        message("getting cached data.")
        return(inv)
# If it has, it gets the result and skips the computation.
    }

# If not, it computes the inverse, sets the value in the cache via
# setinverse function.
	
		data <- x$get()
   		inv <- solve(data)
    	x$setinverse(inv)
   		inv
}

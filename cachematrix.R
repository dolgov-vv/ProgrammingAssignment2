## Put comments here that give an overall description of what your
## functions do

##Define and return special list with access functions
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	list(
		set = function(value) {
			x <<- value
			inverse <<- NULL
		},

		get = function() x,

		setinverse = function(value) {
			inverse <<- value
		},

		getinverse = function() inverse
	)
}


## Return cached value of inverse matrix if it's already exists or compute and cache it
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if( is.null(inv) == FALSE ) {
		message("There is cached inverse matrix")
		return(inv)
	}

	inv <- solve(x$get(), ...)
	x$setinverse(inv)
	inv
}

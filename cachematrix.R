## R Programming Assignment 2
## stephen rojak
## 21 Dec 14

## store a matrix with extended properties:
##  a cacheable inverse
##  a flag that indicates to called functions whether or not
##    to message their actions while performing them

makeCacheMatrix <- function(x = matrix()) {
	## inverse stored here
	inv <- NULL
	## message control
	is.noisy <- FALSE
	set <- function(m) {
		x <<- m
		inv <<- NULL
	}
	get <- function() x
	## manage cached inverse
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	## control messsage flag
	setNoisy <- function(flag) is.noisy <<- flag
	getNoisy <- function() is.noisy
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse,
		setNoisy = setNoisy,
		getNoisy = getNoisy)
}


## Operating on a matrix stored by makeCacheMatrix,
##  get the cached inverse if it exists
##  or create it if it does not
## x must have been created by makeCacheMatrix
## other arguments are only used if cached inverse does not exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		im <- x$getInverse()
		if (!is.null(im)) {
			if (x$getNoisy())
				message("getting cached inverse");
			return (im)
		}
		if (x$getNoisy())
			message("computing inverse");
		mat <- x$get()
		im <- solve(mat, ...)
		x$setInverse(im)
		im
}

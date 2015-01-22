## These functions calculate, cache and re-use the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse_mat <- NULL
	setMatrix <- function(y) {
		x <<- y			#load the matrix in x
		inverse_mat <<- NULL		#create the variable for the inverse of x at next steps
	}
	getMatrix <- function() x		#view the initial matrix
	setInverse <- function(initial_mat){
		inverse_mat <<- solve(initial_mat)	#load the inverse matrix in global environment
	}
	getInverse <- function() inverse_mat	#view the inverse matrix
	list(setMatrix = setMatrix,
		getMatrix = getMatrix,
		setInverse = setInverse,
		getInverse = getInverse)	#create the list of function
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	inverse_mat <- x$getInverse()
	
	if(!is.null(inverse_mat)){
		message("getting cached data")
		return(inverse_mat)
	}

	data <- x$getMatrix()
	inverse_mat <- solve(data, ...)
	x$setInverse(inverse_mat)
	inverse_mat
}

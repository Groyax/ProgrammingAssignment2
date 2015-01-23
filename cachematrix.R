#### These functions calculate, cache and re-use the inverse of a matrix ####

## First, this function creates a special of functions that can cache the inverse of your matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse_mat <- NULL
	setMatrix <- function(y) {
		x <<- y				
		inverse_mat <<- NULL		
	}
	getMatrix <- function() x		
	setInverse <- function(inverse) inverse_mat <<- inverse 
	getInverse <- function() inverse_mat	
	list(setMatrix = setMatrix,
		getMatrix = getMatrix,
		setInverse = setInverse,
		getInverse = getInverse)	
}

## Secondly, this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not been changed), then the cachesolve
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	inverse_mat <- x$getInverse()		#stock the inverse matrix saved
	if(!is.null(inverse_mat) & identical(initial_mat,x$getMatrix())){
		#verification that a matrix inverse has already been calculated
		#and that the initial matrix has not changed (global envt == makeCacheMatrix envt) 
		message("getting cached data")
		return(inverse_mat)
	}
	initial_mat <<- x$getMatrix()		#stock the initial matrix in global environment
	data <- x$getMatrix()
	inverse_mat <- solve(data, ...)
	x$setInverse(inverse_mat)
	inverse_mat
}

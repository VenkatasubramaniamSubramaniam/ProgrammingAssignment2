## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## Find below a pair of functions that cache the inverse of a matrix.

## Description: Function "makeCacheMatrix", creates a special "matrix" object 
## that can cache its inverse.
##
## Arguments:
##   x: the matrix object
##
## Returns:
##   A list containing functions to:
##     1. set - set the value of the matrix
##     2. get - get the value of the matrix
##     3. setInv - set the value of the inverse
##     4. getInv - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

	## When makeCacheMatrix is instantiated, the inverse of its content
	## matrix has not calculated yet, hence setting it to NULL
	
	InvMat <- NULL

	## When every matrix of the makeChacheMatrix instance is changed,
	## We store the new Matrix, and set the Matrix Inverse variable to NULL	
	set <- function(y) {
			x <<- y
			InvMat <<- NULL
	}

	## Function retrieves the Matrix from the makeCacheMatrix instance.
	get <- function() x

	## Function to allows the calculated Matrix Inverse to be stored.
	setInv <- function(solve) InvMat <<- solve

	## Function returns, the existing matrix inverse (matrix)
	getInv <- function() InvMat

	## Return a list object upon instantiation, 
	## with functions to read and manipulate makeCacheMatrix instance.
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Description: Function "cacheSolve", computes the inverse of the special
## "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
## This function sets the Matrix Inverse in the calling object 
## by calling the object's "setInv" function
##
## Arguments:
##   x: the special "matrix" returned from makeCacheMatrix
##
## Returns:
##   The inverse matrix

cacheSolve <- function(x, ...) {

	## Get the matrix inverse from the makeCacheMatrix instance argument.
	InvMat <- x$getInv()

	## If the value is NOT NULL, which means matrix inverse has been 
	## computed, returning the same, saving calculation time.
	if(!is.null(InvMat)) {
		message("getting cached data")
		return (InvMat)
	} 

	## Control is here means Matrix Inverse has not been calculated
	## get the matrix from the argument for calculating inverse.
	data <- x$get()
	
	## Calculating the inverse of matrix
	InvMat <- solve(data, ...)

	## Setting the calculated Matrix inverse, 
	## in the makeCacheMatrix instance argument.
	x$setInv(InvMat)

	## Return the calculated matrix inverse.
	InvMat
}

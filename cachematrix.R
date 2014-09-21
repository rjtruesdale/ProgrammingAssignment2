## The combined function call cacheSolve(makeCacheMatrix) returns the 
## matrix inverse and options to set a new matrix, get the stored matrix,
## set the inverse of the matrix or get the stored inverse of the matrix.

## These functions followed the sample code for makeVector and cachemean
## provided in the instructions for Programming Assignment 2. 

##---------------------------------------------------------------------
## Function makeCacheMatrix takes the definition of a square invertible 
## matrix, sets up the matrix and creates a list of functions that allow
## calculation of the matrix inverse and retrieval of the inverse
## from outside the function.
## 

## Example:  makeCacheMatrix(x=matrix(nrow=2,ncol=2,byrow=TRUE,c(1:4)))

## 	|	1	2    |
## 	|	3	4    |
## is a square invertible matrix.
##---------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
## create a list of functions to cache the inverse of a matrix.

	inverse <- NULL         ## set internal inverse variable NULL
	set <- function(xy) {   ## define a set function
		x <<- y           ## store passed matrix outside the function
		inverse <<- NULL  ## set retrievable inverse to NULL
	}

	get <- function() x     ## define get to retrieve the stored matrix

	setinverse <- function(solve) inverse <<- solve(x)
                              ## define setinverse to calculate the inverse

	getinverse <- function() inverse
                              ## define getinverse to retrieve the inverse

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
                              ## create list of matrix / inverse functions

}

##---------------------------------------------------------------------
## Function cacheSolve allows you to call a function that checks 
## whether the inverse of a matrix has already been calculated and 
## retrievable before performing the calculation again.

## Function makeCacheMatrix can be called as an argument to cacheSolve
##---------------------------------------------------------------------

cacheSolve <- function(x, ...) {
	  ## calculate or retrieve a cached inverse
        ## Return a matrix that is the inverse of 'x'

	inverse <- x$getinverse()   ## retrieve a cached inverse

	if(!is.null(inverse)) {     ## check if no stored inverse
		message("getting cached data")
		return(inverse)       ## return the stored inverse
	}

	data <- x$get()             ## if no cached inverse, get the matrix
	inverse <- solve(data, ...) ## recalculate inverse of the new matrix
	x$setinverse(inverse)       ## store the new inverse
	inverse                     ## return the new inverse
}

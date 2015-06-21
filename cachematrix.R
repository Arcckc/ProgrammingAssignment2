## Similar to the example code, new vector type is defined,
## which can associate a matrix with associated funtions

## makeCacheMatrix will create a matrix "vector" instance 

makeCacheMatrix <- function(x = matrix()) {
        # Save the inverse in internal cache
        i_inverse <- NULL
        
        # Access function for matrix
	set <- function(y) {
		x <<- y
		i_inverse <<- NULL
	}
        get <- function() x
        
        # Access function for inverse of the matrix
        setinv <- function(inv) i_inverse <<- inv
	getinv <- function() i_inverse
	
	# Vector defining associated function
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve, expects an object of created with above function
## This function returns cached inverse if its already present
## Otherwise, it computes the inverse of the matrix using "solve"
## function and returns the same. Inverse matrix also gets saved 
## in cache for future usage.

cacheSolve <- function(x, ...) {
	inverse <- x$getinv()
	if(!is.null(inverse)) {
		message("Getting cached data")
		return (inverse)
	}
	data <- x$get()
	inverse <- solve(data,...)
	x$setinv(inverse)
	
        ## Return a matrix that is the inverse of 'x'
        inverse
}

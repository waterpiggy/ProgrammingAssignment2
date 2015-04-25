## This function takes a matrix and create a list of function that can help
## to cache the computation result (inverse) of this matrix.

makeCacheMatrix <- function(dataset = matrix()) {
	inverse <- NULL
	set <- function(newDataset){
		dataset <<- newDataset
		inverse <<- NULL
	}
	get <- function() dataset
	setInverse <- function(newInverse) inverse <<- newInverse
	getInverse <- function() inverse
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## This function is used to get the inverse of a matrix that processed
## already by the first function. It returns the result immediately if a
## cache is found, or calculates the inverse if not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
        	message("getting cashed data")
        	return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}

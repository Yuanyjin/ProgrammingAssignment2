## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function set the value of the matrix, get the value of the matrix, set the inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## casheSolve function calculates the inverse of a matrix returned by makeCacheMatrix above. If the inverse of the matrix has aleady been calculated, then the calculated value is retrieved from the cache. If the inverse of the matrix has not been calculated, it will calculate a new value.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
        	message("getting cached data")
        	return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

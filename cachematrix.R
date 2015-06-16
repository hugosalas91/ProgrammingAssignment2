## The behaviour of this function is similar to a class.
## The function creates a list which contains 4 functions 
## set, get, setInv and getInv. It uses <<- assignment 
## operator so that these internal variables are not exposed 
## to the outside environment.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	set <- function(y) {
		x <<- y
		i <<- NULL	
	}
	
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the matrix 
## created with the above function. However, it first checks 
## to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips 
## the computation. Otherwise, it calculates the inverse matrix 
## of the data and sets the value of the inverse matrix in the 
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        
        if(!is.null(i)) {
        	message("getting cahed data")
        	return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

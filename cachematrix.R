## Put comments here that give an overall description of what your
## functions do

## Cache a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL #assigns NULL to a variable within the current environment
	y <- NULL 
	setmatrix <- function(y){
		x <<- y #cache the matrix
		m <<- NULL #search for an existing definition of the variable and set to NULL
	}
	getmatrix <- function() x #get the matrix cached with setmatrix
	setinverse <- function(solve) m <<- solve #cache value of the inverse matrix
	getinverse <- function() m #get the inverse matrix cached with setinverse
	list(setmatrix = setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse) #creates list to house the four functions)
}


## Return the inverse of the cached matrix

cacheSolve <- function(x = m(), ...) {
	m <- x$getinverse() # if an inverse has already been calculated this gets it
	if(!is.null(m)){ # check to see if cacheSolve has been run before
		if(x$setmatrix() == x$getmatrix()) { # check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix
    			message("getting cached data")
    			matrix<-x$get()
    			m<-solve(matrix, ...)
    			x$setmatrix(m)
    			return(m) 
    		}	
	}
    	# otherwise 
    	y <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
    	x$setmatrix(y) # run the setmatrix function on the input matrix to cache it
    	m <- solve(y, ...) # compute the value of the inverse of the input matrix
    	x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
    	m # return the inverse
}

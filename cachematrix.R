## Inverting matrices can be timeconsuming.  To increase efficiency, a set of functions are written
## to calculate a matrix inverse and store it for retrieval such that it only need to be inverted one time.
## NB: Both functions assume that the matrix is invertible. No error messages have been written to warn otherwise.

##makeCacheMatrix: This function creates a special "matrix" object, calculates the inverse and stores it in cache 
##for efficient retrieval. 


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL				#this is where the data will go
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve #function to invert the matrix NB:Assumes square matrix is given
        getinverse <- function() s			#function to get the inverted cached matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        s <- x$getinverse()		#first, check the cache
        if(!is.null(s)) {				
                message("getting cached data")
                return(s)		#returns the cached value
        }
        data <- x$get()			#if there is nothing cached, gets the matrix to invert
        s <- solve(data, ...)		#inverts the matrix
        x$setinverse(s)			#save the result in cache
        s					#return the result
}

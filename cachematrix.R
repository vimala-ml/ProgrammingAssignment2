## Put comments here that give an overall description of what your
## functions do

## Function to crate special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

}


## Fuction to get the cached inverse from a  special matrix which caches its inverse.
## If the inverse is not cached yet in the matrix object then the inverse is first computed and cached.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i

}

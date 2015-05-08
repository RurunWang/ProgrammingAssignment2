## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly. This pair of functions will cache the inverse of a matrix.

## The first function, makeCacheMatrix, creates a special vector, which is a list containing a function to 
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value og the inverse of a matrix
#  4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) v <<- inverse
        getinverse <- function() v
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve, calculates the inverse of the special "vector" created with the makeCacheMatrix function. It first checks to see if the inverse of the matrix has already been calculated. If so, it gets the value from the cache and returns the value. Otherwise, it solves the inverse of the matrix and sets the value in the cache via the setinverse function.  

cacheSolve <- function(x, ...) {
        v <- x$getinverse()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data)
        x$setinverse(v)
        v
}

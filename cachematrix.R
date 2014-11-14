## 	Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse

## makeCacheMatrixr creates a list containing functions to
## 	1.set the value of the matrix
##	2.get the value of the matrix
##	3.set the value of the inverted matrix
##	4.get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

## Calculates the inverse of the special matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse matrix from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the value of the inverse in the cache via 
## the setinverse function


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
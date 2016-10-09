## makeCacheMatrix: This function creates a special "matrix" 
##object that can cache its inverse. For a square inversible matrix 
##makeCacheMatrix is a list containing a function to:
##     1. set the matrix;
##     2. get the matrix;
##     3. set the inverse;
##     4. get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) iv <<- solve
        getinverse <- function() iv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##
cacheSolve <- function(x, ...) {
        iv <- x$getinverse()
        if(!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        data <- x$get()
        iv <- solve(data, ...)
        x$setinverse(iv)
        iv
}

##Example:

m<- diag(4,4)
m

##[,1] [,2] [,3] [,4]
##[1,]    4    0    0    0
##[2,]    0    4    0    0
##[3,]    0    0    4    0
##[4,]    0    0    0    4

Cm<- makeCacheMatrix(m)
cacheSolve(Cm)

##     [,1] [,2] [,3] [,4]
##[1,] 0.25 0.00 0.00 0.00
##[2,] 0.00 0.25 0.00 0.00
##[3,] 0.00 0.00 0.25 0.00
##[4,] 0.00 0.00 0.00 0.25


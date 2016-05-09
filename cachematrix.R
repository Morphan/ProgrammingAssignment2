## Rationale: Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute it
## repeatedly.



## This function creates a special "matrix" object that can cache its inverse. 
## The matrix given to the function has to be invertible (here it is assumed
## that the matrix is always invertible).

makeCacheMatrix <- function(x = matrix()) {
        ## initialize empty matrix as cache
        m <- NULL
        ## create function "set": replace old value x with y and empty cache m
        ## (the <<- operator can be used to assign a value to an object in an
        ## environment that is different from the current environment)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## create function "get": store the original matrix x
        get <- function() x
        ## create function "setInverse": calculate the inverse of the matrix
        ## (computing the inverse of a square matrix can be done with the solve
        ## function in R. For example, if X is a square invertible matrix, then
        ## solve(X) returns its inverse)
        setInverse <- function(solve) m <<- solve
        ## return the calculated inverse matrix
        getInverse <- function() m
        ## make list for funtion calls outside of function (?)
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## retrieve inverse matrix directly from above cache function (whether
        ## it has been calculated earlier or not)
        m <- x$getInverse()
        ## if the retrieved value is not empty: return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if the retrieved value has been empty: calculate the inverse matrix
        ## now
        data <- x$get()
        m <- solve(data, ...)
        ## store resulting inverse matrix in cache with above function
        x$setInverse(m)
        ## return the inverse matrix
        m
}




# Example: Inverse of 3x3 matrix 
# x <- matrix(c(1,5,2,7,2,8,8,3,9), nrow=3, ncol=3, byrow=T)
# test <- makeCacheMatrix(x)
# cacheSolve(test)
# Expected Answer:
#       [,1]       [,2]       [,3]
# [1,] -0.6666667 -4.3333333  4.0000000
# [2,]  0.1111111 -0.7777778  0.6666667
# [3,]  0.5555556  4.1111111 -3.6666667
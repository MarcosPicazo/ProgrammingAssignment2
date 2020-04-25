## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix create an R object using a given variable, and assign a set of functions inside the
## object that allows to set a new Matrix, get the Matrix, set the inverse and get the inverse of
## the original matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolveMatrix first check if the inverse of the matrix exists inside the object, if not takes
## the original matrix from the object and calculates the inverse, storing it inside the object so 
## next time there will be no need to recalculate

cacheSolveMatrix <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


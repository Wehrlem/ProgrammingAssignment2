## Programming Assignment 2: Lexical Scoping

# The makeCacheMatrix functions returns a list containing following functions
# set: set the matrix
# get: get the matrix
# settheInverse: set inverse of the matrix
# gettheInverse: get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    settheInverse <- function(inverse) inv <<- inverse
    gettheInverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The function cacheSolve return an inverse matrix of an input matrix. 
# It checks at first step if there is already a cached matrix, otherwise
# it computes the inverse then sets the values in the cache with setinverse. 
# The input matrix has to be invertible.

cacheSolve <- function(x, ...) {
    inv <- x$gettheInverse()
    if(!is.null(inv)) {
        message("get cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$settheInverse(inv)
    inv
}

# Example:
# B <- rbind(c(2, 7, 2),c(1, 8, 1),c(4, 3, 1))
# m = makeCacheMatrix(B)
# cacheSolve(m)
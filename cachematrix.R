# What the function does:

# makeCacheMatrix() is a function that returns a list
#   - it takes a matrix as a parameter.
#   - it holds data and provide functions to access the data

# cacheSolve() is a function that returns a matrix
#   - it takes a list (created by makeCacheMatrix())
#   - it updates the parameter's i if i is NULL




## makeCacheMatrix()
#
#   i is a variable to hold the inversed matrix.
#   set() defines a function that caches x (the matrix) and resets i (the inversed matrix)
#   get() defines a function that returns x (the matrix).
#   setInverse() defines a function that caches i.
#   getInverse() defines a function that returns i.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(y) i <<- y
    getInverse <- function() i
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve()
#
#   initiate i by getting the inversed matrix from getInverse()
#   if (i is not null)
#       return i
#   else
#       initiate a by getting the matrix from get()
#       calculate the inversed matrix and assign it to i
#       update the list
#       return i
#   end

cacheSolve <- function(f, ...) {
    i <- f$getInverse()
    if (!is.null(i)) { # cached data exists
        return(i)
    }
    a <- f$get()
    i <- solve(a, ...)
    f$setInverser(i)
    i
}
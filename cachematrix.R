## R Programming Assignment 2: Lexical Scoping
## Assume that the matrices supplied are always invertible.

## The makeCacheMatrix function creates a special "data matrix",
## which is really a list containing a function to
## 1. set the value of the data matrix
## 2. get the value of the data matrix
## 3. set the value of the inverse of the data matrix
## 4. get the value of the inverse of the data matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The cacheSolve function calculates the inverse of the special "data matrix"
## which was created with the makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated and if it
## has, cacheSolve gets the inverse from the cache and skips the calculation.
## If the inverse has not already been calculated, it calculates it sets the value of the inverse
## in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
       
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        dataMatrix <- x$get()
        inv <- solve(dataMatrix, ...)
        x$setInverse(inv)
        inv
}

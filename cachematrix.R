## Cached version of matrix which efficiently calculates its inverse
## when result is required multiple times.

## Create cached version of matrix.
## This is represented by a list with following 4 functions:
##  - set : Set the underlying matrix.
##  - get : Get the underlying matrix.
##  - setInverse : Set an inverse of the underlying matrix.
##  - getInverse : Get an inverse of the underlying matrix.
##
## Args:
##  x: A underlying matrix represented by the result.
##
## Returns:
##  A list which can be regarded as a cached version of matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(mat) {
        x <<- mat？
        # We have to erase answer since it is no longer right.
        inverse <<- NULL
    }
    get <- function() {
        x
    }

    # This function is intended to be used only by `cacheSolve`.
    # Calling this directly may result in wrong answer.
    setInverse <- function(inv) {
        inverse <<- inv
    }
    getInverse <- function() {
        inverse
    }

    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculate an inverse matrix.
## This function uses answer if available from previous call
## for saving time.
##
## Args:
##  x: Cached version of matrix created by `makeCacheMatrix`.
##
## Returns:
##  An inverse matrix of x, non-cached version.  

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        inverse
    } else {
        inverse <- solve(x$get(), ...)
        x$setInverse(inverse)
        inverse
    }
}

# Below is just for testing.

test <- makeCacheMatrix(matrix(1:4, 2, 2))
print(test$get())
print(test$getInverse())
testInverse <- cacheSolve(test)
testInverse <- cacheSolve(test)

print(test$get() %*% testInverse)

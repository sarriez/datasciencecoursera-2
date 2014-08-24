## Put comments here that give an overall description of what your
## functions do

# "makeCacheMatrix" makes a list containing a function to
# 1. set new matrix
# 2. get the value of a matrix
# 3. set the value of inverse of a matrix
# 4. get the value of inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMatrix <- function() x
    setInvMatrix <- function(inverse) inv <<- inverse
    getInvMatrix <- function() inv
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


# The following function calculates the inverse of the matrix. However, It first 
# checks if the inverse has already been computed. If so, it gets the result from
# cache and skips the computation. If not, it computes the inverse and sets the 
# value in the cache via setInvMatrix function.

cacheSolve <- function(x, ...) {
    inv <- x$getInvMatrix()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$getMatrix()
    inv <- solve(data)
    x$setInvMatrix(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}

## makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) x_inv <<-inverse
        getinverse <- function() x_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.

## If the inverse has already been calculated and the matrix has not
## changed, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$getinverse()
        if (!is.null(x_inv)) {
                message("getting cached inverse matrix")
                return(x_inv)
        } else {
                x_inv <- solve(x$get())
                x$setinverse(x_inv)
                return(x_inv)
        }
}

#####################################
## Function: makeCacheMatrix
## Creates a object that is a special type of matrix, which can
## store a cached version of its inverse matrix.
## Input: a matrix, supposed to be always invertible.
## Output: the matrix converted to the special matrix.
#####################################
makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverse_matrix <<- inverse
        getInverse <- function() inverse_matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


#####################################
## Function: cacheSolve
## Returns the inverse matrix of a special matrix created with
## 		makeCacheMatrix
#####################################
cacheSolve <- function(x, ...) {
        i <- x$getInverse() ## Retrieve the inverse stored in the object
        if(!is.null(i)) {   ## If there was a cached inverse
                message("Getting cached data")
                return(i)
        }
        ## If there wasn't a cached inverse
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}

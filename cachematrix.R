## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        set_inverse_matrix <- function(inverse_matrix) inv_x <<- inverse_matrix
        get_inverse_matrix <- function() inv_x
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix,
             get_inverse_matrix = get_inverse_matrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$get_inverse_matrix()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$set_inverse_matrix(inv_x)
        inv_x

}

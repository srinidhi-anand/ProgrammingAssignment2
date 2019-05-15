## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
###to set the matrix object
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        get <- function() x
        inversematrix <- function(inverses) m <<- inverses
        matrices <- function() x
        list(get = get(),
             inversematrix = inversematrix(solve(x)),
             matrices = matrices())
}


## Write a short comment describing this function
####to do matrix inverse if its not already cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         x <- makeCacheMatrix(x)
        m <- x$matrices
        if(!is.null(m)) {
                message("matrix has not changed")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$inversematrix
        m
}

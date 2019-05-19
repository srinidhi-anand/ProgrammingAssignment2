###to set the matrix object
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        get <- function() x                                 ####to get the matrix
        inversematrix <- function(inverses) m <<- inverses  ###to inverse the matrix
        matrices <- function() x
        list(get = get(),
             inversematrix = inversematrix(solve(x)),
             matrices = matrices())
}



####to do matrix inverse if its not already cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         x <- makeCacheMatrix(x)
        m <- x$matrices
        if(!is.null(m)) {
                message("matrix has not changed")      ####only if matrix is not changed
                return(m)
        }
        data <- x$get()                                ####to inverse the matrix
        m <- solve(data, ...)
        x$inversematrix
        m                                             ####to display the inverse of the matrix
}

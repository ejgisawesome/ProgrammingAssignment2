## An attempt to cache the inverse of a matrix so that
## it's not necessary to recalculate.  Can't tell if it works.

## install and load matlib package to solve matrices
install.packages("matlib")
library(matlib)
?inv #the function for inverse of a matrix, in matlib package

## I guess this sets up the matrix and gets the inverse
makeCacheMatrix <- function(x = matrix()) {
        ## hoping these work the same as example
        ## we'll find out won't we?
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set,get = get,
             setinv = setinv,
             getinv = getinv)
}


## If this works, it's going to calculate the inverse of the
## matrix the last function made, but will skip if already
## computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inv(data,...)
        x$setinv(m)
        m
}

## Let's see if the change to a matrix causes problems...

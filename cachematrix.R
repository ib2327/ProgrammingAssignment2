
## This function creates a special "matrix" object that is a list of four functions: 
## 1) set values of a matrix 2) return the matrix 
## 3) set values of inverse of the matrix 4) return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
       
       invmat <- NULL
       set <- function(y) {
              x <<- y
              invmat <<- NULL
       }
       get <- function() x
       setinv <- function(inv) invmat <<- inv
       getinv <- function() invmat
       
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## This function computes the inverse of a matrix object returned by makeCacheMatrix function. If the 
## inverse has already been calculated (and the matrix has not changed) it retrieves the inverse of 
## the matrix from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}














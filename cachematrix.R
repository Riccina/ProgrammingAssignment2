## Writing a function that can cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(v = matrix()) {
                
                ## Defining the inverse property
                Inv <- NULL
                
                ## 1. set the value of the matrix
                set <- function(matrix) {
                        v <<- matrix
                        Inv <<- NULL
                }
                
                ## 2. get the value of the matrix
                get <- function() v
                
                ## 3. set the value of inverse of the matrix
                setInverse <- function(Inverse) Inv <<- Inverse
                
                ## 4. get the value of inverse of the matrix
                getInverse<- function() Inv
                list(set = set, get = get,
                     setInverse = setInverse,
                     getmean = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        v <- x$getinverse()
        if(!is.null(v)) {
                message("getting cached data.")
                return(v)
        }
        data <- x$get()
        
        ##Calculating the inverse
        v <- solve(data)
        
        ##Setting the inverse to the object
        x$setinverse(v)
        
        ##Returning the inverted matrix
        v  
        
}

## The two functions makeCacheMatrix and cacheSolve retrieve the cached value
## of inverse of matrix if has already been computed else return the newly 
## computed value


## makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse

  makeCacheMatrix <- function(x = matrix()) {
        setMatrix <- function(y) {            
                x <<- y                         ## Set the value for the matrix
                if(exists("inv")) {             ## Initialize the inverse matrix
                        inv
                } else inv <<- NULL
        }
        
        getMatrix <- function() x               ## Get the value of the matrix
        setInverse <- function(mi) inv <<- mi   ## Set the value for the Inverse
        getInverse <- function() inv            ## Get the value of the Inverse
        
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)           ## return a list of functions
  }


## cacheSolve function computes the inverse of the matrix or retrieves 
## cached value 

  cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'y'
       
        fun <- makeCacheMatrix()     ## Retrieve the list of functions
        fun$setMatrix(y)
        x <- fun$getMatrix()
        
                
        inv <- fun$getInverse()      ## Retrieve the inverse from cache
        if((!is.null(inv))) {
                message("getting data from the cache")
                return(inv)                
        }
        
        data <- fun$getMatrix()     
        inv <- solve(data)           ## Compute the inverse of matrix
        fun$setInverse(inv)
        inv
  }

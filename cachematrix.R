## 
## The following functions define the functionality of a special matrix object that can cache its inverse to avoid
## the computational costs of repeatedly solving for the inverse of the matrix. 
## There are two relevant functions:    
##     makeCacheMatrix -- a matrix constructor, and
##     cacheSolve -- a wrapper function to return the matrix inverse, solving for this only if not previously cached.
##
##

#################################################################################################
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##                  Internally, the matrix is stored as 'x', and its inverse as 'inv'.
#################################################################################################

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set the new matrix, and clear the cached inverse.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Return the raw matrix
    get <- function() x
    
    # Set the matrix inverse.
    setinverse <- function(inverse) inv <<- inverse
    
    # Get the matrix inverse
    getinverse <- function() inv
    
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



####################################################################################################################
## cacheSolve: Return a matrix that is the inverse of 'x', a special 'matrix' object, as defined in makeCacheMatrix.
##             and returns the matrix inverse. If the inverse has already been cached, it simply returns the cached 
##             inverse. Otherwise it computes the inverse and caches the result.
####################################################################################################################

cacheSolve <- function(x) {
    
    inv <- x$getinverse()
    
    #If inverse is already cached, return cached data...
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #Else, compute the inverse and cache the result.
    mat <- x$get()
    inv <- solve(mat)  #We assume (for the purposes of the assignment) that mat is invertible.
    x$setinverse(inv)
    inv
}


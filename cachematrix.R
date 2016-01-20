# 1-    Below function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        mat_inv <- NULL 
        
        set <- function(y) { 
                
                x <<- y
                
                inv <<- NULL
        
        }
        
        
        get <- function() 
        
        set_Inverse <- function(inverse) mat_inv <<- inverse
        
        get_Inverse <- function() mat_inv
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# 2-    This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#       If the inverse has already been calculated (and the matrix has not changed),
#       then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        mat_inv <- x$getInverse()
        
        if (!is.null(mat_inv)) {
        
                message("getting cached data")
                
                return(mat_inv)
        
        }
        
        mat <- x$get()
        
        mat_inv <- solve(mat, ...)
        
        x$set_Inverse(mat_inv)
        
        mat_inv

}

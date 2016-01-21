# 1-    Below function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { # start of function
        
        mat_inv <- NULL # empty metrix Variable 
        
        set <- function(y) { 
                
                x <<- y
                
                inv <<- NULL
        
        }
        
        
        get <- function()
        
        set_Inverse <- function(inverse) mat_inv <<- inverse ## set_inverse is a function
        
        get_Inverse <- function() mat_inv 
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse) 
}

# 2-    This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#       If the inverse has already been calculated (and the matrix has not changed),
#       then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { # start of cacheSolve function
        
        mat_inv <- x$getInverse() # get the inverse of x
        
        if (!is.null(mat_inv)) { # check if the mat_inv is empty
        
                message("getting cached data")  # print status
                
                return(mat_inv) # print inverse matix
        
        }
        
        mat <- x$get() 
        
        mat_inv <- solve(mat, ...)
        
        x$set_Inverse(mat_inv)
        
        mat_inv

}

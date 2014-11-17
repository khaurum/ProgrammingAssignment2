## The two presented functions together can create a cache, invert a matrix, save the inverted matrix in 
##      the cache, and if the same matrix is called to be inverted by cacheSolve, it is returned from the
##      cache to save the time it would take to run the function.

# makeCacheMatrix is passed a matrix and creates an "object" of the type "list", 
# which is stored in the cache. The created object is then accessed and returned.
# One can reassign a vaule to the created object by the $set() function.

makeCacheMatrix <- function(x = matrix()) {     # Input x is a matrix.
        s <- NULL                               # 's' is the inverted matrix defined by cacheSolve and is reset to
                                                #       NULL when makeCacheMatrix is called.                                                     
        set <- function(y) {                    # Takes an input matrix 'y'... 
                x <<- y                         #  ... and overwrites the stored object 'x'.
                s <<- NULL                      # Resets any stored inverted matrix to NULL.
        }
        get <- function() {x}                   # Accesses the value of the defined matrix 'x'. 
        setsolve <- function(solve)             # This function is called by 'cacheSolve' first time 'cacheSolve' is
                {s <<- solve}                   #       called and stores the inverted matrix in the object 's'. 
        getsolve <- function() {s}              # The 'getsolve' function is used by 'cacheSolve' to return the inverted
                                                #       matrices stored in 's' during subsequent calls of 'cacheSolve'.
        list(set = set, get = get,              # A list of internal functions that are called each time 'makeCacheMatrix'
             setsolve = setsolve,               #       is called thereby defining the internal functions.
             getsolve = getsolve)               
}


# 'cacheSolve' returns the inverse matrix of 'x'. If the inverse matrix is already cached 
# through earlier runs of the function it will retrieve the cached inverse matrix.

cacheSolve <- function(x, ...) {                # 'x' is an object created by 'makeCacheMatrix'. 
        s <- x$getsolve()                       # Accesses the object 'x' and retrieves the inverted matrix saving it in 's'.
        if(!is.null(s)) {                       # If the retrieved object 's' is not NULL ...
                message("getting cached data")  #       ... this message and ... 
                return(s)                       #       ... the inverted matrix in 's' is returned.
        }
        data <- x$get()                         # If the object 's' is NULL the matrix will be saved as 'data'.
        s <- solve(data, ...)                   # Then the inverse matrix is created by the 'solve()' function.
        x$setsolve(s)                           # The inversed matrix is saved in the oblect 's'.
        s                                       # Lastly 's' is returned.
}

## matrix assignment
## return: a list containing functions to:
## 1. Create matrix
## 2. Get Matrix Data
## 3. Set the inverse
## 4. Get the inverse
## This information is used as the inputs to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}
        
## This returns a matrix that is the inverse of 'x'
## x: is the output of makeCacheMatrix()
## Inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculate the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}




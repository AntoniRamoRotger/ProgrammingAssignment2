## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function creates a special "matrix" object 
        ## that can cache its inverse
        ## assume the matrix supplied is always invertible
        ## a square invertible matrix

inv = NULL
set = function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment. 
        x <<- y
        inv <<- NULL
}
get = function() x
setinv = function(solve) inv <<- solve 
getinv = function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## Write a short comment describing this function

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix().
        ## If the inverse has already been calculated and the matrix has not changed,
        ## it retrieves the inverse from the existing cache
makeCacheMatrix <- function(x = matrix()) {

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}


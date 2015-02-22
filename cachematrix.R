#Below are two functions that are used to create a special object that stores
#a matrix and caches its inverse. 


#The first function, makeCachemMatrix creates a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of its inverse
#get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The second function calculates the inverse of the matrix contained in the list
#created with the above function. It first checks to see if the inverse matrix 
#has already been calculated. If so, it gets the inverse matrix from the cache 
#and skips the computation. Otherwise, it calculates the inverse of the given 
#matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
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
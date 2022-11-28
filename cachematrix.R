## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function consist of set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL           #initializing inverse as NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x   #function to get matrix x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## Write a short comment describing this function

#This function is used to get cache data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        inv <- x$getinv()
        if(!is.null(inv)) {         #checking if inverse is NULL
                message("getting cached data")
                return(inv)         #returns inverse value from cache
        }
        data <- x$get()
        inv <- solve(data, ...)     #calculates inverse value
        x$setinv(inv)
        inv                       #returns matrix that is inverse of x
}



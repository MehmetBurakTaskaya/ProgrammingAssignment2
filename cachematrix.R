## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL                      #initializing inverse as null
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}                #function to get matrix x
        setInverse <- function(inverse) {
                inv <<- inverse              #function to obtain inverse of the matrix
                }
        getInverse <- function() {inv}
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This is used to get cached data
cacheSolve <- function(x, ...){  #gets cache data
        inv <- x$getInverse()
        if(!is.null(inv)){      #checking whether inverse is null
                message("getting cached data")
                return(inv)           #returns inverve value
        }
        mat <- x$get()
        inv <- solve(mat, ...)    #calculates inverse value
        x$setInverse(inv)
        inv                  #return a matrix that is the inverse of x
}


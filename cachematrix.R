# makeCacheMatrix is meant to create a function that makes a cache of the inverse of a matrix
# 1. creating the first parameters of the function
# 2. set is meant to make changes in the vector stored in the main function
# 3. get is meant to return the vector stored in the main function
# 4. setinverse sets the value as the inverse of the matrix
# 5. getinverse is meant to get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {> makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
       set <- function(y) {
             x <<- y
             i <<- NULL
       }
       get <- function() x
       setinverse <- function(solve) i <<- solve
       getinverse <- function() i
       list(set = set, get = get,
            setinverse = setinverse,
           getinverse = getinverse)
}


# cacheSolve is meant to compute the inverse of the matrix. It first sees if 
# the inverse has already computed, and if this is true it gets the result 
# and skips the rest of the function. If the inverse is not yet computed,
# it computes the inverse and sets the value via the setinverse function

cacheSolve <- function(x, ...) {       i <- x$getinverse()
       if(!is.null(i)) {
             message("getting cached data")
             return(i)
       }
       data <- x$get()
       i <- solve(data)
       x$setinverse(i)
       i
 }
               
## Test Run
               ## generate matrix
##> g <- matrix(rnorm(15),3,3)
##> g
##           [,1]      [,2]        [,3]
##[1,] -0.7372368 0.4692986  0.02518957
##[2,]  0.2082417 0.6971028 -1.68635151
##[3,] -1.3971880 0.6572188  0.67417206
               ## apply first function
##> x <- makeCacheMatrix(g)
##> x$get()
##           [,1]      [,2]        [,3]
##[1,] -0.7372368 0.4692986  0.02518957
##[2,]  0.2082417 0.6971028 -1.68635151
##[3,] -1.3971880 0.6572188  0.6741720
               ## inverse not calculated so proceed to solve
##> cacheSolve(x)
##          [,1]     [,2]      [,3]
##[1,] -16.48778 3.132279  8.451024
##[2,] -23.14748 4.824621 12.933021
##[3,] -11.60471 1.788196  6.389831     
}

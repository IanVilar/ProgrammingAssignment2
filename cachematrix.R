## Essentially, 'makeCacheMatrix' implements gets and sets functions for the matrix 'x'
## and the inverted matrix of 'x', inv'.

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## 'cacheSolve' calculates the inverse of a matrix given as input. First it gets the
## possible cached inverse matrix and checks if its value is different from 'NULL'.
## In the affirmative case, the cached inverse matrix is then immediately returned.
## Otherwise, the matrix 'x' is retrieved, its inverse matrix is calculated and cached
## using the 'setInv' function. Lastly, the inverted matrix is returned.

cacheSolve <- function(x, ...) 
{
    inv <- x$getInv()
    if(!is.null(inv))
    {
        print("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(a = data, ...)
    x$setInv(inv)
    inv
}

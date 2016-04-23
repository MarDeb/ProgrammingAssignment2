## Coursera - Data Science: R Programming - Assignment week 3
## 

## Function makeCacheMatrix description:
# Function output is a list with values that do the following: 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    invMtrx <- NULL
    set <- function(y) {
        x <<- y
        invMtrx <<- NULL #-> reset the inverse matrix value to NULL
    }
    
    get <- function() x #-> returns matrix x
    setInvrs <- function(inverse) invMtrx <<- inverse
    getInvrs <- function() invMtrx
    list(setMtrx = set, getMtrx = get, setInvrs = setInvrs, getInvrs = getInvrs)
}


## FUnction checks if inverse of a matrix is cached 
#  if not it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        invMtrx <- x$getInvrs()
        if(!is.null(invMtrx)) {
            message("getting cached data.")
            invMtrx
        }
        data <- x$getMtrx()
        invMtrx <- solve(data)
        x$setInvrs(invMtrx)
        invMtrx
}

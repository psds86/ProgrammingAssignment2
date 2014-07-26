## Pair of functions that cache the inverse of a matrix

## 1) Creates a matrix object that can cache its inverse

makeCacheMatrix <- function (m0 = numeric()) {
    invMat <- NULL
        
    setMat <- function(m1) {
        m0 <<- m1     
        invMat <<- NULL
    }
    getMat <- function() m0
        
    setInv <- function(inv) invMat <<- inv
    getInv <- function() invMat
        
    list(setMat = setMat, getMat = getMat,
         setInv = setInv,
         getInv = getInv)
}

## 2) Computes the inverse of the matrix returned by makeCacheMatrix,
## or retrieves the inverse from the cache if inverse has been alrdy calculated.

cacheSolve <- function(x, ...) {
    invMat <- x$getInv()
    
    if(!is.null(invMat)) {
        message("getting cached data") 
        return(invMat)
    }
    
    m0 <- x$getMat()
    invMat <- solve(m0)
    x$setInv(invMat)
    
    invMat
}
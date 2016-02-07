

## makeCacheMatrix stores a matrix X in memory
## cacheSolve: Function that shows the inverse of a matriz if it already calculated...if not it evaluates the inverse

## makeCacheMatrix uses scoping rules and stores matrices in memory
makeCacheMatrix <- function(x = numeric()) {
        
        
        cache <- NULL
        
       
        setMatrix <- function(newValue) {
                x <<- newValue
                cache <<- NULL
        }
        getMatrix <- function() {
                x
        }
        cacheInverse <- function(solve) {
                cache <<- solve #solve the inverse of the matrix
        }
        getInverse <- function() {
                cache
        }
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

## cacheSolve: Function that shows the inverse of a matriz if it already calculated...if not it evaluates the inverse
cacheSolve <- function(y, ...) {
      
        inverse <- y$getInverse()
  
        if(!is.null(inverse)) # evaluate if the inverse is already calculated and in memory
        { 
                message("Obtaining cached data")
                return(inverse) 
        }
      
        data <- y$getMatrix()
        inverse <- solve(data) # calculte the inverse of the matrix
        y$cacheInverse(inverse) #cache the inverse of the matrix
        inverse
}

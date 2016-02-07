## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function


makeCacheMatrix <- function(X = matrix()) {
inverse <- NULL
set <- function(Y){
	X <<- Y
	inverse <<- NULL
	}
get <- function() X
setinverse <- function(Inverse) inverse <<- Inverse
getinverse <- function() inverse
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function


cacheSolve <- function(X, ...) 
{
if(require("corpcor")){
	print("Corpcor instalado")
	} else {
		print("intentando instalar corpcor")
		install.packages("corpcor")
		if(require(corpcor)){
			print("corpcor instalado y cargado")
			} else {
			stop("no pudo intalarse corpcor")
			}
		}
inverse <- X$getinverse()
if(!is.null(inverse)){
	message("matriz está en memoria")
	return(inverse)
	}
message("la inversa no está en memoria entonces va a ser calculada")
data <- X$get()
inverse <- pseudoinverse(data, ...)
X$setinverse(inverse)
inverse
}


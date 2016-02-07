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
		print("ntentando instalar corpcor")
		install.packages("corpcor")
		if(require(corpcor)){
			print("corpcor instalado y cargado")
			} else {
			stop("mo pudo intalarse corpcor")
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


#Experiment to try if it works
#square matrix
X <- matrix(rpois(25,3), nrow = 5)
cX <- makeCacheMatrix(X)
cX$get()
cacheSolve(cX)
cacheSolve(cX)
invX <- cacheSolve(cX)

#Experiment to try if it works
#rectangular matrix rows > cols
Y <- matrix(rpois(20,2), nrow = 5, ncol = 4)
cY <- makeCacheMatrix(Y)
cY$get()
cacheSolve(cY)
cacheSolve(cY)
invY <- cacheSolve(cY)

#Experiment to try if it works
#rectangular matrix rows < cols
Z <- matrix(rpois(20,1), nrow = 4, ncol = 5)
cZ <- makeCacheMatrix(Z)
cZ$get()
cacheSolve(cZ)
cacheSolve(cZ)
invZ <- cacheSolve(cZ)

#Experiment to try if it works
#multiplication must return identity or closer
invX %*% X 
X %*% invX
invY %*% Y 
Z %*% invZ 

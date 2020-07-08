
makeCacheMatrix <- function(x = matrix()) {  ##create a matrix which is function of x
j <- NULL    ## Assign Null value to J
set <- function(y){
  x <<- y
  j <<- NULL
}
get <- function()x
setInverse <- function(inverse) j <<- inverse
getInverse <- function() j 
list(set = set, get = get, 
     setInverse = setInverse, 
     getInverse = getInverse)
}


cacheSolve <- function(x, ...) { ##function to check if matrix is solved already or not
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){                   #if loof to check if matrix is already solved or not
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

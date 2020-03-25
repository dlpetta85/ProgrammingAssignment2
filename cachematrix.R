##Creation of the matrix. This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mat = matrix()) {
  ## Setting the matrix
  alt <- NULL
  set <- function(y) {
    mat <<- y
    alt <<- NULL
  }
  ##Getting the matrix
  get <- function() mat
  setalternado <- function(inverse) alt <<- inverse
  getalternado <- function() alt
  list(set = set,
       get = get,
       setalternado = setalternado,
       getalternado = getalternado)
}

##unused space
##unused space
##unused space
##unused space

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(mat, ...) {
##returning the inverse matrix 
  alt <- mat$getalternado()
  if(!is.null(alt)) {
    return(alt)
  }
##Getting the matrix
  dados <- mat$get()
##calculating the inverse
  alt <- solve(dados)
 ##Setting the inverse
  mat$setalternado(alt)
  alt
}


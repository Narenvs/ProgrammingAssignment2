## Put comments here that give an overall description of what your
## functions do
#  makeCacheMatrix - wrapper function to set and get the inverse of the input matrix (assumed to be invertable)
#  input : 'x' invertable matrix
#  output : list with functions for
#           1. set the inverse matrix
#           2. get the matrix to be inversed
#           3. set the inverse in the cache
#           4. get the inversed matrix from the cache
#
#  cacheSolve - function to return the inverse for the input matrix

#  makeCacheMatrix - wrapper function to set and get the inverse of the input matrix (assumed to be invertable)

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  
  set <- function(y) {
    x <<- y
    matinv <<- NULL
    
  }
  
  get <- function() x
  setinv <- function(inv) matinv <<- inv
  getinv <- function() matinv
  
  list(set = set, get = get, setinv = setinv, getinv=getinv)

}


#  cacheSolve - function to return the inverse for the input matrix.
#               It will first check the existance of the inverse in the cache :
#               If exists, it will return the cached matrix
#               else it will do the matrix inverse operation and set it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  matinv = x$getinv()
  if (!is.null(matinv)) {
    return(matinv)
  }
  
  matval <- x$get()
  matinv <- solve(matval,...) # getting the inverse of the matrix
  
  x$setinv(matinv)
  return(matinv)
  
}

## Put comments here that give an overall description of what your
## functions do

## Function that creates object to store data: matrix and its inversion

makeCacheMatrix <- function(matr = matrix()) {
  inverted_matrix <- NULL #initialize function with NULL as inverted matrix
  
  set <- function(y) {
    matr <<- y #setting given matrix as a matrix to be inverted ("matr")
    inverted_matrix <<- NULL #setting inverted matrix to NULL for the newly given matrix that is set above
  }
  
  get <- function() matr  #return the matrix (supposed to be inverted) that is acttually stored in function
  set_invmatrix <- function(m) inverted_matrix <<- m  #set the value of inverted_matrix (to be cached for future use)
  get_invmatrix <- function() inverted_matrix  #return the inverted_matrix stored in function
  
  list(set = set, get = get,
       set_invmatrix = set_invmatrix,
       get_invmatrix = get_invmatrix) #list of subfunctions contained in the main function
}

## function that checks if the inverted matrix is already stored in makeCacheMatrix object and:
## 1. returns its value if its stored or
## 2. calculates inverted matrix and store it in the makeCacheMatrix object otherwise. 

cacheSolve <- function(x, ...) {
  m <- x$get_invmatrix() #getting the value of the inverted matrix stored in makeCacheMatrix object
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } # if inverted matrix is already cached then returning its cached value
  
  ##calculation of inverted matrix when it's not stored in makeCahceMatrix object
  
  data <- x$get() #getting matrix to be inverted fom makeCacheMatrix object
  m <- solve(data, ...) #inverting the matrix got from makeCachedMatrix object
  x$set_invmatrix(m) #setting calculated inversion of the matrix in makeCachMatrix object (to be stored as inverted_matrix)
  m #returning the calculated inversion of the matrix
}

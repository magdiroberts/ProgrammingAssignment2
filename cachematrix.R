
## makeCacheMatrix declares a list of functions : set, get, setinverse and getinverse
## these functions will be used in cacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
  
  ## this variable will hold the inverse of the matrix - initially set to NULL
  m <- NULL
  
  ##
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(x) m <<- solve(x)

  getinverse <- function() m
  
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve function shows the inverse of a matrix from cache
## if it was poreviously calculated or calculates and stores
## the inverse
## I used message throughout my function as i was writing it 
## was following the course of execution - now they are commented out

cacheSolve <- function(x) {
  ##message("1")   
  m<- x$getinverse()  ## on first call should return NULL as it was initialized with NULL in makeCacheMatrix 
       
  ##checking if there is a value in the inverse variable
  ##and if there is return than value
  if (!is.null(m)){ 
    
      return(m)
  }
  
  ##message("2")   
  ##return into d the matrix
  d<- x$get() 
    
  ##message("3")   
  ##print(d)
  ##calculate the inverse of the matrix and store it in m
  m <- solve(d) 
  ##message("4")   
  
  x$setinverse(m) 
  ##message("5")   
  ##show m
  m 
  ##message("6")   

}

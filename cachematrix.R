## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## MakeCacheMatrix Caches a value that is sent to it, in this case its Inverse
makeCacheMatrix <- function(x = matrix()) {

  Inv <- NULL
  
  ##Define the "set function" (Nested function)
  set <-function(y){
    
    x <<- y
    Inv <<- NULL
    
  }
  
  
  ##Define the "get function" (Nested function)
  get <- function() x
  
  ##Define the SetInv function. This is where the Inverse gets cached
  setInv <- function() Inv <<- x
  
  ##Define the getInv function that merely returns a previously calulated and cached Inverse
  getInv <- function() Inv

  
  ##Return the list of all nested functions
  list(set=set, get=get, 
       setInv=setInv,
       getInv=getInv)

}


## Write a short comment describing this function


##CacheSolve function checks whether the inverse of 'x' already exists cache memory
##If it exists, it retrieves that value and exits. If it doesn't exist in cache memory, it computes it
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  
  Inv<- x$getInv()
  
  ##If Inverse is stored in Cache memory,it is returned and the programme exits
  If(!is.null(Inv)) {
        
          message("getting cached data")
          return(Inv)
    }
  
  data <- x$get()
  ##Computing the Inverse
  Inv <- solve(data)
  ##Caching the Inverse
  x$setInv(Inv)
  Inv
  
}

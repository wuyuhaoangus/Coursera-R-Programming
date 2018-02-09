makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setVal <- function(n) {
      x <<- n
      m <<- NULL
  }
  
  getVal <- function() x
  
  setCache <- function() {
      m <<- solve(x)
  }
  
  getCache <- function() m
  
  list(setVal = setVal, getVal=getVal, setCache = setCache, getCache = getCache)
}

cacheSolve <- function(x,...){
    m<-x$getCache()
    if(!is.null(m)){
        message("getting cached solved matrix")
        return(m)
    }
    x$setCache()
    x$getCache()
}
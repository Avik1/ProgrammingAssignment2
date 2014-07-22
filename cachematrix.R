## this function defines a "class" makeCacheMatrix which when instanciated
## it recieves a matrix (for this assignment supposed to be invertible) 
## we want to invert, and caches it for repeated usage. 

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  setMtrx <- function(y){    
    x <<- y         ## assigns 'x' and 'im' in the parent environment programatically
    im <<- NULL     ## this mimics somewhat global variables in say 'C' 
                    ## which will help the function cacheSolve return 'im' from
                    ## the pparent environment if it is assigned a value without
                    ## having to recalculate it
  }
  getMtrx <- function() x   ## get interface funcion for the input matrix
  setInvertM <- function(invrt) im <<- invrt  ## set interface for inverted matrix
  getInvertM <- function() im                 ## set interface for inverted matrix
  
  ## list for advertising makeCacheMatrix "class" interface functions
  list(setMtrx = setMtrx, getMtrx = getMtrx, setInvertM = setInvertM, getInvertM = getInvertM)
}


## This function uses makeCacheMatrix "class" to return an inverse to the
## matrix instaciated by calling makeCacheMatrix. To compute the invert matrix
## the solve() function is called

cacheSolve <- function(x, ...) {
  im <- x$getInvertM()
  if(!is.null(im)) {    ## A cached inverted matrix is available, so skip computing and return it
    message("getting cached matrix")
    return(im)
  }
  data <- x$getMtrx()   ## get the input vector instanciated in makeCacheMatrix
  im <- solve(data)     ## get an invert matrix
  x$setInvertM(im)      ## cache the inverted matrix for repeated use
  im
}

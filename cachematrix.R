#makeCacheMatrix function takes the values of the input matrix and computes the 
#inverse it.


makeCacheMatrix <- function(x = matrix()) {

#'inv' is the variable into which the inverse of the input matrix will be stored.

  inv<-NULL

#'set' function takes the values of the input matrix and assigns it 'x'
  set<-function(y){
 
  x<<-y
  inv<<-NULL
  
}

#gets the values of input matrix 'x'

  get<-function()x

#computes the inverse of the input matrix that is given values via 'set'

  setinverse<-function(solve)inv<<-solve

#prints out the inverse of the input matrix computed via setinverse
  getinverse<-function()inv

#'list' is the set of values being returned via makeCacheMatrix
  list(set = set,get = get,setinverse = setinverse, getinverse = getinverse)
  
}


#below function checks if the inverse of the input matrix has been calculated
#in the past, if yes then it says that it retrieves the cached value
#if no if computes the inverse of the matrix

  cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
 
##the 'if' condition below is checking if the value of the inverse is already calculated
  if(!is.null(inv)) {
    
    
#if value of the inv is already calculated the 'if' function returns "TRUE"
#and the message in the below line is printed.
  
  message("getting cached data")
  
   
  return(inv)
    
  }
  
#if a different set of input values is given, the 'if' condition above
#returns a "FALSE" and the code below is executed.
#below line gets the new input matrix whose inverse is not yet been calculated.


  data <- x$get()


#below line calculated the inverse of the matrix using 'solve' function

  inv <- solve(data, ...)


#below function is passing the 'inv' value to the 'setinverse' function. So that the
#value is stored in the cache and returned next time when the same input matrix
#is passed.
  

  x$setinverse(inv)
  
#'inv' is the value being returned by this function if a new set of 
#' #'input matrix is passed whose inverse is not calculated below.
 
  inv
}

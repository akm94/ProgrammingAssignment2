# The following functions are used to Invert a matrix and avoid repeating computations of inverting matrix unless the matrix changes
# makeCachematrix allows a matrix argument.
# it does the following :
# 1) sets the matrix into a function
# 2) gets the matrix with a function
# 3) creates a function with which Inversematrix can be obtained which is helpful in the cachesolve to check whether Inverted matrix is empty or not
# 4) Finally it creates a setter function which is used to mutate the inversematrix data in cachesolve.
makeCachematrix<- function(z = matrix()){ ## This initializes the matrix z as an arg to makecachematrix
  InverseMat<- NULL                     ## function. The following code creates InverseMat and setMat for input of future Inverse matrix 
  setMat<- function(y)
  {
    z<<-y
    InverseMat<<- NULL
  }
  getMat<- function()z                  ## getMat is a function which obtains z matrix, useful to obtain z matrix in cachesolve
  getInverseMat<- function() InverseMat ## getInverseMat obatains InverseMat, which would be useful to obtain Inverse Matrix and check whether it is empty in cachesolve
  setInverseMat<- function(InverseMatrix) InverseMat<<- InverseMatrix ## setInveseMat is useful to put final Inverted Matrix from cachesolve to z
  
  list(getMat=getMat, setMat= setMat, setInverseMat = setInverseMat,
       getInverseMat = getInverseMat) ## Listing all functions would allow them to be called in cachesolve
}
cachesolve<- function(z,...){
  InverseMat<- z$getInverseMat() ## It obtains the inverted matrix from the previous function.
  if(!is.null(InverseMat)){
    message("getting cached Matrix") ## Checks if there is an existing Inverse Matrix and returns if found
    return(InverseMat)
  }
  InverseMat<- solve(z$getMat()) ## if the Inverse Matrix is empty than forms the inverted matrix and adds it to makeCachematrix by setInversematrix function
  z$setInverseMat(InverseMat)
  InverseMat
}
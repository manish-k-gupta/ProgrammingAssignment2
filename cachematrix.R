## Below are two functions, makeCacheMatrix - for creating a special "matrix" object which can also store its 
## inverse, and cacheSolve to compute the inverse of the matrix returned by makeCacheMatrix. If matrix inverse 
## is already computed then cacheSolve will retrieve it from the cache


## makeCacheMatrix - This function will create special "matrix" object, it also has provision for caching the
##                   inverse of the matrix which is returned by this function, this inverse will be computed by 
##                   another function cacheSolve
##                   
##                   This special "matrix" object is basically a list consist of following functions
##                           set the value of matrix
##                           get the value of matrix
##                           set the value of inverse of the matrix
##                           get the value of inverse of the matrix        

makeCacheMatrix <- function(x = matrix()) {
        
        ## Variables
        ##      x -> matrix
        ##      inverseMatrix -> To store inverse of the matrix "x", intialized to null in the begining
        ##      newMatrix ->  to set the value of "x"
        ##      inverse -> to set the valuse of invserseMatrix
        ## functions
        ##      setMatrixF -> set the value of matrix
        ##      getMatrixF -> get the value of matrix
        ##      setMatrixInverseF -> set the value of inverse of the matrix
        ##      getInverseMatrixF -> get the value of inverse of the matrix
        ## output
        ##      list of above mentioned functions
        
        ##################################################################################################
        ## Amendment History                                                                            ##
        ##                                                                                              ##
        ## Manish Gupta         21st May        Initial Version                                         ##
        ## Manish Gupta         22nd May        Added more comments                                     ##
        ##################################################################################################
                
        inverseMatrix <- NULL                   ## initializing to Null in the begining

        setMatrixF <- function(newMatrix) {
                x <<- newMatrix
                inverseMatrix <<- NULL          ## initializing to null again, because matrix "x" may get changed 
        }                                                                               
                                        
        getMatrixF <- function() x
        
        setMatrixInverseF <- function(inverse) inverseMatrix <<- inverse
        
        getInverseMatrixF <- function() inverseMatrix
        
        list(setMatrixF = setMatrixF, getMatrixF = getMatrixF, setMatrixInverseF = setMatrixInverseF, 
             getInverseMatrixF = getInverseMatrixF) ## return list of functions
      
}

## cacheSolve - This function computes the inverse of the special "matrix" object created by function
##              makeCacheMatrix, if the inverse is already calculated, it will retrieve it from cache 
##                      this function will use the following functions
##                      get the value of matrix        
##                      set the value of inverse of the matrix
##                      get the value of inverse of the matrix
##

cacheSolve <- function(x, ...) {
        
        ## Variables
        ##      inverseMatrix -> inverse of the matrix "x"
        ##      tempMatrix -> to get the original matrix for calculating inverse
        ## functions used
        ##      getMatrixF -> get the value of matrix
        ##      setMatrixInverseF -> set the value of inverse of the matrix
        ##      getInverseMatrixF -> get the value of inverse of the matrix
        ## output
        ##      returns inverse of the matrix "x" 
        ##      if the inverse of matrix is already calculated, it will fetch it from cache
        
        ##################################################################################################
        ## Amendment History                                                                            ##
        ##                                                                                              ##
        ## Manish Gupta         21st May        Initial Version                                         ##
        ## Manish Gupta         22nd May        Added more comments                                     ##
        ##################################################################################################        
        
        inverseMatrix <- x$getInverseMatrixF()
        
        if(!is.null(inverseMatrix)) {           
                message("getting inverse matrix from cache")
                return(inverseMatrix)
        }
        
        tempMatrix <- x$getMatrixF()            # to store the matrix
        inverseMatrix <- solve(tempMatrix, ...) 
        x$setMatrixInverseF(inverseMatrix)
        inverseMatrix 
}

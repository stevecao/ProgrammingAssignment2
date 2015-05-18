## The purpose is to cache the inverse matrix so that the system does not have to 
## calculate it each time you need it.  Caching improves the efficency of the
## program

## This function makeCacheMatrix takes in a square matrix.  The matrix needs to be
## square so we can take the inverse of it.  It will store the matrix in the 
## global environment.

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL  #Set inverse matrix (im) to NULL
	#Check if it's a square matrix
	if (ncol (x) == nrow(x)) { 
		#This set function stores the new matrix in the global environment
		set <- function (newMatrix) {
			x <<- newMatrix  #store the new matrix in the global environment
			im <<- NULL  #Set the inverse matrix to null for the new Matrix
		}	
		#This get function returns the stored matrix
		get <- function() x 

		#This setInverseMatrix stores the inverse matrix in global environment
		setInverseMatrix <- function (inverseMatrix) {
			im <<- inverseMatrix #store the inverse matrix in global environment variable im
		}

		#This getInverseMatrix function retrieves the inverse matrix in global environment
		getInverseMatrix <- function() im

		#Return a list of 4 functions
		list (set=set, get=get, setInverseMatrix=setInverseMatrix, 
			getInverseMatrix=getInverseMatrix)

	} else {  #If the input is not a square matrix, output a warning message.
		message ("The input matrix is not a square matrix.")
		message ("Please input a square matrix (nrow = ncol)")
	}
}


## This cacheSolve function will return the inverse matrix if it's in cache (global environment variable
## im is not NULL.  If the varial im is NULL, it will calculate the inverse matrix and store it in the
## global environment variable im and return it as well.

cacheSolve <- function(x, ...) {
	im <- x$getInverseMatrix()
	
	#If inverse matrix is not NULL, return the cached inverse matrix.
	if(!is.null(im)) {
		message("getting cached inverse matrix")
		return (im)
	}
	#Since the inverse matrix is null, we need to get the cached matrix to calculate the inverse matrix
	data <- x$get()
	#calculate the inverse matrix using solve function 
	im <- solve(data, ...)
	#Store the inverse matrix in global environment
	x$setInverseMatrix(im)
	#Return the inverse matrix
	im
}

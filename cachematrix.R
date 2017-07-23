## Put comments here that give an overall description of what your functions do
# We will create a matrix and inverse this matrix with a first function. Also, the first function will be able to cache the inverse of this matrix 
# in the system. A second function will check if the inverse of a matrix is already cached in the system. If yes, the function will restore the result
# from the cache, if not the function will calculate the inverse of the matrix and save it in the cache.
# Test with:    1. Create e.g. aMatrix <- matrix(c(2, 3, 1, 2), 2, 2).
#               2. Create test_Matrix <- makeCacheMatrix(aMatrix).
#               3. Show the matrix with test_Matrix$get().
#               4. Show the not yet calcuated inverse of the matrix with test_Matrix$getsolve() --> will be NULL.
#               5. Run cacheSolve(test_Matrix) to check if the inverse is already cached --> not the case so the function will calculate the inverse.
#               6. Test the function with test_Matrix$getsolve() --> since the inverse is already stored in the cache, the message "getting cached data" and 
#                  the result is shown.
#               7. Test the function by first creating and then setting a new matrix test_Matrix$set("NEWMATRIX") and check again with steps 5 and 4.
#
## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

# 
makeCacheMatrix <- function(x = matrix()) {     # "x" is a matrix we first have to define.
        s <- NULL                               # "s" is the solution to the SOLVE function which is set to NULL in the beginning and will be filled with a calculated value in the function.
        set <- function(y) {
                x <<- y                         # With the <<- assignment operator we assign the value on the right side of the operator to the object "x" in the parent environment.
                s <<- NULL                      # Assign NULL to the "s" object in the parent environment. This clears any value of "s" that had been cached by an earlier execution of cachesolve().
        }
        get <- function() x                     # We retrieve "x" from the parent environment of makeCacheMatrix().
        setsolve <- function(solve) s <<- solve # We use the <<- assignment operator to assign the input argument to "the value of "s" in the parent environment.
        getsolve <- function() s                # Due to lexical scoping we find the correct symbol "s" to retrieve its value from the parent environment.
        list(set = set, get = get,  
             setsolve = setsolve,
             getsolve = getsolve)               # We return a fully formed object of type makeMatrixCache() which will be used in the second function. Each element in the list is named which allows us to use the $ extract operator to access the functions by name. 
}

## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
#
cacheSolve <- function(x, ...) {                # Return a matrix that is the inverse of 'x'.
        s <- x$getsolve()                       # We call the getsolve() function on the input object.
        if(!is.null(s)) {                       # We check whether the result is NULL. Since makeMatrixCache() sets the cached inverse to NULL whenever a new matrix is set into the object, if the value here is not equal to NULL, we have a valid, cached inversen and can return it to the parent environment.
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s                                       # If the result of !is.null(s) is FALSE, cachesolve() gets the matrix from the input object, calculates a solve(), uses the setsolve() function on the input object to set the inverse in the input object, and then returns the inverse to the parent environment by printing the inverse.
}


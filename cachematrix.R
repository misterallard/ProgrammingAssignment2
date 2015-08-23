## Two functions to prevent unnecessary calculation of a matrix's inverse:
## makeCacheMatrix: takes a matrix as an input and outputs a special list of 4 functions:
        #set/get: assign/retreive the input matrix (stored as a variable within the fcn's environment)
        #setmi/getmi: assign/retrieve the matrix inverse
## cacheSolve: uses special list "matrix" created by makeCacheMatrix as input
        #If inverse is already defined, it just retrieves it
        #Otherwise it calculates the inverse


## Exact same logic as Peng's makeVector function since it will work any time you have an input and an output to cache
## I changed the names of the vars so we're not calling the inverse the "mean"
        # m becomes mi
        # setmean/getmean become setmi/getmi
        # x and y keep their names
makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setmi <- function(matrix_inverse) mi <<- matrix_inverse
        getmi <- function() mi
        list(set = set, get = get,
             setmi = setmi,
             getmi = getmi)
}


## calculate the inverse of a matrix only when it is unknown
        # Input is a "Matrix" that is actually stored in a list of 4 values, one of the list values is for the matrix inverse
        # If a non-null inverse is in the input "Matrix" list, we skip the solve step and return the stored inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getmi()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...) #solve() function calculates matrix inverse
        x$setmi(mi)
        mi
}






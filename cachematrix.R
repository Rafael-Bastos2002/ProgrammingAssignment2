## Two functions created to prompt the user for matrix, store it,
## generate the inverse form of the one prompted, and then print
## and store the result

## This function stores the original matrix prompted and it's inverse

makeCacheMatrix <- function(matriz = matrix()) {
        inverse_form <- NULL
        
        set_original_matrix <- function(new_matriz){
            matriz <<- new_matriz
            inverse_form <<- NULL
        }
        
        get_original_matrix <- function(){matriz}
        
        set_inverse <- function(inver){
            inverse_form <<- inver
        }
        
        get_inverse <- function(){inverse_form}
        
        list(set_original_matrix = set_original_matrix,
             get_original_matrix = get_original_matrix,
             set_inverse = set_inverse, get_inverse = get_inverse)
}

## This function proceeds to search within the cache for the inverse data
## and print it. If not found just calculate, print and store
## (Also, JoJo references...)

cacheSolve <- function(matrix_object, ...){
        if (!is.null(matrix_object$get_inverse())){
            print("I, Giorno Giovanna, will get your cache...")
            print("...")
            return(matrix_object$get_inverse())
        }
        else{
            print("I, Giorno Giovanna, couldn't find a cache...")
            print("Proceeding to calculate it")
            data <- matrix_object$get_original_matrix()
            result <- solve(data)
            matrix_object$set_inverse(result)
            print("...")
            print("Here is your inverse matrix: ")
            result
            }
}
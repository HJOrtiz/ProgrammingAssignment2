### Pair of functions that are used to create a special object that 
### stores a matrix and caches its inverse. There is a first function that
### creates a special matrix. The second function calculates the inverse of the
### special matrix.

### This first function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i = NULL
  set = function(y) {
    x = y
    i = NULL
                     }
  
  get = function() x
  set.inverse = function(inverse) i = inverse
  get.inverse = function() i
  list(set = set,
       get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
  
                                          }

## This second function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i = x$get.inverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
                   }
  mat = x$get()
  i = solve(mat, ...)
  x$set.inverse(i)
  i
                                }

### Example: Let say we want to test the functions. the program creates first 
### a matrix that you normally would do on R and then the functions created 
### and the output results.
matrix = matrix(rnorm(100),10,10)
Cachematrix = makeCacheMatrix(matrix)
cacheSolve(Cachematrix)
             [,1]         [,2]        [,3]        [,4]         [,5]        [,6]
 [1,] -0.35601047  0.007115346 -0.30304710  0.08030224 -0.066041094 -0.10336453
 [2,]  0.63118331  0.344523293  0.04304780 -0.22399626 -0.558256411  0.36187776
 [3,]  0.31986113  0.218926983  0.04749454  0.41525580 -0.461942291  0.35030148
 [4,]  0.22351116  0.041001857 -0.26319652  0.62646520  0.001706825  0.40857629
 [5,] -0.06964005 -0.203551563 -0.20218345  0.32777538 -0.128934127  0.09153218
 [6,]  0.35251962  0.047227862  0.39312771 -0.57733075 -0.173813057  0.35662183
 [7,]  0.01485483 -0.434632014  0.10105743  0.74421858  0.302111759 -0.16352155
 [8,] -0.65793066 -0.015522135  0.03105431  0.24824555  0.560605139 -0.30424562
 [9,]  0.19660605 -0.148045196 -0.18223067  0.49078422  0.334207672  0.17357203
[10,]  0.58896894  0.901197507  0.06589227 -0.82194330 -1.350010931  0.99394822
             [,7]        [,8]        [,9]        [,10]
 [1,]  0.06650154 -0.04557326  0.02116965 -0.262626170
 [2,] -0.25934646  0.03621801 -0.06421143 -0.004455598
 [3,] -0.04623437 -0.11554668 -0.19858744  0.017316610
 [4,]  0.17909779 -0.40504712 -0.27324861  0.389795009
 [5,]  0.11121954  0.04942659  0.03131758 -0.059459300
 [6,] -0.22108300 -0.03676777  0.01530130 -0.004963640
 [7,]  0.35719257 -0.63810735  0.06192845  0.095187456
 [8,] -0.29890005  0.52220816  0.51107068 -0.228881802
 [9,]  0.53611034 -0.31706245 -0.25526177  0.106856814
[10,] -0.42460731  0.65476244  0.02513085 -0.015331326








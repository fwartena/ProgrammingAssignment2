test <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
testCache <- makeCacheMatrix(test)
cacheSolve(testCache)
solve(test)
cacheSolve(testCache)

test2 <- matrix(c(1,2,3,4), nrow = 4, ncol = 1)
testCache2 <- makeCacheMatrix(test2)
cacheSolve(testCache2)
solve(test2)
cacheSolve(testCache2)

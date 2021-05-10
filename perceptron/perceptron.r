f <- function(net){
    if (net>=0.5) return(1)
    return (0)
}

perceptron.test <- function(x, weights){
    net = c(as.numeric(x),1) %*% weights
    return (f(net))
}

perceptron.train <- function(dataset, eta=0.1, threshold=1e-3){
    classId = ncol(dataset)
    X = dataset[,1:(classId-1)]
    Y = dataset[,classId]

    weights = runif(min=-0.5, max=0.5, n=classId)

    sqerror = 2 * threshold

    while(sqerror > threshold){
        sqerror = 0
        for(i in 1:nrow(X)){
            x = as.numeric(X[i,])
            y = Y[i]

            net = c(x,1) %*% weights 
            y.o = f(net)

            error = y - y.o
            sqerror = sqerror + error^2

            # derivada do erro em w1,w2 e theta
            dE2 = 2*error*-c(x,1)

            weights = weights - eta * dE2

        }
        sqerror = sqerror / nrow(X)
        cat("sqerror = ", sqerror, "\n")
    }

    return (weights)
}

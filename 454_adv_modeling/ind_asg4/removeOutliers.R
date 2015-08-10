removeOutliers = function(input) {
  col.names = colnames(input)
  for (i in 1 : ncol(input)) {
    col.ind = which(names(input)== col.names[i])
    q1 = quantile(input[,c(col.ind)], c(0.25))
    q3 = quantile(input[,c(col.ind)], c(0.75))
    iqr = IQR(input[,c(col.ind)])
    lb = q1 - 1.5*iqr
    ub = q3 + 1.5*iqr

    input[,c(col.ind)] = ifelse(input[,c(col.ind)] < lb, lb, input[,c(col.ind)])
    input[,c(col.ind)] = ifelse(input[,c(col.ind)] > ub, ub, input[,c(col.ind)])
  }
  return (input)
}
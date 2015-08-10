normalize = function(input) {
  col.names = colnames(input)
  for (i in 1:ncol(input)) {
    col.ind = which(names(input)== col.names[i])
    if (input[,c(col.ind)] != "factor") {
      col.ind = which(names(input) == col.names[i])
      input[,c(col.ind)] = (input[,c(col.ind)] - min(input[,c(col.ind)]))/(max(input[,c(col.ind)]) - min(input[,c(col.ind)]))
    }
  }
  return (input)
}
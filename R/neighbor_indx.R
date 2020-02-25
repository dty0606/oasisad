neighbor_indx <- function(x,
                          dim){
  #grid
  temp <- expand.grid(c(x[1]-1,x[1],x[1]+1),
                      c(x[2]-1,x[2],x[2]+1),
                      c(x[3]-1,x[3],x[3]+1))
  #calculate distance
  temp1 <- cbind(temp,
                (temp[,1]-x[1])^2 + (temp[,2]-x[2])^2 + (temp[,3]-x[3])^2)
  #select nearest neighbor
  temp <- temp[which(temp1[,4]=1),]
  #return their vectorized indice
  indx <- temp[,1] + (temp[,2]-1)*dim[1] + (temp[,3]-1)*dim[1]*dim[2]
  return(indx)
}

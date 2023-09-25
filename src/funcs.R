# If the directory does not exist, create a new directory
check_path <- function(path) {
  if (!dir_exists(path)) {
    dir_create(path)
  }
}

# Clear all directories and files in folder but don't delete folder
clear_data <- function(path) {
  if (dir_exists((path))) {
    items <- dir_ls(path)
    
    for (item in items) {
      if (is_dir(item)) {
        dir_delete(item, recursive = TRUE, force = TRUE)
      } else {
        file_delete(item)
      }
    }
  }
}

p <- NULL
pp <- NULL

target.coef <- function(P) {
  X0 <- P[1]
  Xs <- P[2]
  DFIs <- P[3]
  CFIs <- P[4]
  
  a <- -X0 * (2 * CFIs * Xs - CFIs * X0 - Xs ^ 2 * DFIs + Xs * DFIs * X0) / (Xs ^ 2 - 2 * X0 * Xs + X0 ^ 2)
  b <- (-Xs ^ 2 * DFIs + DFIs * X0 ^ 2 + 2 * CFIs * Xs) / (Xs ^ 2 - 2 * X0 * Xs + X0 ^ 2)
  c <- -(CFIs - Xs * DFIs + X0 * DFIs) / (Xs ^ 2 - 2 * X0 * Xs + X0 ^ 2)
  
  pp <- as.vector(c(a, b, c))
  return(pp)
}

nls.func <- function(X0, Xs, DFIs, CFIs) {
  pp <- c(X0, Xs, DFIs, CFIs)
  # print(paste0('X0: ', X0))
  # print(paste0('Xs: ', Xs))
  # print(paste0('DFIs: ', DFIs))
  # print(paste0('CFIs: ', CFIs))
  
  #calculation of a, b and c using these new parameters
  a <- target.coef(pp)[1]
  b <- target.coef(pp)[2]
  c <- target.coef(pp)[3]
  
  ind1 <- as.numeric(X < Xs)
  return (ind1 * (a + b * X + c * X ^ 2) + (1 - ind1) * ((a + b * Xs + c * Xs ^ 2)+(b + 2 * c * Xs) * (X - Xs)))
}

predict.target.trajectory <- function(par, age){
  #
  X0 <- par[1]
  Xs <- par[2]
  DFIs <- par[3]
  CFIs <- par[4]
  #
  x <- age
  #calculation of a, b and c using these new parameters
  func.coef <- target.coef(par)
  c <- func.coef[3]
  b <- func.coef[2]
  a <- func.coef[1]
  
  #
  brk <- as.numeric(x < Xs)
  #
  result <- list()
  cfi <- brk * (a + b * x + c * x ^ 2) + (1 - brk) * ((a + b * Xs + c * Xs ^ 2) + (b + 2 * c * Xs) * (x - Xs))  #CFI
  dfi <- brk * (b + 2 * c * x) + (1 - brk) * (b+ 2 * c * Xs)                                 #DFI
  result[[1]] <- cfi
  result[[2]] <- dfi
  return (result)
}
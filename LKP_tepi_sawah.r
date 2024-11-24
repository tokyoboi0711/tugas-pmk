# Function untuk metode Bagi Dua (Bisection)
bagi_dua <- function(f, a, b, error = 1e-4, max_iter = 20) { # errornya mengambil dari nomor 2
  if (f(a) * f(b) > 0) {
    stop("f(a) dan f(b) harus berbeda tanda")
  }
  
  hasil <- matrix(nrow = max_iter, ncol = 5)
  colnames(hasil) <- c("i", "a", "b", "c", "f(c)")
  
  for(i in 1:max_iter) {
    c <- (a + b) / 2
    fc <- f(c)
    
    hasil[i,] <- c(i, a, b, c, fc)
    
    if (abs(fc) < error) {
      return(list(iterasi = hasil[1:i,], akar = c, iterations = i))
    }
    
    if (fc * f(a) < 0) {
      b <- c
    } else {
      a <- c
    }
  }
  return(list(iterasi = hasil[1:i,], akar = c, iterations = i))
}

# Function untuk metode Regula Falsi
regula_falsi <- function(f, a, b, error = 1e-4, max_iter = 20) { #errornya mengambil dari nomor 2
  if (f(a) * f(b) > 0) {
    stop("f(a) dan f(b) harus berbeda tanda")
  }
  
  hasil <- matrix(nrow = max_iter, ncol = 5)
  colnames(hasil) <- c("i", "a", "b", "c", "f(c)")
  
  for(i in 1:max_iter) {
    c <- b -( (f(b) * (b - a)) / (f(b) - f(a)))
    fc <- f(c)
    
    hasil[i,] <- c(i, a, b, c, fc)
    
    if (abs(fc) < error) {
      return(list(iterasi = hasil[1:i,], akar = c, iterations = i))
    }
    
    if (fc * f(a) < 0) {
      b <- c
    } else {
      a <- c
    }
  }
  return(list(iterasi = hasil[1:i,], akar = c, iterations = i))
}

# Contoh penggunaan:
f <- function(x) 4*x^3 - 15*x^2 + 17*x - 6
# Metode Regula Falsi
hasil_regula_falsi <- regula_falsi(f, -1, 3)
print(hasil_regula_falsi)


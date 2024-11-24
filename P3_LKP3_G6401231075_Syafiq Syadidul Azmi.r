#1.a
print(diag(100)) #menampilkan matriks identitas 100x100 pada terminal

#1.b
matrixABC = matrix(0, nrow = 5, ncol = 10) #membuat matriks 5x10 dengan nilai 0
matrixABC[1, ] = "A" #mengisi baris pertama dengan huruf A
matrixABC[2, ] = "B" #mengisi baris kedua dengan huruf B
matrixABC[3, ] = "C" #mengisi baris ketiga dengan huruf C
matrixABC[4, ] = "D" #mengisi baris keempat dengan huruf D
matrixABC[5, ] = "E" #mengisi baris kelima dengan huruf E

print(matrixABC) #menampilkan matriks pada terminal

# no.2

rumusKecap = function(a, b, c){  #membuat fungsi rumusKecap dengan parameter a, b, c
  x1 = (-b + sqrt(b^2 - 4*a*c))/2*a #menghitung x1 atau faktor pertama dengan rumus yang tertera dengan notasi (+)
  x2 = (-b - sqrt(b^2 - 4*a*c))/2*a #menghitung x2 atau faktor kedua dengan rumus yang tertera dengan notasi (-)
  return(rbind(x1, x2)) #mengembalikan nilai x1 dan x2 dalam bentuk matriks
}

rumusKecap(1, -5, 6) #memanggil fungsi rumusKecap dengan parameter 1, -5, 6
rumusKecap(1, 0, -4) #memanggil fungsi rumusKecap dengan parameter 1, 0, -4


#no.3
set.seed(47) #mengatur seed agar hasil random tidak berubah
x <- seq(1, 20, by = 0.8) #menentukan nilai x dari 1 sampai 20 dengan interval 0.8
y <- rnorm(length(x), mean = 1, sd = 0.5) / x #menghitung nilai y dengan rumus yang tertera

plot(x, y, type = "l",  xlab = "Nilai x", ylab = "Nilai y", main = "grafik nomor 3") 
#membuat plot grafik dengan nilai x dan y yang sudah dihitung

#no.4
x <- 1:50 #menentukan nilai x dari 1 sampai 50
y <- (x+3)/((x^2)*sqrt(x)) #menghitung nilai y dengan rumus yang tertera

plot(x, y, type = "l",  xlab = "Nilai x", ylab = "Nilai y", main = "grafik nomor 4")
#membuat plot grafik dengan nilai x dan y yang sudah dihitung

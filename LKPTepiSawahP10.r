# Mendefinisikan fungsi yang akan diintegrasikan ganda
fungsi <- function(x, y) {
  x^2 * y^3 - x * y
}

# Mendefinisikan rentang nilai untuk integrasi
nilai_x_soal <- seq(1, 3, by = 0.5)  # Rentang x dari 1 sampai 3 dengan langkah 0.5
nilai_y_soal <- seq(0, 2, by = 0.5)  # Rentang y dari 0 sampai 2 dengan langkah 0.5

# Fungsi integrasi ganda menggunakan metode trapesium untuk kedua variabel
trapesium_2d <- function(fungsi, nilai_x, nilai_y) {
  jumlah_x <- length(nilai_x)
  jumlah_y <- length(nilai_y)
  hasil_jumlah <- 0
  
  for (i in 1:jumlah_x) { # untuk setiap titik x
    for (j in 1:jumlah_y) { # untuk setiap titik y di masing masing titik x (dydx)
      bobot_x <- ifelse(i == 1 || i == jumlah_x, 1, 2) # Jika jumlah i == 1 atau i == jumlah_x (i di ujung), return 1. lainnya 2
      bobot_y <- ifelse(j == 1 || j == jumlah_y, 1, 2)
      hasil_jumlah <- hasil_jumlah + bobot_x * bobot_y * fungsi(nilai_x[i], nilai_y[j])
    }
  }
  hx <- diff(nilai_x)[1]  # mencari delta x (di soal dituliskan 0.5, namun di fungsi ini mencari selisihnya)
  hy <- diff(nilai_y)[1]   
  integral <- (hx * hy / 4) * hasil_jumlah
  return(integral)
}

# Fungsi integrasi ganda menggunakan metode Simpson untuk kedua variabel
simpson_2d <- function(fungsi, nilai_x, nilai_y) {
  jumlah_x <- length(nilai_x)
  jumlah_y <- length(nilai_y)
  hasil_jumlah <- 0
  
  for (i in 1:jumlah_x) {
    for (j in 1:jumlah_y) {
      bobot_x <- ifelse(i == 1 || i == jumlah_x, 1, ifelse(i %% 2 == 0, 4, 2)) # jika dia bukan titik ujung dan dia genap dia mengembalikan nilai 2, kalau ganjil 4
      bobot_y <- ifelse(j == 1 || j == jumlah_y, 1, ifelse(j %% 2 == 0, 4, 2)) # Bobot Simpson untuk y
      hasil_jumlah <- hasil_jumlah + bobot_x * bobot_y * fungsi(nilai_x[i], nilai_y[j])
    }
  }
  h_x <- diff(nilai_x)[1]
  h_y <- diff(nilai_y)[1]
  integral <- (h_x * h_y / 9) * hasil_jumlah # hx*hy / 9 diambil dari rumus simpson yaitu h/3 untuk masing masing x ataupun y
  return(integral)
}

# Metode kombinasi: Trapesium untuk x dan Simpson untuk y
trapesium_x_simpson_y <- function(fungsi, nilai_x, nilai_y) {
  jumlah_x <- length(nilai_x)
  jumlah_y <- length(nilai_y)
  h_x <- diff(nilai_x)[1]
  h_y <- diff(nilai_y)[1]
  
  integral <- 0
  for (i in 1:jumlah_x) { # dydx
    bobot_x <- ifelse(i == 1 || i == jumlah_x, 1, 2) # Bobot trapesium untuk x
    sum_y <- 0
    for (j in 1:jumlah_y) {
      bobot_y <- ifelse(j == 1 || j == jumlah_y, 1, ifelse(j %% 2 == 0, 4, 2)) # Bobot Simpson untuk y
      sum_y <- sum_y + bobot_y * fungsi(nilai_x[i], nilai_y[j])
    }
    integral <- integral + bobot_x * (h_y / 3) * sum_y
  }
  integral <- (h_x / 2) * integral
  return(integral)
}

# Metode kombinasi: Simpson untuk x dan Trapesium untuk y
simpson_x_trapesium_y <- function(fungsi, nilai_x, nilai_y) {
  jumlah_x <- length(nilai_x)
  jumlah_y <- length(nilai_y)
  h_x <- diff(nilai_x)[1]
  h_y <- diff(nilai_y)[1]
  
  integral <- 0
  for (j in 1:jumlah_y) {
    bobot_y <- ifelse(j == 1 || j == jumlah_y, 1, 2) # Bobot trapesium untuk y
    sum_x <- 0
    for (i in 1:jumlah_x) {
      bobot_x <- ifelse(i == 1 || i == jumlah_x, 1, ifelse(i %% 2 == 0, 4, 2)) # Bobot Simpson untuk x
      sum_x <- sum_x + bobot_x * fungsi(nilai_x[i], nilai_y[j])
    }
    integral <- integral + bobot_y * (h_x / 3) * sum_x
  }
  integral <- (h_y / 2) * integral
  return(integral)
}

# Menghitung hasil integrasi dengan semua metode
hasil_trapesium <- trapesium_2d(fungsi, nilai_x_soal, nilai_y_soal)
hasil_simpson <- simpson_2d(fungsi, nilai_x_soal, nilai_y_soal)
hasil_mix1 <- trapesium_x_simpson_y(fungsi, nilai_x_soal, nilai_y_soal)
hasil_mix2 <- simpson_x_trapesium_y(fungsi, nilai_x_soal, nilai_y_soal)

# Menampilkan hasil perhitungan untuk semua metode
cat("Trapesium untuk kedua arah: ", hasil_trapesium, "\n")
cat("Simpson untuk kedua arah: ", hasil_simpson, "\n")
cat("Trapesium di x, Simpson di y: ", hasil_mix1, "\n")
cat("Simpson di x, Trapesium di y: ", hasil_mix2, "\n")

# Definisikan fungsi
fungsi <- expression(x^3 + 2*x^2 + x)

# Hitung turunan terhadap x
turunan <- D(fungsi, "x")

# Cetak hasil turunan
print(turunan)
3 * x^2 + 4 * x + 1




# Definisikan fungsi
fungsi <- expression(x^2 + y^2)

# Hitung turunan parsial terhadap x
turunan_x <- D(fungsi, "x")

# Hitung turunan parsial terhadap y
turunan_y <- D(fungsi, "y")

# Cetak hasil
print(turunan_x) # 2 * x
print(turunan_y) # 2 * y







# Definisikan fungsi f(x, y)
fungsi <- expression(x^2 + y^2 + 2*x*y - 4*x)
f2a <- expression(y^3 -3*x^2 -3*y^2 + 3*(x^2)*y +2) 
f2b <- expression(exp(-(x^2 + y^2 - 4*y)))
f2d <- expression(x*y + (2/x)+(4/y))

# Hitung turunan parsial pertama
df_dx <- D(fungsi, "x")
df_dy <- D(fungsi, "y")

df_dx <- D(f2a, "x")
df_dy <- D(f2a, "y")

df_dx <- D(f2b, "x")
df_dy <- D(f2b, "y")

df_dx <- D(f2d, "x")
df_dy <- D(f2d, "y")

# Cetak turunan pertama
print(df_dx) # 2*x + 2*y - 4
print(df_dy) # 2*y + 2*x

# Hitung turunan parsial kedua
d2f_dx2 <- D(df_dx, "x")
d2f_dy2 <- D(df_dy, "y")
d2f_dxdy <- D(df_dx, "y")

# Cetak turunan kedua
print(d2f_dx2) # 2
print(d2f_dy2) # 2
print(d2f_dxdy) # 2

# Hitung determinan Hessian
D_hessian <- (d2f_dx2 * d2f_dy2) - (d2f_dxdy)^2
print(D_hessian) # 4



# Fungsi sederhana f(x, y) = x^2 + y^2
fungsi_sederhana <- expression(x^2 + y^2)

# Turunan pertama
df_dx_sederhana <- D(fungsi_sederhana, "x")
df_dy_sederhana <- D(fungsi_sederhana, "y")

# Turunan kedua
d2f_dx2_sederhana <- D(df_dx_sederhana, "x")
d2f_dy2_sederhana <- D(df_dy_sederhana, "y")
d2f_dxdy_sederhana <- D(df_dx_sederhana, "y")


# Determinan Hessian
D_hessian_sederhana <- (d2f_dx2_sederhana * d2f_dy2_sederhana) - (d2f_dxdy_sederhana)^2
print(D_hessian_sederhana) # 4


# Definisikan fungsi tujuan f(x, y)
f <- expression(x^2 + y^2)

# Definisikan kendala g(x, y) = x + y - 1
g <- expression(x + y - 1)

# Definisikan fungsi Lagrange
lagrange <- expression(x^2 + y^2 + lambda * (x + y - 1))

# Hitung turunan parsial Lagrange terhadap x, y, dan lambda
dL_dx <- D(lagrange, "x")
dL_dy <- D(lagrange, "y")
dL_dlambda <- D(lagrange, "lambda")

# Cetak turunan parsial
print(dL_dx) # 2*x + lambda
print(dL_dy) # 2*y + lambda
print(dL_dlambda) # x + y - 1

# Selesaikan sistem persamaan
library(rootSolve)

# Fungsi untuk menyelesaikan persamaan turunan parsial
solve_lagrange <- function(vars) {
  x <- vars[1]
  y <- vars[2]
  lambda <- vars[3]
  
  eq1 <- 2*x + lambda # Turunan parsial terhadap x
  eq2 <- 2*y + lambda # Turunan parsial terhadap y
  eq3 <- x + y - 1 # Kendala
  
  return(c(eq1, eq2, eq3))
}

# Tentukan solusi awal
solusi_awal <- c(1, 1, 1)

# Gunakan rootSolve untuk menyelesaikan sistem persamaan
solusi <- multiroot(f = solve_lagrange, start = solusi_awal)

# Cetak hasil
print(solusi$root) # Solusi untuk x, y, lambda
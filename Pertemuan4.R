library(Deriv)

turunan <- function(fungsi, n){
  if(n == 1){
    Deriv(fungsi)
  }
  else {
    turunan(Deriv(fungsi), n-1)
  }
}

fungsi_taylor <- function(fungsi, x, a, n){
  fx <- fungsi(a)
  for (i in 1:(n-1)) {
    cn <- turunan(fungsi, i)(a)
    fx <- fx + (cn*(x-a)^i/factorial(i))
  }
  return(fx)
}


fx <- function(x) {
  exp(x) * sin(x)
  }

print(fungsi_taylor(fx, 1, 0, 4))
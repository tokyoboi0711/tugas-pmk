# Definisikan fungsi dan kendala
fxy = expression((x+y)^(1/2))

# Turunkan fungsi fxy terhadap x
turunan_x = D(fxy, "x")

# Turunkan fungsi fxy terhadap y
turunan_y = D(fxy, "y")

print(turunan_x)
print(turunan_y)
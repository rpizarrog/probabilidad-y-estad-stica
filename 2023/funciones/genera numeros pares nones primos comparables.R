# Funciones para números del 1  al n
# No incluye el número 0
# Se recomienda 10000 por lo de los números primos
f_generarNumeros <- function(n) {
  nums <- 1:n
  return(nums)
}

# Función que regresa cuales son los pares
f_pares <- function(nums) {
  pares <- which(nums %% 2 == 0)  
  return (pares)
}

# Función que regresa cuales son los nones
f_nones <- function(nums) {
  nones <- which(nums %% 2 == 1)
  return (nones)
}

# Función que regresa cuales son primos
# Se recomienda 10000 por lo de los números primos
# el 1 y el 2 para este algoritmo si son primos
f_primos <- function(nums) {
  primos <- c(1,2)
  n <- length(numeros)
  for (numero in 3:n) {
    cont <- 0
    for (div in 1:numero) {
      if (numero %% div == 0) {
        cont <- cont + 1
        if (cont > 2) {
          break # sale del ciclo divisor y va al siguiente numero
        }
          
      }
    }
    if (cont == 2) {
      primos <- c(primos, numero)
    }
  }
  return(primos)
}


# Función para seleccionar con una comparación menor igual
f_menor <- function(nums, valor) {
  return (nums[nums < valor])
}

# Función para seleccionar con una comparación menor
f_menorigual <- function(nums, valor) {
  return (nums[nums <= valor])
}

# Función para seleccionar con una comparación mayor igual
f_mayor <- function(nums, valor) {
  return (nums[nums > valor])
}

# Función para seleccionar con una comparación mayor
f_mayorigual <- function(nums, valor) {
  return (nums[nums >= valor])
}

f_rango <- function (nums, ri, rs) {
  if (rs < ri) 
    return ("El límite superior debe ser mayor o igual que el limite inferior")
  return (nums[nums >= ri & nums <= rs])
}




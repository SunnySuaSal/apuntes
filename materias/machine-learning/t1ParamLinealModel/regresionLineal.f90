program regresion_lineal
  implicit none

  integer :: n, i, ios
  real, allocatable :: x(:), y(:)
  real :: x_mean, y_mean
  real :: num, den
  real :: a0, a1
  character(len=100) :: linea

  !---------------------------------------------
  ! Contar líneas del archivo
  !---------------------------------------------
  n = 0
  open(unit=10, file="datos.txt", status="old", action="read")

  do
     read(10, '(A)', iostat=ios) linea
     if (ios /= 0) exit
     n = n + 1
  end do

  close(10)

  !---------------------------------------------
  ! Reservar memoria
  !---------------------------------------------
  allocate(x(n), y(n))

  !---------------------------------------------
  ! Leer datos
  !---------------------------------------------
  open(unit=10, file="datos.txt", status="old", action="read")

  do i = 1, n
     read(10, *) x(i), y(i)
  end do

  close(10)

  !---------------------------------------------
  ! Calcular medias
  !---------------------------------------------
  x_mean = sum(x) / n
  y_mean = sum(y) / n

  !---------------------------------------------
  ! Mínimos cuadrados
  !---------------------------------------------
  num = 0.0
  den = 0.0

  do i = 1, n
     num = num + (x(i) - x_mean)*(y(i) - y_mean)
     den = den + (x(i) - x_mean)**2
  end do

  a1 = num / den
  a0 = y_mean - a1*x_mean

  print *, "Numero de datos =", n
  print *, "Pendiente a1 =", a1
  print *, "Intercepto a0 =", a0

  !---------------------------------------------
  ! Crear archivo para graficar
  !---------------------------------------------
  open(unit=20, file="ajuste.dat", status="replace")

  do i = 1, n
     write(20,*) x(i), y(i), a1*x(i) + a0
  end do

  close(20)

  print *, "Archivo 'ajuste.dat' creado."

end program regresion_lineal

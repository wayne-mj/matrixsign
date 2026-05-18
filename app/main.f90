program main
  !> Include the matrixsign modules
  use matrixsign
  implicit none
  !> Define as parameters the height and width of each letter
  integer, parameter   :: height=6, width=5
  !> While not formally use, declare word as an allocatable length 
  !> character string
  character(len=:), allocatable  ::  word

  !> Call the initial user input
  word = user_input(height,width)

end program main


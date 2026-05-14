program main
  use matrixsign
  implicit none
  integer, parameter   :: height=6, width=5
  character(height*(width+1)) :: letter

  letter = make_r(height, width)
  ! print *, len(letter)
  write (*,'(A)') letter
  
  letter = make_e(height, width)
  ! print *, len(letter)
  write (*,'(A)') letter

  letter = make_d(height, width)
  ! print *, len(letter)
  write (*,'(A)') letter

  letter = make_b(height, width)
  ! print *, len(letter)
  write (*,'(A)') letter

  letter = make_l(height, width)
  ! print *, len(letter)
  write (*,'(A)') letter

  letter = make_u(height, width)
  ! print *, len(letter)
  write (*,'(A)') letter
end program main


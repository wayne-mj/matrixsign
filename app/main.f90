program main
  use matrixsign
  implicit none
  integer, parameter   :: height=6, width=5
  character(height*(4*width+1)) :: letter
  character(len=:), allocatable  ::  word
  character(len=:), allocatable  :: render

  word = user_input(height,width)

  ! render = make_marquee(word, height, width)
  ! call display_marquee(render)
  
  ! word = "REDBLUK"
  ! if (validate(word)) then
  !   write (*,*) "Valid"
  ! else 
  !   write (*,*) "Not valid"
  ! end if

  ! letter = upper("aBc")
  ! ! letter = make_marquee(word, height, width)

  ! write (*, '(A)') letter
  ! letter = make_r(height, width)
  ! ! print *, len(letter)
  ! write (*,'(A)') letter
  
  ! letter = make_e(height, width)
  ! ! print *, len(letter)
  ! write (*,'(A)') letter

  ! letter = make_d(height, width)
  ! ! print *, len(letter)
  ! write (*,'(A)') letter

  ! letter = make_b(height, width)
  ! ! print *, len(letter)
  ! write (*,'(A)') letter

  ! letter = make_l(height, width)
  ! ! print *, len(letter)
  ! write (*,'(A)') letter

  ! letter = make_u(height, width)
  ! ! print *, len(letter)
  ! write (*,'(A)') letter
end program main


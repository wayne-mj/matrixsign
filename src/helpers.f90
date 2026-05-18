module helpers
  !> Use the Fortran STDLIB module
  use stdlib_ascii
  implicit none

  private
  public :: exists

contains

!> Function exists check if a character exists in the dictionary
!> As long as the tokens are greater than zero, the character exists
!> and true is return, other wise false is returned
  function exists(ch, str) result(b)
    character(len=1),intent(in)     :: ch
    character(len=*),intent(in)     :: str
    logical                         :: b
    integer                         :: i, token
    integer                         :: lenstr

    lenstr = len_trim(str)    
    b = .false.
    token = 0

    do i=1,lenstr
      if (ch .eq. str(i:i)) then
        token = token + 1
      end if
    end do

    if (token .gt. 0) then
      b = .true.
    else 
      b = .false.
    end if
  end function

end module helpers
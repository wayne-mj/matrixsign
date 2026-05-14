module matrixsign
  implicit none
  private

  public :: make_r, make_e
contains
  ! subroutine say_hello
  !   print *, "Hello, matrixsign!"
  ! end subroutine say_hello

  function make_r(height,width) result(letter_str)
    integer, intent(in)                   :: height, width
    character(len=(height)*(width+1))     :: letter_str
    integer                               :: row,col, pos

    pos = 1
    letter_str = ""
    do row = 0, height-1
      do col = 0, width-1
        if ((col .eq. 1) .or. & 
            (col .eq. 2 .and. (row .eq. 0 .or. row .eq. 2 .or. row .eq. 3)) .or. &
            (col .eq. 3 .and. (row .eq. 1 .or. row .eq. 4 .or. row .eq. 5)) ) then
          ! letter_str = letter_str // "*"
          letter_str(pos:pos) = "*"
        else
          ! letter_str = letter_str // " "
          letter_str(pos:pos) = "."
        end if
        pos = pos + 1
      end do
      ! letter_str = letter_str // new_line('a')
      letter_str(pos:pos) = new_line('a')
      pos = pos + 1
    end do
  end function

  function make_e(height, width) result(letter_str)
    integer, intent(in)                   :: height, width
    character(len=(height)*(width+1))     :: letter_str
    integer                               :: row,col, pos

    pos = 1
    letter_str = ""
    do row = 0, height-1
      do col = 0, width-1
        if ((col .eq. 1) .or. & 
            (col .eq. 2 .or. col .eq. 3) .and. (row .eq. 0 .or. row .eq. 2 .or. row .eq. 5))  then
          ! letter_str = letter_str // "*"
          letter_str(pos:pos) = "*"
        else
          ! letter_str = letter_str // " "
          letter_str(pos:pos) = "."
        end if
        pos = pos + 1
      end do
      ! letter_str = letter_str // new_line('a')
      letter_str(pos:pos) = new_line('a')
      pos = pos + 1
    end do
  end function
end module matrixsign

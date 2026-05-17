module matrixsign
  use stdlib_ascii
  use helpers
  implicit none
  private
  character(len=1), parameter       :: star = "*"
  character(len=1), parameter       :: empt = " "
  integer, parameter                :: dictlen = 6
  character(len=dictlen), parameter :: dictionary = "REDBLU"
  

  public :: make_r, make_e, make_d, make_b, make_l, make_u, make_marquee, validate, user_input
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
          letter_str(pos:pos) = star
        else
          letter_str(pos:pos) = empt
        end if
        pos = pos + 1
      end do
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
          letter_str(pos:pos) = star
        else
          letter_str(pos:pos) = empt
        end if
        pos = pos + 1
      end do
      letter_str(pos:pos) = new_line('a')
      pos = pos + 1
    end do
  end function

  function make_d(height, width) result(letter_str)
    integer, intent(in)                   :: height, width
    character(len=(height)*(width+1))     :: letter_str
    integer                               :: row,col, pos

    pos = 1
    letter_str = ""
    do row = 0, height-1
      do col = 0, width-1
        if ((col .eq. 1) .or. & 
            (col .eq. 2) .and. (row .eq. 0 .or. row .eq. 5) .or. &
            (col .eq. 3) .and. (row .gt. 0 .and. row .lt. 5))  then
          letter_str(pos:pos) = star
        else
          letter_str(pos:pos) = empt
        end if
        pos = pos + 1
      end do
      letter_str(pos:pos) = new_line('a')
      pos = pos + 1
    end do
  end function

  function make_b(height, width) result(letter_str)
    integer, intent(in)                   :: height, width
    character(len=(height)*(width+1))     :: letter_str
    integer                               :: row,col, pos

    pos = 1
    letter_str = ""
    do row = 0, height-1
      do col = 0, width-1
        if ((col .eq. 1) .or. & 
            (col .eq. 2) .and. (row .eq. 0 .or. row .eq. 2 .or. row .eq. 5) .or. &
            (col .eq. 3) .and. (row .eq. 1 .or. row .eq. 3 .or. row .eq.4))  then
          letter_str(pos:pos) = star
        else
          letter_str(pos:pos) = empt
        end if
        pos = pos + 1
      end do
      letter_str(pos:pos) = new_line('a')
      pos = pos + 1
    end do
  end function

  function make_l(height, width) result(letter_str)
    integer, intent(in)                   :: height, width
    character(len=(height)*(width+1))     :: letter_str
    integer                               :: row,col, pos

    pos = 1
    letter_str = ""
    do row = 0, height-1
      do col = 0, width-1
        if ((col .eq. 1) .or. & 
            (col .gt. 0 .and. col .lt. 4 .and. row .eq. 5) )  then
          letter_str(pos:pos) = star
        else
          letter_str(pos:pos) = empt
        end if
        pos = pos + 1
      end do
      letter_str(pos:pos) = new_line('a')
      pos = pos + 1
    end do
  end function

  function make_u(height, width) result(letter_str)
    integer, intent(in)                   :: height, width
    character(len=(height)*(width+1))     :: letter_str
    integer                               :: row,col, pos

    pos = 1
    letter_str = ""
    do row = 0, height-1
      do col = 0, width-1
        if (((col .eq. 1 .or. col .eq.3) .and. row .lt. 5) .or. & 
            (col .eq. 2 .and. row .eq. 5))  then
          letter_str(pos:pos) = star
        else
          letter_str(pos:pos) = empt
        end if
        pos = pos + 1
      end do
      letter_str(pos:pos) = new_line('a')
      pos = pos + 1
    end do
  end function

  function make_marquee(word, height, width) result(marquee)
    character(len=*), intent(in)                      :: word
    integer, intent(in)                               :: height, width
    character(len=height*(len_trim(word)*width +1))   :: marquee
    character(len=height*(width+1))                   :: letter
    integer                                           :: row, col, pos, i, row_start, row_end
    integer                                           :: lenword

    lenword = len_trim(word)
    pos =1

    do row =0, height-1
      do i=1, lenword
        select case(word(i:i))
          case("R"); letter = make_r(height,width)
          case("E"); letter = make_e(height,width)
          case("D"); letter = make_d(height,width)
          case("B"); letter = make_b(height,width)
          case("L"); letter = make_l(height,width)
          case("U"); letter = make_u(height,width)
        end select

        row_start = row * (width+1) +1
        row_end = row * (width +1) + width
        marquee(pos:pos+width-1) = letter(row_start:row_end)
        pos = pos + width
      end do
      marquee(pos:pos) = new_line('a')
      pos = pos + 1
    end do
  end function
  
  function validate(word) result(tf)
    character(len=*),intent(in)   :: word
    logical                       :: tf
    integer                       :: i,j, wordlen, tokens

    tokens = 0

    wordlen = len_trim(word)

    do i=1 , wordlen
      if (exists(word(i:i),dictionary)) then
        tokens = tokens + 1
      else 
        tokens = tokens - 1
      end if
    end do
    write (*,*) wordlen, tokens
    if (tokens .eq. wordlen) then
      tf = .true.
    else 
      tf = .false.
    end if
  end function

  function user_input() result(word)
    character (len=:), allocatable    :: word
    character (len=256)               :: buffer
    integer                           :: ierror

    do while (.true.)
      write (*,'(A)', advance='no') "Enter your word, 'End' to terminate: "
      read (*,'(A)', iostat = ierror) buffer

      if (ierror .eq. 0) then
        word = trim(buffer)
        if (word == 'END') then
          exit
        else 
          if (validate(word)) then
            write (*,*) "Word is valid"
          else 
            write (*,'(A,A,A)') "Word: '", word, "' is invalid"
          end if        
        end if
      end if
    end do
  end function

  function upper(str) result(uppstr)
    character(len=*),intent(in)   :: str 
    character(len=:), allocatable :: uppstr
    integer                       :: strlen, i, pos

    strlen = len_trim(str)
    uppstr = to_upper(str)

    write (*,*) str, uppstr

  end function
end module matrixsign


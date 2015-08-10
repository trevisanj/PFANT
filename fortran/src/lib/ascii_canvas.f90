!> Allows printing with coordinates on a "canvas"
!>
!> ASCII image is mounted, then printed on-screen at once

module ascii_canvas
  use logging
  use misc
  implicit none

  integer, parameter, private :: MAX_DIM=100

  type canvas
    character(len=MAX_DIM) :: rows(MAX_DIM)
    integer :: nr !< number of rows
    integer :: nc !< number of columns
  contains
    procedure :: init => canvas_init
    procedure :: paint => canvas_paint
    procedure :: print => canvas_print
  end type

contains

  !> Initializes with given number of rows and columns

  subroutine canvas_init(this, nr, nc)
    class(canvas) :: this
    integer, intent(in) :: nr, nc

    character(len=MAX_DIM) :: s_temp
    integer :: i

    call assert_le(nr, MAX_DIM, 'canvas_init()', 'nr', 'MAX_DIM')
    call assert_le(nc, MAX_DIM, 'canvas_init()', 'nc', 'MAX_DIM')

    s_temp = repeat(' ', MAX_DIM)

    this%nr = nr
    this%nc = nc
    do i = 1,nr
      this%rows(i) = s_temp
    end do
  end

  !> "paints" string in canvas

  subroutine canvas_paint(this, lin, col, str)
    class(canvas) :: this
    integer :: lin, col
    character(len=*), intent(in) :: str

    !integer :: len_eff, i


    call assert_le(lin, this%nr, 'canvas_paint()', 'lin', 'this%nr')

    call replace_part(this%rows(lin))

    !len_eff = min(len(str), this%nc-col+1)

    !write(*,*) 'len_eff=',len_eff


    !if (len_eff .gt. 0) then
    !  this%rows(lin)(col:col+len_eff-1) = str(1:len_eff)
      !do i = 1, len_eff
      !  this%rows(i+col-1, lin) = str(i:i)
      !end do

      !this%rows(col:col+len_eff-1, lin) = str(1:len_eff)
      !this%rows(lin,col:col+len_eff-1) = str(1:len_eff)
    !end if

    !write(*,*) '#',this%rows(col:col+len_eff-1, lin),'#'
    !write(*,*) '!',str(1:len_eff),'!'

    !write(*,*) '$',this%rows,'$'

  contains
    !> Does the "painting"

    subroutine replace_part(row)
      character(len=*) :: row !< row of the canvas
      integer :: len_eff

      len_eff = min(len(str), this%nc-col+1)


      if (len_eff .gt. 0) then
        row(col:col+len_eff-1) = str(1:len_eff)
      !do i = 1, len_eff
      !  this%rows(i+col-1, lin) = str(i:i)
      !end do

      !this%rows(col:col+len_eff-1, lin) = str(1:len_eff)
      !this%rows(lin,col:col+len_eff-1) = str(1:len_eff)
    end if

    !write(*,*) '#',this%rows(col:col+len_eff-1, lin),'#'
    !write(*,*) '!',str(1:len_eff),'!'

    !write(*,*) '$',this%rows,'$'
  end
  end

  !> prints to given unit

  subroutine canvas_print(this, flag_frame, level)
    class(canvas) :: this
    !> Whether to print a "frame" around (optional). Default: .false.
    logical, intent(in), optional :: flag_frame
    !> Logging level (optional). Default: LOGGING_INFO
    integer, intent(in), optional :: level

    logical :: flag_frame_
    character(len=:), allocatable :: dashes ! for the frame
    character(len=MAX_DIM) :: s_temp
    integer :: level_, i

    flag_frame_ = .false.
    if (present(flag_frame)) flag_frame_ = flag_frame

    level_ = LOGGING_INFO
    if (present(level)) level_ = level


    if (flag_frame_) then
      dashes = repeat('-', this%nc)
      call log_any('+'//dashes//'+', level_, .false.)
      do i = 1, this%nr
        s_temp = this%rows(i)
        call log_any('|'//s_temp(1:this%nc)//'|', level_, .false.)
      end do
      call log_any('+'//dashes//'+', level_, .false.)
    else
      do i = 1, this%nr
        s_temp = this%rows(i)
        call log_any(s_temp(1:this%nc), level_, .false.)
      end do
    end if
  end
end

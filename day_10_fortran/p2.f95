program outputdata   

  integer :: FID = 1
  Character*4 :: op
  integer :: op_value
  integer :: IERR = 0
  Character*4 :: noop = 'noop'
  Character*4 :: addx = 'addx'
  integer :: num_lines = 0
  Character*4, allocatable :: line_types(:)
  integer, allocatable :: op_values(:)
  integer :: i
  integer :: j
  integer :: num_cycles = 0
  integer, allocatable :: dX(:)
  integer, allocatable :: X(:)
  integer :: screen_width = 40
  integer :: CRT = 0


  open (unit=FID, file='input.txt')

  !======= File reading is done in three passes =======
  ! First read counts the number of lines
  do
    read(FID, *, iostat=IERR) op
    if (IERR /= 0) then
      exit
    end if
    num_lines = num_lines + 1
    ! OPERATIONS
  end do

  ! Second read gets line types
  allocate(line_types(num_lines))
  rewind(FID)
  do i=1, num_lines
    read(FID, *) op
    line_types(i) = op
  end do

  ! Third read gets the operation types
  allocate(op_values(num_lines))
  rewind(FID)
  do i=1, num_lines
    if (line_types(i) == noop) then
      read(FID, *) op
    else if (line_types(i) == addx) then
      read(FID, *) op, op_value
      op_values(i) = op_value
    end if
  end do
  close(unit=FID)

  !======= Data is now in Fortran arrays =======
  ! calculate number of cycles
  do i=1, num_lines
    if (line_types(i) == noop) then
      num_cycles = num_cycles + 1
    else if (line_types(i) == addx) then
      num_cycles = num_cycles + 2
    end if
  end do

  ! Assign register values by cycle
  allocate(dX(num_cycles))
  j = 1
  do i=1, num_lines
    if (line_types(i) == noop) then
      dX(j) = 0
      j = j+1
    else if (line_types(i) == addx) then
      dX(j) = 0
      dX(j+1) = op_values(i)
      j = j+2
    end if
  end do

  allocate(X(num_cycles+1))
  X(1) = 1
  do i=1, num_cycles
    X(i+1) = X(i) + dX(i)
  end do

  do i=1, num_cycles+1
    CRT = modulo(i-1, screen_width)
    if (modulo(CRT, screen_width) == 0) then
      print *, ''
    end if
    if (my_abs(X(i) - CRT) <= 1) then
      write(*, '(A)', advance='no') '#'
    else
      write(*, '(A)', advance='no') '.'
    end if
  end do
  print *, ''
   
end program outputdata

integer function my_abs(x)
  integer, intent(in) :: x
  if (x < 0) then
    my_abs = -x
  else
    my_abs = x
  end if
end function my_abs
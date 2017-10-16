program bcast

include 'mpif.h'

INTEGER  rank, numproc, ierror,root,tag
INTEGER*8  msg_size,i
INTEGER,dimension(:), ALLOCATABLE :: s_request
INTEGER,dimension(:), ALLOCATABLE :: s_status
INTEGER:: r_request
INTEGER:: r_status
integer :: length=0
CHARACTER(255) :: arg,f1,f
Character(4):: f2
DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE :: msg
DOUBLE PRECISION:: t_start,t_end,t_bcast,t_isend
!REAL*4, DIMENSION(:),ALLOCATABLE :: msg

!====================================================!

call MPI_INIT(ierror)
call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)
call MPI_COMM_SIZE(MPI_COMM_WORLD,numproc,ierror)


!print *,"Hello I am Processor ",rank," of ",numproc

CALL getarg(1, arg)


root=0
read (arg,'(I32)') msg_size
!print*,msg_size

allocate(msg(msg_size),STAT=ierror)
 if ( ierror /= 0 ) print*,'Error allocating msg'
 
 if(rank.eq.root) then
   msg(1:msg_size)=1
   !print*,msg(:)
 endif

!=====================Bcast==============================!
 
call mpi_barrier(MPI_COMM_WORLD,ierror) 
t_start= mpi_wtime() 
call MPI_BCAST( msg, msg_size, MPI_DOUBLE_PRECISION, root, MPI_COMM_WORLD, ierror )
!call mpi_barrier(MPI_COMM_WORLD,ierror)
t_end=mpi_wtime()

t_bcast=t_end-t_start


!=====================Isend==============================!

tag=100

call mpi_barrier(MPI_COMM_WORLD,ierror) 
t_start= mpi_wtime() 
if (rank.eq.root) then
 allocate(s_request(numproc-1))
 allocate(s_status(numproc-1))
 do i=1,numproc-1
  call MPI_Isend(msg, msg_size,MPI_DOUBLE_PRECISION ,i, tag, MPI_COMM_WORLD, s_request(i),ierror )
 end do
else
 call MPI_Irecv(msg, msg_size,MPI_DOUBLE_PRECISION ,root, tag, MPI_COMM_WORLD, r_request,ierror ) 
end if


!Ensure transfere completion
if (rank.eq.root) then
 call MPI_waitall(numproc-1,s_request,s_status,ierror)
else 
 call MPI_wait(r_request,r_status,ierror)
end if

!call mpi_barrier(MPI_COMM_WORLD,ierror) 
t_end= mpi_wtime()
t_isend=t_end-t_start
 
!if(rank.eq.5) then
! print*,"tbcast=",t_bcast,"tisend=",t_isend
!end if
!==========write time measurement in output file=========!
 if (rank.eq.root) then
 write(f, '(A, I10,A)') 'file', numproc,'.data'
 
 f = sweep_blanks(f)
 !print*,f
 
 
 open(unit = 110, file = f)
  do  
       read(110,*,end=10)
       length= length + 1 
   end do
   10   close(110)
   
  open (unit= 120,file = f)
   do i = 1,length
       read(120,*)
   end do

  !write at the end of file

   write(120,*) msg_size/1000,numproc,t_bcast/1000000,t_isend/1000000
   close(120)
 end if

!========================================================!
if (rank.eq.root) then
 deallocate(msg,s_request,s_status);
end if
!========================================================!

call MPI_FINALIZE(ierror)


!===================================================!
contains 
  character(30) function sweep_blanks(in_str)
    character(*), intent(in) :: in_str
    character(30) :: out_str
    character :: ch
    integer :: j

    out_str = " "
    do j=1, len_trim(in_str)
      ! get j-th char
      ch = in_str(j:j)
      if (ch .ne. " ") then
        out_str = trim(out_str) // ch
      endif
      sweep_blanks = out_str 
    end do
  end function sweep_blanks
end program bcast

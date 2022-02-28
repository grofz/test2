!
! Example of "ping-pong" game 
!
  program ping
    use mpi
    implicit none

    integer, parameter :: TAG_NORMAL=100, TAG_END=101
    integer :: rank, np, status(MPI_STATUS_SIZE), ierr
    integer :: nsend, i
    integer, allocatable :: nsendall(:)
    real    :: msg_threshold, msg_recv

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, np, ierr)

    if (np <= 1) then
      print *, 'Need at least two processes for the play'
      call MPI_ABORT(MPI_COMM_WORLD, 0, ierr)
    endif

    ! Initialize the send counter and prompt user for threshold to end
    nsend = 0
    if (rank == 0) then
      allocate(nsendall(np))
      write(*,'(a)',advance='no') 'What is the threshold ? '
      read(*,*) msg_threshold
    endif
    call MPI_BCAST(msg_threshold, 1, MPI_REAL, 0, MPI_COMM_WORLD, ierr)

    ! Root starts by sending the first message
    if (rank == 0) then
      msg_recv = 0.0
      call send(msg_recv, rank, np)
      nsend = nsend + 1
    endif

    ! Processes wait and process messages until told to exit
    MAIN: do
      call MPI_RECV(msg_recv, 1, MPI_REAL, MPI_ANY_SOURCE, MPI_ANY_TAG, &
      &             MPI_COMM_WORLD, status, ierr)

      if (status(MPI_TAG) == TAG_END) exit MAIN

      if (msg_recv > msg_threshold) then
        do i=0, np-1
          if (i==rank) cycle
          call MPI_SSEND(msg_recv, 1, MPI_REAL, i, TAG_END, &
          &              MPI_COMM_WORLD, ierr)
        enddo
        exit MAIN
      endif

      call send(msg_recv, rank, np)
      nsend = nsend + 1
    enddo MAIN

    ! Send statistics is gathered by root
    print '("Rank ",i0," has send ",i0," messages")', rank, nsend
    call MPI_GATHER(nsend, 1, MPI_INTEGER, nsendall(1), 1, MPI_INTEGER, &
    &               0, MPI_COMM_WORLD, ierr)

    if (rank == 0) then
      print '(a,20(i0,1x))', 'Send score: ', nsendall
      print '(a,i0)', 'Sum of messages send: ', sum(nsendall)
    endif

    call MPI_FINALIZE(ierr)

  contains

    subroutine send(msg_recv, rank, np)
      real, intent(in) :: msg_recv
      integer, intent(in) :: rank, np
!
! Increment message and send it to a random process
!
      real :: y, msg_send
      integer :: ito

      ! Randomly select where message will be sent, avoid sending to itself
      call random_number(y)
      ito = int(x*(np-1)) ! ito is between 0 and np-2
      if (ito >= rank) ito = ito + 1

      ! Compose the message and send it
      call random_number(y)
      msg_send = msg_recv + y
      print '("Send ",i0," to ",i0," Message ",f10.6)', &
      &   rank, ito, msg_send

      call MPI_SSEND(msg_send, 1, MPI_REAL, ito, TAG_NORMAL, &
      &              MPI_COMM_WORLD, ierr)
    end subroutine send
  
  end program ping

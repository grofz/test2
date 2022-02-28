!
! 1. Process with rank 0 begins by sending the first message assuming 
!    "msg_recv=0".
!
! 2. In an endless loop, all processes wait for a message from any process.
!    There are two possible types of a message:
!    - "normal" message is a real number "msg_recv"
!    - "end-of-game" message
!
!   The type of a message can be defined either by a different tag or by a
!   special value of "msg_recv" (differentiation by tag is preferred).
!
! 3. When a process receives "end-of-game" message it stops waiting for more
!    messages and exits the loop.

! 4. When a process receives "normal" message, but the received value is
!    larger than a some threshold (msg_recv > msg_threshold), it sends
!    "end-of-game" messages to all other processes and exits the loop.
!
! 5. When a process receives "normal" message with received value less than
!    the threshold, then the process:
!
!   - generates a random real number "x" in the range <0; 1) and
!     adds it to the received value "msg_recv"
!     (new message value "msg_send" will be "msg_recv" + "x"); 
!   - generates randomly an integer "j" in the range <0; np-1> in such a way
!     that "j" is not the rank of the actual process;
!   - sends a "normal" message "msg_send" to the process with the rank "j";
!   - prints "msg_send", its rank and the rank of the process where the
!     message has been send to;
!   - increments its counter of sent messages;
!   - resumes waiting for further messages.
!
! 6. After exiting the loop, every process prints its rank and the number of
!    messages it has send.
!
! Optional extensions:
!
! 7. At the beginning, before sending the first message, the root can
!    prompt the user for the threshold value and then broadcast this
!    value to other processes.
!
! 8. Apart from printing the number of messages send, processes send this
!    value to the root (try to use MPI_GATHER). The root than prints this
!    statistics for every process and the total number of send messages.
!
! Here is an example of the output:
! $ mpirun -np 4 ./a2.exe 
!
! What is the threshold ? 5
! Send 0 to 2 Message   0.789020
! Send 2 to 3 Message   1.056683
! Send 3 to 1 Message   1.883816
! Send 1 to 3 Message   2.505021
! Send 3 to 0 Message   3.335455
! Send 0 to 2 Message   4.230506
! Send 2 to 1 Message   4.476061
! Send 1 to 0 Message   4.670770
! Send 0 to 3 Message   5.060951
! Rank 0 has send 3 messages
! Rank 1 has send 2 messages
! Rank 2 has send 2 messages
! Rank 3 has send 2 messages
! Statistics: 3 2 2 2

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
      real :: x, msg_send
      integer :: ito

      ! Randomly select where message will be sent, avoid sending to itself
      call random_number(x)
      ito = int(x*(np-1)) ! ito is between 0 and np-2
      if (ito >= rank) ito = ito + 1

      ! Compose the message and send it
      call random_number(x)
      msg_send = msg_recv + x
      print '("Send ",i0," to ",i0," Message ",f10.6)', &
      &   rank, ito, msg_send

      call MPI_SSEND(msg_send, 1, MPI_REAL, ito, TAG_NORMAL, &
      &              MPI_COMM_WORLD, ierr)
    end subroutine send
  
  end program ping

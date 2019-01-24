program latex2png
    use MPI
    use wrank
    use define
    implicit none
    integer :: i, mpirank, mpisize, ierr
    integer :: maingroup
    integer :: workergroup, workergroupsize, workergrouprank, COMM_WORKERGROUP
    integer :: sendergroup, sendergroupsize, sendergrouprank, COMM_SENDERGROUP
    integer :: inlinegroup, inlinegroupsize, inlinegrouprank, COMM_INLINEGROUP
    integer, dimension(:), allocatable :: inlines, workers, senders

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, mpirank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, mpisize, ierr)

    if(mpisize.lt.(3+count_inline+count_sender+1)) then
        print *, START_ERROR
        call MPI_ABORT(MPI_COMM_WORLD, 1, ierr)
        stop
    end if

    if(allocated(inlines)) deallocate(inlines)
    if(allocated(workers)) deallocate(workers)
    if(allocated(senders)) deallocate(senders)
    allocate(inlines(count_inline + 1))
    allocate(senders(count_sender + 1))
    allocate(workers(mpisize - 3 - count_inline - count_sender + 1))
    inlines(1) = 1
    senders(1) = 2
    workers(1) = 1

    do i = 3, 3 + count_inline
        inlines(i-3+2) = i
    end do
    do i = 3 + count_inline, 3 + count_inline + count_sender
        senders(i-3-count_inline+2) = i
    end do
    do i = 3 + count_inline + count_sender, mpisize
        workers(i-3-count_inline-count_sender+2) = i
    end do

    call MPI_COMM_GROUP(MPI_COMM_WORLD, maingroup, ierr)

    call MPI_GROUP_INCL(maingroup, size(inlines), inlines, inlinegroup, ierr)
    call MPI_COMM_CREATE(MPI_COMM_WORLD, inlinegroup, COMM_INLINEGROUP, ierr)
    call MPI_GROUP_SIZE(inlinegroup, inlinegroupsize, ierr)
    call MPI_GROUP_RANK(inlinegroup, inlinegrouprank, ierr)

    call MPI_GROUP_INCL(maingroup, size(senders), senders, sendergroup, ierr)
    call MPI_COMM_CREATE(MPI_COMM_WORLD, sendergroup, COMM_SENDERGROUP, ierr)
    call MPI_GROUP_SIZE(sendergroup, sendergroupsize, ierr)
    call MPI_GROUP_RANK(sendergroup, sendergrouprank, ierr)

    call MPI_GROUP_INCL(maingroup, size(workers), workers, workergroup, ierr)
    call MPI_COMM_CREATE(MPI_COMM_WORLD, workergroup, COMM_WORKERGROUP, ierr)
    call MPI_GROUP_SIZE(workergroup, workergroupsize, ierr)
    call MPI_GROUP_RANK(workergroup, workergrouprank, ierr)

    if(mpirank.eq.0) then
        call rank0()
    else if(mpirank.eq.1) then
        call rank1(COMM_WORKERGROUP, COMM_INLINEGROUP)
    else if(mpirank.eq.2) then
        call rank2(COMM_SENDERGROUP)
    else if(any(inlines.eq.mpirank)) then
         call rank_inlines(COMM_INLINEGROUP)
    else if(any(workers.eq.mpirank)) then
         call rank_workers(COMM_WORKERGROUP)
    else if(any(senders.eq.mpirank)) then
         call rank_senders(COMM_SENDERGROUP)
    end if

    call MPI_GROUP_FREE(inlinegroup, ierr)
    call MPI_GROUP_FREE(sendergroup, ierr)
    call MPI_GROUP_FREE(workergroup, ierr)
    call MPI_FINALIZE(ierr)
end program latex2png
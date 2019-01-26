module wrank
    use MPI
    use wsleep
    implicit none

    integer(4) :: MPIBUFFERSIZE, CHARBUFFERSIZE, MAXLENGTH
    parameter (MPIBUFFERSIZE = 100000 * MPI_BSEND_OVERHEAD)
    parameter (CHARBUFFERSIZE = 1024)
    parameter (MAXLENGTH = 4096*12)

contains
    subroutine rank0()
        use tgAPI
        use wfile, only : getoffset => json_rank0
        character(len=:), allocatable :: str, str1
        character(len=CHARBUFFERSIZE) :: charbuffer
        integer(4) :: ierr, res, offset
        integer(1) :: buffer(MPIBUFFERSIZE)
        call MPI_BUFFER_ATTACH(buffer, MPIBUFFERSIZE, ierr)
        offset = 0
        do
            if(offset.le.0) then
                call getUpdates(str, timeout=60, status=res)
            else
                call getUpdates(str, offset=offset, timeout=60, status=res)
            end if
            if(res.eq.0.and.len(trim(str)).ne.0) then
                str = adjustl(trim(str))
                call getoffset(str, offset)
                offset = offset + 1
                call MPI_BSEND(1, 1, MPI_INTEGER, 1, 599, MPI_COMM_WORLD, ierr)
                do while (len(str) .ne. 0)
                    charbuffer = str(1:MIN(CHARBUFFERSIZE, len(str)))
                    str1 = str(CHARBUFFERSIZE+1:len(str))
                    str = str1
                    call MPI_BSEND(len(charbuffer), 1, MPI_INTEGER, 1, 600, MPI_COMM_WORLD, ierr)
                    call MPI_BSEND(charbuffer, len(charbuffer), MPI_CHARACTER, 1, 601, MPI_COMM_WORLD, ierr)
                end do
                call MPI_BSEND(0, 1, MPI_INTEGER, 1, 600, MPI_COMM_WORLD, ierr)
            else
                call curl_err(res, str)
                print *, "|", str, "|"
            end if
        end do
    end subroutine

    subroutine rank1(COMM_WORKERGROUP, COMM_INLINEGROUP)
        use wfile, only : parse => json_rank1
        use define, only : workermode, inlinemode
        integer, intent(in) :: COMM_WORKERGROUP, COMM_INLINEGROUP
        integer :: length
        integer :: status(MPI_STATUS_SIZE)
        character(len=:), allocatable  :: strinp
        character(len=CHARBUFFERSIZE)  :: charbuffer = REPEAT(" ", CHARBUFFERSIZE)
        integer(4),        allocatable :: userid(:), mode(:), command(:)
        character(len=40), allocatable :: supdateid(:)
        integer(4)                     :: lenans
        character(len=MAXLENGTH)       :: text
        character(len=MAXLENGTH), allocatable :: atext(:)
        !mpi_message
        character(len=MAXLENGTH+64) :: mpimess
        character(len=12)           :: convert
        character(len=40)           :: convert40
        integer(4)                  :: dest, commworkersize, comminlinesize, ierr, i
        integer(1)                  :: buffer(MPIBUFFERSIZE)
        real                        :: randreal
        logical                     :: canget
        call MPI_BUFFER_ATTACH(buffer, MPIBUFFERSIZE, ierr)
        call MPI_COMM_SIZE(COMM_WORKERGROUP, commworkersize, ierr)
        call MPI_COMM_SIZE(COMM_INLINEGROUP, comminlinesize, ierr)
        do
            length = 1
            strinp = ""
            text = REPEAT(" ", MAXLENGTH)
            canget = .FALSE.
            do while (canget.neqv..TRUE.)
                call MPI_IPROBE(0, 599, MPI_COMM_WORLD, canget, status, ierr)
                if (canget.eqv..FALSE.) then
                    call msleep(10)
                end if
            end do
            call MPI_RECV(length, 1, MPI_INTEGER, 0, 599, MPI_COMM_WORLD, status, ierr)

            do while (length.ne.0)
                charbuffer = REPEAT(" ", CHARBUFFERSIZE)
                call MPI_RECV(length, 1, MPI_INTEGER, 0, 600, MPI_COMM_WORLD, status, ierr)
                if(length.ne.0) then
                    call MPI_RECV(charbuffer, length, MPI_CHARACTER, 0, 601, MPI_COMM_WORLD, status, ierr)
                    strinp = strinp // trim(charbuffer)
                end if
            end do
            call parse(strinp, atext, userid, supdateid, mode, command)
            lenans = size(userid)
            if(lenans.gt.0) print *, "rank1: ", strinp
            do i = 1, lenans
                print *, supdateid(i), userid(i), command(i), trim(atext(i))
                text = REPEAT(" ", MAXLENGTH)
                text = atext(i)
                mpimess = REPEAT(" ", MAXLENGTH+36)
                mpimess( 1:40) = supdateid(i)
                write (convert, "(I12)") userid(i)
                mpimess(41:52) = convert
                write (convert, "(I12)") command(i)
                mpimess(53:64) = convert
                mpimess(65:MAXLENGTH+64) = text
                if(mode(i).eq.workermode) then
                    call random_number(randreal)
                    dest = 1 + floor(randreal*(commworkersize-1))
                    call MPI_BSEND(mpimess, MAXLENGTH+64, MPI_CHARACTER, dest, 700, COMM_WORKERGROUP, ierr)
                else if(mode(i).eq.inlinemode) then
                    call random_number(randreal)
                    dest = 1 + floor(randreal*(comminlinesize-1))
                    call MPI_BSEND(mpimess, MAXLENGTH+64, MPI_CHARACTER, dest, 720, COMM_INLINEGROUP, ierr)
                end if
            end do
        end do
    end subroutine

    subroutine rank2(COMM_SENDERGROUP)
        integer, intent(in)         :: COMM_SENDERGROUP
        character(len=MAXLENGTH+64) :: mpimess
        integer(4)                  :: status(MPI_STATUS_SIZE), dest, commsendersize, ierr
        logical                     :: canget
        integer(1)                  :: buffer(MPIBUFFERSIZE)
        real                        :: randreal
        call MPI_BUFFER_ATTACH(buffer, MPIBUFFERSIZE, ierr)
        call MPI_COMM_SIZE(COMM_SENDERGROUP, commsendersize, ierr)
        do
            canget = .FALSE.
            do while (canget.neqv..TRUE.)
                call MPI_IPROBE(MPI_ANY_SOURCE, 800, MPI_COMM_WORLD, canget, status, ierr)
                if (canget.eqv..FALSE.) then
                    call msleep(4)
                end if
            end do
            call MPI_RECV(mpimess, MAXLENGTH+64, MPI_CHAR, MPI_ANY_SOURCE, 800, MPI_COMM_WORLD, status, ierr)
            call random_number(randreal)
            dest = 1 + floor(randreal*(commsendersize-1))
            call MPI_BSEND(mpimess, MAXLENGTH+64, MPI_CHARACTER, dest, 900, COMM_SENDERGROUP, ierr)
        end do
    end subroutine

    subroutine rank_senders(COMM_SENDERGROUP)
        use wfile, only : send
        integer(4), intent(in)        :: COMM_SENDERGROUP
        integer(4)                    :: userid, resulttype, error, res
        character(len=:), allocatable :: text
        character(len=12)             :: convert
        character(len=40)             :: convert40
        character(len=MAXLENGTH)      :: charbuffer
        !mpi
        character(len=MAXLENGTH+64) :: mpimess
        integer(4)                  :: status(MPI_STATUS_SIZE), ierr
        logical                     :: canget
        integer(1)                  :: buffer(MPIBUFFERSIZE)
        call MPI_BUFFER_ATTACH(buffer, MPIBUFFERSIZE, ierr)
        do
            canget = .FALSE.
            do while (canget.neqv..TRUE.)
                call MPI_IPROBE(0, 900, COMM_SENDERGROUP, canget, status, ierr)
                if (canget.eqv..FALSE.) then
                    call msleep(100)
                end if
            end do
            call MPI_RECV(mpimess, MAXLENGTH+64, MPI_CHAR, 0, 900, COMM_SENDERGROUP, status, ierr)
            read(mpimess( 1:40), '(I40)') userid
            read(mpimess(41:52), '(I12)') resulttype
            read(mpimess(53:64), '(I12)') error
            read(mpimess(65:  ), '(A)')   charbuffer
            text = trim(charbuffer)
            call send(userid, resulttype, text, res)
            error = error + 1
            if(res.ne.0.and.error.le.10) then
                mpimess = REPEAT(' ', MAXLENGTH+64)
                write (convert40, '(I40)') userid
                mpimess(1:40) = convert40
                write (convert, '(I12)') resulttype
                mpimess(41:52) = convert
                write (convert, '(I12)') error
                mpimess(53:64) = convert
                mpimess(65:MAXLENGTH+64) = charbuffer
                call MPI_BSEND(mpimess, MAXLENGTH+64, MPI_CHARACTER, 2, 800, MPI_COMM_WORLD, ierr)
            end if
        end do
    end subroutine

    subroutine rank_workers(COMM_WORKERGROUP)
        use actions, only : generateresponse
        integer(4), intent(in)        :: COMM_WORKERGROUP
        integer(4)                    :: ierr, status(MPI_STATUS_SIZE), request
        character(len=MAXLENGTH+64)   :: mpimess
        character(len=MAXLENGTH)      :: charbuffer
        character(len=:), allocatable :: text, key, error
        character(len=12)             :: convert
        character(len=40)             :: convert40
        integer(4)                    :: updateid, userid, command, resulttype
        logical                       :: canget
        integer(1)                    :: buffer(MPIBUFFERSIZE)
        call MPI_BUFFER_ATTACH(buffer, MPIBUFFERSIZE, ierr)
        do
            canget = .FALSE.
            do while (canget.neqv..TRUE.)
                call MPI_IPROBE(0, 700, COMM_WORKERGROUP, canget, status, ierr)
                if (canget.eqv..FALSE.) then
                    call msleep(10)
                end if
            end do
            call MPI_RECV(mpimess, MAXLENGTH+64, MPI_CHAR, 0, 700, COMM_WORKERGROUP, status, ierr)
            read(mpimess( 1:40), '(I40)') updateid
            read(mpimess(41:52), '(I12)') userid
            read(mpimess(53:64), '(I12)') command
            read(mpimess(65:  ), '(A)')   charbuffer
            text = trim(charbuffer)
            call generateresponse(userid, updateid, command, text, resulttype, key, error)
            print *, resulttype, len(error), '"', key, ':', error, '"'
            mpimess = REPEAT(' ', MAXLENGTH+64)
            write (convert40, '(I40)') userid
            mpimess( 1:40) = convert40
            write (convert, '(I12)') resulttype
            mpimess(41:52) = convert
            write (convert, '(I12)') 0
            mpimess(53:64) = convert
            mpimess(65:MAXLENGTH+64) = key
            call MPI_BSEND(mpimess, MAXLENGTH+64, MPI_CHARACTER, 2, 800, MPI_COMM_WORLD, ierr)
            if(len(error).ne.0) then
                mpimess = REPEAT(' ', MAXLENGTH+64)
                write (convert40, '(I40)') userid
                mpimess( 1:40) = convert40
                write (convert, '(I12)') 0
                mpimess(41:52) = convert
                write (convert, '(I12)') 0
                mpimess(53:64) = convert
                mpimess(65:MAXLENGTH+64) = error
                call MPI_BSEND(mpimess, MAXLENGTH+64, MPI_CHARACTER, 2, 800, MPI_COMM_WORLD, ierr)
            end if
        end do
    end subroutine

    subroutine rank_inlines(COMM_INLINEGROUP)
        use actions, only : inlineresponse
        integer(4), intent(in)        :: COMM_INLINEGROUP
        integer(4)                    :: ierr, status(MPI_STATUS_SIZE), request
        character(len=MAXLENGTH+64)   :: mpimess
        character(len=MAXLENGTH)      :: charbuffer
        character(len=:), allocatable :: text, key, error, supdateid
        integer(4)                    :: userid, command, resulttype
        logical                       :: canget
        integer(1)                    :: buffer(MPIBUFFERSIZE)
        call MPI_BUFFER_ATTACH(buffer, MPIBUFFERSIZE, ierr)
        do
            canget = .FALSE.
            do while (canget.neqv..TRUE.)
                call MPI_IPROBE(0, 720, COMM_INLINEGROUP, canget, status, ierr)
                if (canget.eqv..FALSE.) then
                    call msleep(100)
                end if
            end do
            print *, "TEST"
            call MPI_RECV(mpimess, MAXLENGTH+64, MPI_CHAR, 0, 720, COMM_INLINEGROUP, status, ierr)
            supdateid = mpimess( 1:40)
            read(mpimess(41:52), '(I12)') userid
            read(mpimess(53:64), '(I12)') command
            read(mpimess(65:  ), '(A)')   charbuffer
            text = trim(charbuffer)
            call inlineresponse(userid, supdateid, command, text)
        end do
    end subroutine
end module
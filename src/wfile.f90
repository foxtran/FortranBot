module wfile
    implicit none
    integer(4) :: MAXLENGTH
    parameter (MAXLENGTH = 4096*12)
contains
    subroutine getUserIDbyIP(IP, IPchars)
        integer(kind=2),  dimension(4), intent(in)  :: IP
        character(len=:), allocatable,  intent(out) :: IPchars
        character(len=4)                            :: tmpchar
        integer(4)                                  :: i
        IPchars = ''
        do i = 1, 4
            write(tmpchar, '(I4)') IP(i)
            IPchars = trim(IPchars) // '.' // adjustl(tmpchar)
        end do
        IPchars = IPchars(2:)
    end subroutine

    subroutine getUserData(ID, MESSID)
        implicit none
        character(len=*) :: ID,        MESSID
        logical          :: file_exist
        integer          :: error,     idmess
        character(len=:), allocatable :: sID
        read (MESSID, '(I10)') idmess
        sID = adjustl(trim(ID))
        open(unit=idmess, file='./users/' // sID // '.history', action='WRITE', position='APPEND')
        write(idmess, '(a)') trim(MESSID) // ': ' // 'GET USER DATA'
        close(idmess)
    end subroutine

    subroutine checkFiles(ID, MESSID)
        character(len=*) :: ID,        MESSID
        logical          :: file_exist
        integer          :: error,     idmess
        character(len=:), allocatable :: sID
        read (MESSID, '(I10)') idmess
        sID = adjustl(trim(ID))
        call execute_command_line('[[ ! -e users ]] && mkdir users', cmdstat=error)
        if(error .ne. 0) then
            print *, 'CANNOT CREATE USERS DIRECTORY'
            stop
        end if
        inquire(file='./users/' // sID // '.settings', exist=file_exist)
        if(file_exist .neqv. .true.) then
            open(unit=idmess, file='./users/' // sID // '.settings', action='WRITE')
            close(unit=idmess)
        end if
        inquire(file='./users/' // sID // '.packages', exist=file_exist)
        if(file_exist .neqv. .true.) then
            open(unit=idmess, file='./users/' // sID // '.packages', action='WRITE')
            close(unit=idmess)
        end if
        open(unit=idmess, file='./users/' // sID // '.history', action='WRITE', position='APPEND')
        close(idmess)
    end subroutine

    subroutine writeLog(ID, MESSID, MESS)
        implicit none
        character(len=*)              :: ID, MESSID, MESS
        character(len=:), allocatable :: filenm, sID
        integer                       :: idmess
        read (MESSID, '(I10)') idmess
        sID = adjustl(trim(ID))
        filenm = './users/' // sID // '.history'
        open(unit=idmess, file=filenm, position='append', action='write')
        write(idmess, '(A)') MESSID // ': ' // MESS
        close(idmess)
    end subroutine

    subroutine readfile(filename, result)
        USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_EOR
        implicit none
        character(len=*),  intent(in)  :: filename
        character(len=:),  allocatable :: result
        character(len=256)             :: buffer
        integer                        :: stat
        result = ''
        open(unit=7, file=trim(filename), action='READ')
        do
            read(7, '(a)', iostat=stat) buffer
            if(stat .eq. -1) then
                result = trim(result)
                close(unit=7)
                return
            end if
            result = result // trim(buffer) // NEW_LINE('')
        end do
        result = trim(result)
        close(unit=7)
    end subroutine

    subroutine LatexDefault(result)
        implicit none
        character(len=:), allocatable :: result
        call readfile('./default.tex', result)
    end subroutine

    subroutine getPackages(ID, result, isMessage)
        implicit none
        character(len=*), intent(in)  :: ID
        character(len=:), allocatable :: result, filenm
        logical, optional             :: isMessage
        filenm = './users/' // adjustl(trim(ID)) // '.packages'
        call readfile(filenm, result)
        if(present(isMessage).and.len(result).eq.0) then
            result = 'No usage packages'
        end if
    end subroutine

    subroutine reset(ID, MESSID)
        implicit none
        character(len=*), intent(in)  :: ID, MESSID
        integer                       :: error, idmess
        character(len=:), allocatable :: sID
        read (MESSID, '(I10)') idmess
        sID = adjustl(trim(ID))
        call writeLog(ID, MESSID, 'RESET')
        open(unit=idmess, file='./users/' // sID // '.settings', status='unknown', iostat=error)
        if(error .eq. 0) then
             close(unit=idmess, status='delete')
        end if
        open(unit=idmess, file='./users/' // sID // '.settings', status='new')
        close(unit=idmess)
        open(unit=idmess, file='./users/' // sID // '.packages', status='unknown', iostat=error)
        if(error .eq. 0) then
             close(unit=idmess, status='delete')
        end if
        open(unit=idmess, file='./users/' // sID // '.packages', status='new')
        close(unit=idmess)
    end subroutine

    subroutine appendHeadline(ID, MESSID, text, package)
        character(len=*), intent(in)  :: ID, MESSID, package, text
        integer                       :: idmess
        call writeLog(ID, MESSID, 'APPEND HEADLINE: ' // text // package)
        read (MESSID, '(I10)') idmess
        open(unit=idmess, file='./users/' // adjustl(trim(ID)) // '.packages', status='old', position='append', action='write')
        write(idmess, '(A)') text // package
        close(idmess)
    end subroutine

    subroutine setHeadlines(ID, MESSID, text)
        character(len=*), intent(in)  :: ID, MESSID, text
        integer                       :: idmess
        call writeLog(ID, MESSID, 'SET HEADLINE: ' // text)
        read (MESSID, '(I10)') idmess
        open(unit=idmess, file='./users/' // adjustl(trim(ID)) // '.packages', status='unknown', action='write')
        write(idmess, '(A)') text
        close(idmess)
    end subroutine

    subroutine saveLaTeX(fullfile, filenm)
        use hash
        use strings
        implicit none
        character(len=*),              intent(in)  :: fullfile
        character(len=:), allocatable, intent(out) :: filenm
        integer                                    :: error
        call execute_command_line('[[ ! -e pics ]] && mkdir pics', cmdstat=error)
        if(error .ne. 0) then
            print *, 'CANNOT CREATE PICS DIRECTORY'
            stop
        end if
        filenm = './pics/' // sha256sum(fullfile)
        open(unit=9, file=filenm // '.tex', action='WRITE', encoding='utf-8')
        write(9, '(A)') ASCIItoUTF8(fullfile)
        close(9)
    end subroutine

    subroutine generateLaTeX(filenm, strerr)
        implicit none
        character(len=*),              intent(in)  :: filenm
        character(len=:), allocatable, intent(out) :: strerr
        integer                                    :: error
        logical                                    :: file_exist
        call execute_command_line('pdflatex -output-format=dvi -no-shell-escape -interaction=batchmode -output-directory pics ' // filenm // '.tex &>/dev/null', cmdstat=error)
        if(error .ne. 0) then
            strerr = 'CANNOT CREATE DVI'
            return
        end if
        inquire(file=filenm // '.dvi', exist=file_exist)
        if(file_exist .neqv. .true.) then
            strerr = 'ERROR IN LATEX'
            return
        end if
        call execute_command_line('dvipng -D 1200 ' // filenm // '.dvi -o ' // filenm // '.png &>/dev/null', cmdstat=error)
        if(error .ne. 0) then
            strerr = 'CANNOT CREATE PIC'
            return
        end if
        strerr = 'OK'
    end subroutine

    subroutine savePDF(link, save, strerr)
        implicit none
        character(len=*),              intent(in)  :: link, save
        character(len=:), allocatable, intent(out) :: strerr
        integer                                    :: error
        call execute_command_line('[[ ! -e pdfs ]] && mkdir pdfs', cmdstat=error)
        if(error .ne. 0) then
            print *, 'CANNOT CREATE PDFS DIRECTORY'
            stop
        end if
        !call downloadFile(link, save, strerr)
    end subroutine

    subroutine generatePDF(filenm, strerr)
        implicit none
        character(len=*),              intent(in)  :: filenm
        character(len=:), allocatable, intent(out) :: strerr
        integer                                    :: error
        logical                                    :: file_exist
        call execute_command_line('pdflatex -interaction=batchmode -output-directory pdfs ' // filenm // '.tex &>/dev/null', cmdstat=error)
        if(error .ne. 0) then
            strerr = 'CANNOT CREATE PDF'
            return
        end if
        inquire(file=filenm // '.pdf', exist=file_exist)
        if(file_exist .neqv. .true.) then
            strerr = 'ERROR IN LATEX'
            return
        end if
        strerr = 'OK'
    end subroutine

    subroutine json_rank0(strinp, offset)
        use fson
        use fson_value_m, only: fson_value_count, fson_value_get
        character(len=*), intent(in)        :: strinp
        integer(4),       intent(out)       :: offset
        integer(4), allocatable             :: updateid(:)
        type(fson_value), pointer           :: json, result, item
        integer(4)                          :: i, lenresult
        json => fson_parse(str=strinp)
        if(fson_check(json, 'result')) then
            call fson_get(json, 'result', result)
            lenresult = fson_value_count(result)
            if(allocated(updateid)) then
                deallocate(updateid)
            end if
            allocate(updateid(lenresult))
            do i = 1, lenresult
                item => fson_value_get(result, i)
                call fson_get(item, 'update_id', updateid(i))
            end do
            offset = maxval(updateid)
        else
            offset = 0
        end if
        call fson_destroy(json)
    end subroutine

    subroutine json_rank1(strinp, atext, userid, supdateid, mode, command)
        use fson
        use fson_value_m, only: fson_value_count, fson_value_get
        use define, only: workermode, inlinemode
        character(len=*),        intent(in)         :: strinp
        integer(4), allocatable, intent(out)        :: userid(:), mode(:), command(:)
        character(len=40), allocatable, intent(out) :: supdateid(:)
        integer(4), allocatable                     :: updateid(:)
        type(fson_value), pointer                   :: json, result, message, from, item
        character(len=MAXLENGTH)                    :: text
        character(len=MAXLENGTH), allocatable       :: atext(:)
        character(len=:), allocatable               :: tmptext
        integer(4)                                  :: lenresult, i, messtype
        messtype = -1
        json => fson_parse(str=strinp)
        if(fson_check(json, 'result')) then
            call fson_get(json, 'result', result)
            lenresult = fson_value_count(result)
            if(allocated(updateid))  deallocate(updateid)
            if(allocated(supdateid)) deallocate(supdateid)
            if(allocated(userid))    deallocate(userid)
            if(allocated(mode))      deallocate(mode)
            if(allocated(command))   deallocate(command)
            if(allocated(atext))     deallocate(atext)
            allocate(updateid(lenresult))
            allocate(supdateid(lenresult))
            allocate(userid(lenresult))
            allocate(mode(lenresult))
            allocate(command(lenresult))
            allocate(atext(lenresult))
            do i = 1, lenresult
                item => fson_value_get(result, i)
                call fson_get(item, 'update_id', updateid(i))
                write(supdateid(i), "(I40)") updateid(i)
                if(fson_check(item, 'message')) then
                    call fson_get(item, 'message', message)
                    call fson_get(message, 'from', from)
                    call fson_get(from, 'id', userid(i))
                    if(fson_check(message, 'text')) then
                        call fson_get(message, 'text', text)
                        messtype = 0
                    else if(fson_check(message, 'document')) then
                        !call fson_get(message, 'document', text)
                        text = ''
                        messtype = 1
                    end if
                    mode(i) = workermode
                else if(fson_check(item, 'inline_query')) then
                    call fson_get(item, 'inline_query', message)
                    call fson_get(message, 'id', supdateid(i))
                    call fson_get(message, 'from', from)
                    call fson_get(from, 'id', userid(i))
                    call fson_get(message, 'query', text)
                    messtype = 2
                    mode(i) = inlinemode
                end if
                atext(i) = trim(text)
                tmptext = atext(i)
                call parsecommand(tmptext, messtype, command(i))
                atext(i) = tmptext
            end do
        else
            lenresult = 0
            if(allocated(updateid))  deallocate(updateid)
            if(allocated(supdateid)) deallocate(supdateid)
            if(allocated(userid))    deallocate(userid)
            if(allocated(command))   deallocate(command)
            if(allocated(atext))     deallocate(atext)
            if(allocated(mode))      deallocate(mode)
            allocate(atext(lenresult))
            allocate(updateid(lenresult))
            allocate(command(lenresult))
            allocate(userid(lenresult))
            allocate(supdateid(lenresult))
        end if
        call fson_destroy(json)
    end subroutine json_rank1

    subroutine send(userid, type, text, status)
        use tgAPI, only : sendmessage, sendphoto, senddocument
        integer(4)      , intent(in)  :: userid, type
        character(len=*), intent(in)  :: text
        integer(4)      , intent(out) :: status
        !tempopary
        character(len=12)             :: convert
        character(len=:), allocatable :: suserid
        select case(type)
            case (0)
                write (convert, '(I12)') userid
                suserid = adjustl(trim(convert))
                call sendmessage(suserid, text, status=status)
            case (1)
                !send photo
                write (convert, '(I12)') userid
                suserid = adjustl(trim(convert))
                call sendphoto(suserid, text, status=status)
            case (2)
                !send document
                write (convert, '(I12)') userid
                suserid = adjustl(trim(convert))
                call senddocument(suserid, text, status=status)
            case (3)
                write (convert, '(I12)') userid
                suserid = adjustl(trim(convert))
                call sendmessage(suserid, text, parse_mode='Markdown', status=status)
            case default
                print *, 'UNSUPPORTED TYPE', type
        end select
    end subroutine

    subroutine parsecommand(message, messtype, command)
        implicit none
        character(len=:), allocatable, intent(inout) :: message
        integer(4),                    intent(in)    :: messtype
        integer(4),                    intent(out)   :: command
        character(len=:), allocatable                :: strtmp
        message = message // '  '
        select case(messtype)
            case (0)
                if(index(message, '/start ') .eq. 1) then
                    command = 0
                    message = ''
                else if(index(message, '/help ') .eq. 1) then
                    command = 1
                    message = ''
                else if(index(message, '/reset ') .eq. 1) then
                    command = 2
                    message = ''
                else if(index(message, '/getpackages ') .eq. 1) then
                    command = 3
                    message = ''
                else if(index(message, '/addpackage ') .eq. 1) then
                    command = 4
                    strtmp = message(13:)
                    message = strtmp
                else if(index(message, '/rempackage ') .eq. 1) then
                    command = 5
                    strtmp = message(13:)
                    message = strtmp
                else if(index(message, '/debug ') .eq. 1) then
                    command = 6
                    strtmp = message(8:)
                    message = strtmp
                else if(index(message, '/fox ') .eq. 1) then
                    command = 7
                    message = ''
                else if(index(message, '/recipe ') .eq. 1) then
                    command = 8
                    strtmp = message(9:)
                    message = strtmp
                else if(index(message, '/version ') .eq. 1) then
                    command = 9
                    message = ''
                else if(index(message, '/preambula ') .eq. 1) then
                    command = 10
                    strtmp = message(11:)
                    message = strtmp
                else if(index(message, '/setpreambula ') .eq. 1) then
                    command = 11
                    strtmp = message(14:)
                    message = strtmp
                else if(index(message, '/ip ') .eq. 1) then
                    command = 12
                    message = ''
                else
                    command = -1
                end if
            case (1)
                command = -2
            case (2)
                if(index(message, '/fox ') .eq. 1) then
                    command = 0
                    message = ''
                else
                    command = -1
                end if
            case default
                command = -200000
                message = 'ERROR IN PARSECOMMAND'
        end select
    end subroutine
end module
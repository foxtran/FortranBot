module actions
    implicit none
contains
    subroutine inputLaTeX(ID, MESSID, LATEX, filenm, strerr)
        use wfile, only : latexdefault, writelog, savelatex, generatelatex
        implicit none
        character(len=:), allocatable, intent(out) :: filenm, strerr
        character(len=*),              intent(in)  :: ID,      MESSID,   LATEX
        character(len=:), allocatable              :: default, packages, fullfile, stat
        integer :: ind1, ind2, lenpack, lendata
        call LatexDefault(default)
        call getPackages(ID, MESSID, packages)
        call writeLog(ID, MESSID, LATEX)
        lenpack = len_trim('%usepackage%')
        ind1 = index(default, '%usepackage%')
        lendata = len_trim('%latex_text%')
        ind2 = index(default, '%latex_text%')
        fullfile = default(:ind1-1) // packages // default(ind1+lenpack:ind2-1) // LATEX // default(ind2+lendata:)
        call saveLaTeX(fullfile, filenm)
        call generateLaTeX(filenm, stat)
        if(len_trim(stat) .ne. 2) then
            strerr = stat
        else
            strerr = ""
            filenm = filenm // '.png'
        end if
        call writeLog(ID, MESSID, filenm)
    end subroutine

    subroutine getPackages(ID, MESSID, settings, isMessage)
        use wfile, wgetPackages => getPackages
        implicit none
        character(len=*), intent(in)  :: ID, MESSID
        character(len=:), allocatable :: settings
        logical         , optional    :: isMessage
        call getUserData(ID, MESSID)
        if(present(isMessage)) then
            call wgetPackages(ID, settings, .TRUE.)
        else
            call wgetPackages(ID, settings)
        end if
    end subroutine

    subroutine reset(ID, MESSID)
        use wfile, only : wreset => reset
        implicit none
        character(len=*), intent(in) :: ID, MESSID
        call wreset(ID, MESSID)
    end subroutine

    subroutine addPackage(ID, MESSID, package)
        use wfile, only : appendHeadline
        implicit none
        character(len=*), intent(in)  :: ID, MESSID, package
        call appendHeadline(ID, MESSID, '\\usepackage', package)
    end subroutine

    subroutine addPreambula(ID, MESSID, message)
        use wfile, only : appendHeadline
        implicit none
        character(len=*), intent(in)  :: ID, MESSID, message
        call appendHeadline(ID, MESSID, '', message)
    end subroutine

    subroutine removePackage(ID, MESSID, num_package, text)
        use wfile, wgetPackages => getPackages
        implicit none
        character(len=*), intent(in)               :: ID, MESSID, num_package
        character(len=:), allocatable, intent(out) :: text
        integer(4)                                 :: val, stat, line, skip
        character(len=:), allocatable              :: settings, strtmp
        character(len=5)                           :: sline
        val = 0
        line = 1
        skip = 1
        read(num_package, *, iostat=stat) val
        call wgetPackages(ID, settings, .TRUE.)
        if(stat.eq.0) then
            text = 'Number was read, but function is not implemented yet'
            strtmp = ''
            do while (index(settings(skip:), '\n').ne.0)
                write (sline, '(I5)') line
                if(line.ne.val) then
                    strtmp = strtmp // settings(skip:skip + index(settings(skip:), '\n')-1)
                end if
                skip = skip + index(settings(skip:), '\n')
                line = line + 1
            end do
            call setHeadlines(ID, MESSID, strtmp)
            text = 'Now, your file is: ```\n' // strtmp // '```'
        else
            text = 'Sorry, It is not a number\nYour file with line numbersis: ```'
            do while (index(settings(skip:), '\n').ne.0)
                write (sline, '(I5)') line
                strtmp = settings(:skip-1) // sline // ': ' // settings(skip:)
                settings = strtmp
                skip = skip + index(settings(skip:), '\n')
                line = line + 1
            end do
            text = text // settings // '```\n If you want to remove some string, input this line number.'
        end if
    end subroutine

    subroutine start(str)
        implicit none
        character(len=:), allocatable :: str
        str = 'Hi!' // NEW_LINE('') // 'I am a simple *Fortran* bot!'
    end subroutine

    subroutine ip(ID, MESSID, text)
        character(len=*), intent(in)               :: ID, MESSID
        character(len=*), intent(out)              :: text
        integer                                    :: idmess
        character(len=1024)                        :: msg
        read (MESSID, '(I10)') idmess
        if(idmess .ne. 336838433) then
            text = 'You do not access to this command'
        else
            text = 'Something went wrong'
            call execute_command_line('ifconfig', cmdmsg=msg)
            print *, msg
            text = trim(msg)
        end if
    end subroutine

    subroutine help(str)
        implicit none
        character(len=:), allocatable :: str
        str = 'I can convert your text into LaTeX pictire'
    end subroutine

    subroutine debug()
    end subroutine

    subroutine fox(key)
        use strings, only : ItC
        character(len=:), allocatable, intent(out) :: key
        real(4)                                    :: randreal
        integer(4)                                 :: fileid
        call random_number(randreal)
        fileid = 1 + floor(randreal*(5))
        key = './foxes/' // ItC(fileid) // '.jpg'
    end subroutine

    subroutine generateResponse(userid, updateid, command, inputtext, resulttype, key, error)
        use wfile, only : writelog, checkfiles, setHeadlines
        implicit none
        integer(4),                    intent(in)  :: userid, updateid, command
        character(len=*),              intent(in)  :: inputtext
        integer(4),                    intent(out) :: resulttype
        character(len=:), allocatable, intent(out) :: key, error
        character(len=12)                          :: suserid, supdateid, scommand
        write (suserid, "(I12)") userid
        write (supdateid, "(I12)") updateid
        write (scommand, "(I12)") command
        call checkFiles(suserid, supdateid)
        error = ""
        if(command.le.-3) then
            key = 'Something went wrong :('
            resulttype = 0
        else if(command.eq.-2) then
            key = 'files not supported yet'
            resulttype = 0
        else if(command.eq.-1) then
            call inputlatex(suserid, supdateid, inputtext, key, error)
            resulttype = 1
        else if(command.eq.0) then
            call start(key)
            resulttype = 3
        else if(command.eq.1) then
            call help(key)
            resulttype = 0
        else if(command.eq.2) then
            call reset(suserid, supdateid)
            key = 'Resetting your settings'
            resulttype = 0
        else if(command.eq.3) then
            call getPackages(suserid, supdateid, key, .TRUE.)
            resulttype = 0
        else if(command.eq.4) then
            if (len(trim(inputtext)).eq.0) then
                key = 'Usage "/addpackage PACKAGE". It will be generated as "\\usepackagePACKAGE" '
            else
                call addPackage(suserid, supdateid, inputtext)
                key = 'Package was added'
            end if
            resulttype = 0
        else if(command.eq.5) then
            call removePackage(suserid, supdateid, inputtext, key)
            resulttype = 3
        else if(command.eq.6) then
            call inputlatex(suserid, supdateid, inputtext, key, error)
            resulttype = 1
            if(index(key, 'png').eq.0) then
                resulttype = 2
                key = key // '.log'
            end if
        else if(command.eq.7) then
            call fox(key)
            resulttype = 1
        else if(command.eq.8) then
            key = 'Command not implemented yet'
            resulttype = 0
        else if(command.eq.9) then
            key = '``` VERSION    ' // VERSION // NEW_LINE('') // 'COMPILED AT ' // __TIMESTAMP__ // '```'
            resulttype = 3
        else if(command.eq.10) then
            if (len(trim(inputtext)).eq.0) then
                key = 'Usage "/preambula text". It will be generated as "text" near \\usepackages '
            else
                call addPreambula(suserid, supdateid, inputtext)
                key = 'Preambula was added'
            end if
            resulttype = 0
        else if(command.eq.11) then
            if(len(trim(inputtext)).eq.0) then
                key = 'Usage "/setpreambula text". It changed your preabula to text'
            else
                call setHeadlines(suserid, supdateid, inputtext)
                key = 'Preambula was changed'
            end if
            resulttype = 0
        else if(command.eq.12) then
            call ip(suserid, supdateid, key)
            resulttype = 0
        else if(command.ge.13) then
            key = 'Unsupported command'
            resulttype = 0
        else
            key = 'How do you do this?'
            resulttype = 0
        end if
        call writelog(suserid, supdateid, scommand)
        call writelog(suserid, supdateid, inputtext)
        call writelog(suserid, supdateid, key)
        call writelog(suserid, supdateid, error)
    end subroutine

    subroutine inlineresponse(userid, supdateid, command, inputtext)
        use wfile, only : writelog, checkfiles
        use define, only : URLBOT
        use inline
        use tgAPI, only : answerInlineQuery
        integer(4),                    intent(in) :: userid, command
        character(len=*),              intent(in) :: supdateid, inputtext
        integer(4)                                :: resulttype
        character(len=:), allocatable             :: key, error
        character(len=12)                         :: suserid, scommand
        character(len=:), allocatable             :: result, text, message
        integer(4)                                :: stat
        write (suserid, "(I12)") userid
        write (scommand, "(I12)") command
        call checkFiles(suserid, supdateid(1:7))
        error = ""
        key = ""
        if(command.le.-2) then
            key = "Something went wrong"
            resulttype = 0
        else if(command.eq.-1) then
            call inputlatex(suserid, trim(supdateid(1:7)), inputtext, key, error)
            if(index(key, 'png').eq.0) then
                resulttype = 0
                key = key // '.log'
            else
                key = URLBOT // key(2:)
            end if
            resulttype = 1
        else if(command.eq.0) then
            call fox(key)
            key = URLBOT // key(2:)
            resulttype = 1
        else
            key = "Something went wrong"
            resulttype = 0
        end if
        if(resulttype.eq.1) then
            call InlineQueryResultPhoto(id="0", photo_url=key, thumb_url=key, json_str=text)
            call ResultArray([text], result)
            print *, text, supdateid, suserid
            call answerInlineQuery(inline_query_id=trim(supdateid), results=result, is_personal=.true., status=stat)
        else
            call InputTextMessageContent(message_text="Something went wrong :(", json_str=message)
            call InlineQueryResultArticle(id="0", Title="ERROR IN LaTeX", input_message_content=message, json_str=text)
            call ResultArray([text], result)
            call answerInlineQuery(inline_query_id=trim(supdateid), results=result, is_personal=.true., status=stat)
            print *, text, supdateid, suserid
        end if
        call writelog(suserid, supdateid(1:7), scommand)
        call writelog(suserid, supdateid(1:7), inputtext)
        call writelog(suserid, supdateid(1:7), key)
    end subroutine
end module
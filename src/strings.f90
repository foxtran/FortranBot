module strings
contains
    function LtC(l)
        implicit none
        logical                       :: l
        character(len=:), allocatable :: LtC
        if(l.eqv..TRUE.) then
            LtC = 'true'
        else
            LtC = 'false'
        end if
    end function LtC

    function ItC(i)
        implicit none
        integer                       :: i
        character(len=:), allocatable :: ItC
        character(len=12)             :: tmp
        write(tmp, '(I0)') i
        ItC = trim(tmp)
    end function ItC

    function ASCIItoUTF8(ascii)
        implicit none
        integer, parameter                    :: u = selected_char_kind('ISO_10646')
        character(len=*)                      :: ascii
        character(kind=u, len=:), allocatable :: parse
        character(kind=u, len=:), allocatable :: ASCIItoUTF8
        integer                               :: i, escape, length, code
        logical                               :: escaped
        character(len=10)                     :: tmp
        character                             :: this
        length = len(ascii)
        escaped = .FALSE.
        escape = 0
        parse = ''
        do i = 1, length
            this = ascii(i:i)
            if(escaped) then
                escaped = .FALSE.
                select case(this)
                    case ('x')
                        escape = 2
                        tmp = ascii(i+1:i+2)
                        parse = parse // CHAR(CXtI(tmp), u)
                    case ('u')
                        escape = 4
                        tmp = ascii(i+1:i+4)
                        parse = parse // CHAR(CXtI(tmp), u)
                    case ('U')
                        escape = 8
                        tmp = ascii(i+1:i+8)
                        parse = parse // CHAR(CXtI(tmp), u)
                    case default
                        print *, 'PARSE ERROR'
                        escape = 0
                        tmp = ''
                end select
            else if(escape.ne.0) then
                escape = escape - 1
            else if(ICHAR(this).eq.0) then
                escaped = .TRUE.
            else
                parse = parse // CHAR(ICHAR(this), u)
            end if
        end do
        print *, parse
        asciitoutf8 = parse
    end function

    INTEGER(4) FUNCTION CXtI(STR) RESULT(I)
        IMPLICIT NONE
        CHARACTER(len=*) :: STR
        READ(STR, "(Z8)") I
    END FUNCTION
end module
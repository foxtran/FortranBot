module wsleep
contains
    subroutine fsleep(secs)
        implicit none
        integer(4), intent(in) :: secs
        call sleep(secs)
    end subroutine

    subroutine msleep(millisecs)
        implicit none
        integer(4), intent(in) :: millisecs
        call Csleep(millisecs)
    end subroutine
end module
module define
    implicit none
    integer :: count_inline, count_sender
    integer :: workermode, inlinemode
    
    character(len=*) :: START_ERROR
    character(len=*) :: URLBOT, BOT_USERNAME
    character(len=*) :: TGBOTKEY
    
    parameter(count_inline = 3, count_sender=4)
    parameter(workermode = 0, inlinemode = 1)
    
    parameter(START_ERROR="THIS PROGRAM CANNOT WORK\nTHREADS NOT ENOUGH\nNEEDS, AT LEAST, 11") ! updater + parser + count_worker (1) + count_inline + queue+sender + count_sender
    parameter(URLBOT = "https://" // URLBOTKEY, BOT_USERNAME = "testfortranapibot")
    parameter(TGBOTKEY = BOTKEY)
end module
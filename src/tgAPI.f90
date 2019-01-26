module tgAPI
    use ISO_C_BINDING, only: C_INT, C_CHAR, C_NULL_CHAR
    use strings, only : ItC, LtC
    use define, only : TGBOTKEY
    implicit none
    character(len=*) :: apibotkey
    integer          :: maxsizetext
    parameter (apibotkey = 'https://api.telegram.org/bot' // TGBOTKEY)
    parameter (MAXSIZETEXT = 1048576)

    INTERFACE
        SUBROUTINE CCURL_GET(RESULT, URL, MESSAGE, LENGTH, STATUS) BIND(C)
            IMPLICIT NONE
            CHARACTER, DIMENSION(*) :: RESULT, URL, MESSAGE
            INTEGER                 :: LENGTH, STATUS
        END SUBROUTINE

        SUBROUTINE CCURL_SENDFILE(RESULT, URL, COUNT, KEYLEN, KEYS, VALLEN, VALUES, FILETYPE, FILEPATH, LENGTH, STATUS) BIND(C)
            CHARACTER, DIMENSION(*) :: RESULT, URL, FILETYPE, FILEPATH
            INTEGER                 :: COUNT, KEYLEN, VALLEN, LENGTH, STATUS
            CHARACTER               :: KEYS(COUNT, KEYLEN), VALUES(COUNT, KEYLEN)
        END SUBROUTINE
    END INTERFACE

contains
    subroutine get(subURL, message, result, status)
        implicit none
        character(len=*),              intent(in)  :: subURL
        character(len=*)             , intent(in)  :: message
        character(len=:), allocatable, intent(out) :: result
        integer(4)                   , intent(out) :: status
        character(len=MAXSIZETEXT)                 :: response
        character(len=:), allocatable              :: URL
        URL = apibotkey // subURL // C_NULL_CHAR
        call ccurl_get(response, URL, message // C_NULL_CHAR, MAXSIZETEXT, status)
        result = trim(response)
    end subroutine

    subroutine post(subURL, message, status)
        character(len=*),              intent(in)  :: subURL
        character(len=*)             , intent(in)  :: message
        integer(4)                   , intent(out) :: status
        character(len=MAXSIZETEXT)                 :: response
        character(len=:), allocatable              :: URL
        URL = apibotkey // subURL // C_NULL_CHAR
        call ccurl_get(response, URL, message // C_NULL_CHAR, MAXSIZETEXT, status)
    end subroutine

    subroutine sendfile(subURL, keys, values, filetype, filepath, status)
        character(len=*)             , intent(in)  :: subURL
        character(len=*)             , intent(in)  :: filepath, filetype
        integer(4)                   , intent(out) :: status
        character(len=:), allocatable, intent(in)  :: keys(:), values(:)
        character(len=MAXSIZETEXT)                 :: response
        character(len=:), allocatable              :: URL
        integer(4)      , allocatable              :: shaper(:)
        URL = apibotkey // subURL // C_NULL_CHAR
        shaper = shape(keys)
        call ccurl_sendfile(response, URL, shaper(1), len(keys(1)), keys, len(values(1)), values, filetype // C_NULL_CHAR, filepath // C_NULL_CHAR, MAXSIZETEXT, status)
        print *, trim(response)
    end subroutine

    subroutine getMe(result, status)
        implicit none
        character(len=:), allocatable, intent(out) :: result
        integer(4)      , optional   , intent(out) :: status
        !temporary
        character(len=:), allocatable              :: message
        integer(4)                                 :: hidden
        message = ''
        hidden = 0
        call get('/getMe', message, result, hidden)
        if(present(status)) status = hidden
    end subroutine

    subroutine getUpdates(result, offset, limit, timeout, status)
        implicit none
        character(len=:), allocatable, intent(out) :: result
        integer(4)      , optional   , intent(in)  :: offset, limit, timeout
        integer(4)      , optional   , intent(out) :: status
        !temporary
        character(len=:), allocatable              :: message, strtmp
        integer(4)                                 :: hidden
        message = ''
        hidden = 0
        if(present(offset)) then
            message = message // '&offset=' // ItC(offset)
        end if
        if(present(limit)) then
            message = message // '&limit=' // ItC(limit)
        end if
        if(present(timeout)) then
            message = message // '&timeout=' // ItC(timeout)
        end if
        if(len(message).ne.0) then
            strtmp = message(2:)
            message = strtmp
        end if
        print *, message
        call get('/getUpdates', message, result, hidden)
        if(present(status)) status = hidden
    end subroutine

    subroutine sendMessage(chat_id, text, parse_mode, disable_web_page_preview, disable_notification, reply_to_message_id, reply_markup, status)
        character(len=*)          , intent(in)  :: chat_id, text
        character(len=*), optional, intent(in)  :: parse_mode, reply_markup
        logical         , optional, intent(in)  :: disable_web_page_preview, disable_notification
        integer(4)      , optional, intent(in)  :: reply_to_message_id
        integer(4)      , optional, intent(out) :: status
        !tempopary
        character(len=:), allocatable :: message
        integer(4)                                 :: hidden
        hidden = 0
        message = 'chat_id=' // chat_id // '&text=' // text
        if(present(parse_mode)) then
            message = message // '&parse_mode=' // parse_mode
        end if
        if(present(disable_web_page_preview)) then
            message = message // '&disable_web_page_preview=' // LtC(disable_web_page_preview)
        end if
        if(present(disable_notification)) then
            message = message // '&disable_notification=' // LtC(disable_notification)
        end if
        if(present(reply_to_message_id)) then
            message = message // '&reply_to_message_id=' // ItC(reply_to_message_id)
        end if
        if(present(reply_markup)) then
            message = message // '&reply_markup=' // reply_markup
        end if
        call post('/sendMessage', message, status)
        if(present(status)) status = hidden
    end subroutine

    subroutine sendPhoto(chat_id, photo, caption, disable_notification, reply_to_message_id, reply_markup, status)
        character(len=*)          , intent(in)  :: chat_id, photo
        character(len=*), optional, intent(in)  :: caption, reply_markup
        logical         , optional, intent(in)  :: disable_notification
        integer(4)      , optional, intent(in)  :: reply_to_message_id
        integer(4)      , optional, intent(out) :: status
        !tempopary
        character(len=:), allocatable           :: keys(:), values(:)
        integer(4)                              :: hidden, count, maxlen
        hidden = 0
        count = 1
        maxlen = len(chat_id)
        if(present(caption)) then
            count = count + 1
            maxlen = max(maxlen, len(caption))
        end if
        if(present(disable_notification)) then
            count = count + 1
            maxlen = max(maxlen, len(LtC(disable_notification)))
        end if
        if(present(reply_to_message_id)) then
            count = count + 1
            maxlen = max(maxlen, len(ItC(reply_to_message_id)))
        end if
        if(present(reply_markup)) then
            count = count + 1
            maxlen = max(maxlen, len(reply_markup))
        end if
        allocate(character(len=maxlen+1) :: values(count))
        allocate(character(len=80) :: keys(count))
        count         = 1
        keys  (count) = 'chat_id' // C_NULL_CHAR
        values(count) =  chat_id  // C_NULL_CHAR
        if(present(caption)) then
            count         = count + 1
            keys  (count) = 'caption' // C_NULL_CHAR
            values(count) =  caption  // C_NULL_CHAR
        end if
        if(present(disable_notification)) then
            count         = count + 1
            keys  (count) =  'disable_notification'  // C_NULL_CHAR
            values(count) = LtC(disable_notification) // C_NULL_CHAR
        end if
        if(present(reply_to_message_id)) then
            count         = count + 1
            keys  (count) =  'reply_to_message_id'  // C_NULL_CHAR
            values(count) = ItC(reply_to_message_id) // C_NULL_CHAR
        end if
        if(present(reply_markup)) then
            count         = count + 1
            keys  (count) = 'reply_markup' // C_NULL_CHAR
            values(count) =  reply_markup  // C_NULL_CHAR
        end if
        !subroutine sendfile(subURL, keys, values, filetype, filepath, status)
        call sendfile('/sendPhoto', keys, values, 'photo', photo, hidden)
        if(present(status)) status = hidden
    end subroutine

    subroutine sendDocument(chat_id, document, caption, disable_notification, reply_to_message_id, reply_markup, status)
        character(len=*)          , intent(in)  :: chat_id, document
        character(len=*), optional, intent(in)  :: caption, reply_markup
        logical         , optional, intent(in)  :: disable_notification
        integer(4)      , optional, intent(in)  :: reply_to_message_id
        integer(4)      , optional, intent(out) :: status
        !tempopary
        character(len=:), allocatable           :: keys(:), values(:)
        integer(4)                              :: hidden, count, maxlen
        hidden = 0
        count = 1
        maxlen = len(chat_id)
        if(present(caption)) then
            count = count + 1
            maxlen = max(maxlen, len(caption))
        end if
        if(present(disable_notification)) then
            count = count + 1
            maxlen = max(maxlen, len(LtC(disable_notification)))
        end if
        if(present(reply_to_message_id)) then
            count = count + 1
            maxlen = max(maxlen, len(ItC(reply_to_message_id)))
        end if
        if(present(reply_markup)) then
            count = count + 1
            maxlen = max(maxlen, len(reply_markup))
        end if
        allocate(character(len=maxlen+1) :: values(count))
        allocate(character(len=80) :: keys(count))
        count         = 1
        keys  (count) = 'chat_id' // C_NULL_CHAR
        values(count) =  chat_id  // C_NULL_CHAR
        if(present(caption)) then
            count         = count + 1
            keys  (count) = 'caption' // C_NULL_CHAR
            values(count) =  caption  // C_NULL_CHAR
        end if
        if(present(disable_notification)) then
            count         = count + 1
            keys  (count) =  'disable_notification'  // C_NULL_CHAR
            values(count) = LtC(disable_notification) // C_NULL_CHAR
        end if
        if(present(reply_to_message_id)) then
            count         = count + 1
            keys  (count) =  'reply_to_message_id'  // C_NULL_CHAR
            values(count) = ItC(reply_to_message_id) // C_NULL_CHAR
        end if
        if(present(reply_markup)) then
            count         = count + 1
            keys  (count) = 'reply_markup' // C_NULL_CHAR
            values(count) =  reply_markup  // C_NULL_CHAR
        end if
        !subroutine sendfile(subURL, keys, values, filetype, filepath, status)
        call sendfile('/sendDocument', keys, values, 'document', document, hidden)
        if(present(status)) status = hidden
    end subroutine

    subroutine answerInlineQuery(inline_query_id, results, cache_time, is_personal, next_offset, switch_pm_text, switch_pm_parameter, status)
        character(len=*),           intent(in)  :: inline_query_id, results
        integer(4),       optional, intent(in)  :: cache_time
        logical,          optional, intent(in)  :: is_personal
        character(len=*), optional, intent(in)  :: next_offset, switch_pm_text, switch_pm_parameter
        integer(4),       optional, intent(out) :: status
        character(len=:), allocatable           :: message, result
        integer(4)                              :: hidden
        message = "inline_query_id=" // inline_query_id // "&results=" // results
        if(present(cache_time)) message = message // "&cache_time=" // ItC(cache_time)
        if(present(is_personal)) message = message // "&is_personal=" // LtC(is_personal)
        if(present(next_offset)) message = message // "&next_offset=" // next_offset
        if(present(switch_pm_text)) message = message // "&switch_pm_text=" // switch_pm_text
        if(present(switch_pm_parameter)) message = message // "&switch_pm_parameter=" // switch_pm_parameter
        call get("/answerInlineQuery", message, result, status)
        print *, result
        if(present(status)) status = hidden
    end subroutine

    subroutine CURL_ERR(status, text)
        integer(4),                    intent(in)  :: status
        character(len=:), allocatable, intent(out) :: text
        text = ''
        select case (status)
            case (0)
                text = 'CURLE_OK'
            case (7)
                text = 'CURLE_COULDNT_CONNECT'
            case (23)
                text = 'CURLE_WRITE_ERROR'
            case (35)
                text = 'CURLE_SSL_CONNECT_ERROR'
            case (52)
                text = 'CURLE_GOT_NOTHING'
            case (60)
                text = 'CURLE_PEER_FAILED_VERIFICATION'
            case default
                text = 'UNSUPPORTED ERROR: ' // ItC(status)
        end select
    end subroutine
end module
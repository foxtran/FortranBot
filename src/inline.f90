module inline
    use strings, only : ItC, LtC
    implicit none
contains
    subroutine InlineQueryResultArticle(id, title, input_message_content, reply_markup, url, hide_url, description, thumb_url, thumb_width, thumb_height, json_str)
        character(len=*),              intent(in)  :: id, title, input_message_content
        character(len=*), optional,    intent(in)  :: url, description, thumb_url, reply_markup
        integer,          optional,    intent(in)  :: thumb_width, thumb_height
        logical,          optional,    intent(in)  :: hide_url
        character(len=:), allocatable, intent(out) :: json_str
        json_str = "{"
        json_str = json_str // '"type":"article","id":"' // id // '","title":"' // title // '",input_message_content":' // input_message_content
        if(present(reply_markup)) json_str = json_str // ',"reply_markup":' // reply_markup
        if(present(url))          json_str = json_str // ',"usr":"' // url // '"'
        if(present(hide_url))     json_str = json_str // ',"hide_url":' // LtC(hide_url)
        if(present(description))  json_str = json_str // ',"description":"' // description // '"'
        if(present(thumb_url))    json_str = json_str // ',"thumb_url":"' // thumb_url // '"'
        if(present(thumb_width))  json_str = json_str // ',"thumb_width":' // ItC(thumb_width)
        if(present(thumb_height)) json_str = json_str // ',"thumb_height":' // ItC(thumb_height)
        json_str = json_str // "}"
    end subroutine

    subroutine InlineQueryResultPhoto(id, photo_url, thumb_url, photo_width, photo_height, title, description, caption, parse_mode, reply_markup, input_message_content, json_str)
        character(len=*),              intent(in)  :: id, photo_url, thumb_url
        integer,          optional,    intent(in)  :: photo_width, photo_height
        character(len=*), optional,    intent(in)  :: title, description, caption, parse_mode, reply_markup, input_message_content
        character(len=:), allocatable, intent(out) :: json_str
        json_str = "{"
        json_str = json_str // '"type":"photo","id":"' // id // '","photo_url":"' // photo_url // '","thumb_url":"' // thumb_url // '"'
        if(present(photo_width))  json_str = json_str // ',"photo_width":' // ItC(photo_width)
        if(present(photo_height)) json_str = json_str // ',"photo_height":' // ItC(photo_height)
        if(present(title))        json_str = json_str // ',"title":"' // title // '"'
        if(present(description))  json_str = json_str // ',"description":"' // description // '"'
        if(present(caption))      json_str = json_str // ',"caption":"' // caption // '"'
        if(present(parse_mode))   json_str = json_str // ',"parse_mode":"' // parse_mode // '"'
        if(present(reply_markup)) json_str = json_str // ',"reply_markup":' // reply_markup
        if(present(input_message_content)) json_str = json_str // ',"input_message_content":' // input_message_content
        json_str = json_str // "}"
    end subroutine

    subroutine InputTextMessageContent(message_text, parse_mode, disable_web_page_preview, json_str)
        character(len=*),              intent(in)  :: message_text
        character(len=*), optional,    intent(in)  :: parse_mode
        logical,          optional,    intent(in)  :: disable_web_page_preview
        character(len=:), allocatable, intent(out) :: json_str
        json_str = "{"
        json_str = json_str // '"message_text":"' // message_text // '"'
        if(present(parse_mode))               json_str = json_str // ',"parse_mode":"' // parse_mode // '"'
        if(present(disable_web_page_preview)) json_str = json_str // ',"disable_web_page_preview":' // LtC(disable_web_page_preview)
        json_str = json_str // "}"
    end subroutine

    subroutine ResultArray(stringarray, json_str)
        character(len=*), dimension(:),              intent(in)  :: stringarray
        character(len=:),               allocatable, intent(out) :: json_str
        integer                                                  :: i
        json_str = "["
        do i = 1, size(stringarray)
            json_str = json_str // stringarray(i) // ","
        end do
        json_str(len(json_str):len(json_str)) = "]"
    end subroutine
end module inline
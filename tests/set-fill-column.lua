-- FIXME: Next line should not be needed
call_command ("setq", "sentence-end-double-space", "nil")
call_command ("edit-select-on", "point")
call_command ("forward-line", "2")
call_command ("edit-kill-selection", "point", "mark")
call_command ("edit-paste")
call_command ("edit-paste")
call_command ("open-line", "1")
call_command ("forward-line")
call_command ("edit-paste")
call_command ("forward-line")
call_command ("set-fill-column", "3")
call_command ("edit-wrap-paragraph")
call_command ("forward-line", "-3")
call_command ("set-fill-column", "12")
call_command ("edit-wrap-paragraph")
call_command ("forward-line", "-6")
call_command ("move-end-line")
call_command ("set-fill-column", "33")
call_command ("edit-wrap-paragraph")
call_command ("file-save")
call_command ("file-quit")

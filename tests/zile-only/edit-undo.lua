-- undo in Emacs undoes everything from the start of the script (cf. fill-paragraph_2.el)
call_command ("edit-delete-next-character", "1")
call_command ("edit-delete-next-character", "1")
call_command ("edit-delete-next-character", "1")
call_command ("edit-delete-next-character", "1")
call_command ("edit-undo")
call_command ("edit-undo")
call_command ("edit-undo")
call_command ("file-save")
call_command ("file-quit")

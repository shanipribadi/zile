-- describe-variable "tab-width" RET other-window edit-select-on edit-goto-line 2 RET
-- edit-repeat move-previous-word edit-copy other-window edit-paste
-- file-save file-quit
call_command ("macro-play", "\\M-xdescribe-variable\\rtab-width\\r\\C-xo\\C-@\\M-gg2\\r\\C-u\\M-b\\M-w\\C-xo\\C-y\\C-x\\C-s\\C-x\\C-c")

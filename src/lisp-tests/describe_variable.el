(describe-variable "tab-width")
(other-window 1)
(set-mark (point))
(forward-line)
(kill-region (point) (mark))
(other-window -1)
(yank)
(save-buffer)
(save-buffers-kill-emacs)

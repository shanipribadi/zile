/*	$Id: tbl_funcs.h,v 1.32 2005/01/26 18:26:48 rrt Exp $	*/

/*
 * Add an entry to this list for declaring a new function.
 * X0 means no key binding, X1 means one key binding, etc.
 *
 * Please remember to keep in sync with the Texinfo documentation
 * `../doc/zile.texi' and the manual page `zile.1.in'.
 */

X0("auto-fill-mode", auto_fill_mode)
X1("back-to-indentation", back_to_indentation,		"\\M-m")
X2("backward-char", backward_char,			"\\C-b", "\\LEFT")
X1("backward-delete-char", backward_delete_char,	"\\BS")
X2("backward-kill-word", backward_kill_word,		"\\M-\\DEL", "\\M-\\BS")
X1("backward-paragraph", backward_paragraph,		"\\M-{")
X1("backward-sexp", backward_sexp,			"\\C-\\M-b")
X1("backward-word", backward_word,			"\\M-b")
X1("beginning-of-buffer", beginning_of_buffer,		"\\M-<")
X2("beginning-of-line", beginning_of_line,		"\\C-a", "\\HOME")
X2("call-last-kbd-macro", call_last_kbd_macro,		"\\C-xe", "\\F12")
X1("capitalize-word", capitalize_word,			"\\M-c")
X0("cd", cd)
X1("copy-region-as-kill", copy_region_as_kill,		"\\M-w")
X1("copy-to-register", copy_to_register,		"\\C-xrs")
X1("delete-blank-lines", delete_blank_lines,		"\\C-x\\C-o")
X2("delete-char", delete_char,				"\\C-d", "\\DEL")
X1("delete-horizontal-space", delete_horizontal_space,	"\\M-\\\\")
X1("delete-other-windows", delete_other_windows,	"\\C-x1")
X1("just-one-space", just_one_space,			"\\M- ")
X1("delete-window", delete_window,			"\\C-x0")
X0("delete-region", delete_region)
X2("describe-function", describe_function,		"\\C-hd", "\\C-hf")
X1("describe-key", describe_key,			"\\C-hk")
X1("describe-variable", describe_variable,		"\\C-hv")
X1("downcase-region", downcase_region,			"\\C-x\\C-l")
X1("downcase-word", downcase_word,			"\\M-l")
X1("end-kbd-macro", end_kbd_macro,			"\\C-x)")
X1("end-of-buffer", end_of_buffer,			"\\M->")
X2("end-of-line", end_of_line,				"\\C-e", "\\END")
X1("enlarge-window", enlarge_window,			"\\C-x^")
X1("eval-expression", eval_expression,			"\\M-:")
X1("eval-last-sexp", eval_last_sexp,			"\\C-x\\C-e")
X1("exchange-point-and-mark", exchange_point_and_mark,	"\\C-x\\C-x")
X1("execute-extended-command", execute_extended_command,"\\M-x")
X1("fill-paragraph", fill_paragraph,			"\\M-q")
X1("find-alternate-file", find_alternate_file,		"\\C-x\\C-v")
X2("find-file", find_file,				"\\C-x\\C-f", "\\F2")
X2("forward-char", forward_char,			"\\C-f", "\\RIGHT")
X0("forward-line", forward_line)
X1("forward-paragraph", forward_paragraph,		"\\M-}")
X1("forward-sexp", forward_sexp,			"\\C-\\M-f")
X1("forward-word", forward_word,			"\\M-f")
X0("global-set-key", global_set_key)
X0("goto-char", goto_char)
X1("goto-line", goto_line,				"\\M-g")
X2("help", help,					"\\C-hh", "\\F1")
X1("help-config-sample", help_config_sample,		"\\C-hs")
X1("help-with-tutorial", help_with_tutorial,		"\\C-ht")
X1("indent-command", indent_command,			"\\TAB")
X0("insert-buffer", insert_buffer)
X1("insert-file", insert_file,				"\\C-xi")
X1("insert-register", insert_register,			"\\C-xri")
X1("isearch-backward-regexp", isearch_backward_regexp,	"\\C-r")
X1("isearch-forward-regexp", isearch_forward_regexp,	"\\C-s")
X1("keyboard-quit", keyboard_quit,			"\\C-g")
X1("kill-buffer", kill_buffer,				"\\C-xk")
X2("kill-line", kill_line,				"\\C-k", "\\F6")
X2("kill-region", kill_region,				"\\C-w", "\\F7")
X1("kill-sexp", kill_sexp,				"\\C-\\M-k")
X1("kill-word", kill_word,				"\\M-d")
X1("list-bindings", list_bindings,			"\\C-hb")
X1("list-buffers", list_buffers,			"\\C-x\\C-b")
X1("list-functions", list_functions,			"\\C-hlf")
X1("list-registers", list_registers,			"\\C-hlr")
X1("mark-whole-buffer", mark_whole_buffer,		"\\C-xh")
X1("mark-paragraph", mark_paragraph,			"\\M-h")
X1("mark-sexp", mark_sexp,				"\\C-\\M-@")
X1("mark-word", mark_word,				"\\M-@")
X0("name-last-kbd-macro", name_last_kbd_macro)
X1("newline", newline,					"\\RET")
X1("newline-and-indent", newline_and_indent,		"\\C-j")
X2("next-line", next_line,				"\\C-n", "\\DOWN")
X1("open-line", open_line,				"\\C-o")
X1("other-window", other_window,			"\\C-xo")
X1("overwrite-mode", overwrite_mode,			"\\INS")
X2("previous-line", previous_line,			"\\C-p", "\\UP")
X1("query-replace-regexp", query_replace_regexp,	"\\M-%")
X1("quoted-insert", quoted_insert,			"\\C-q")
X1("recenter", recenter,				"\\C-l")
X0("replace-regexp", replace_regexp)
X2("save-buffer", save_buffer,				"\\C-x\\C-s", "\\F3")
X1("save-buffers-kill-zile", save_buffers_kill_zile,	"\\C-x\\C-c")
X1("save-some-buffers", save_some_buffers,		"\\C-xs")
X2("scroll-down", scroll_down,				"\\M-v", "\\PGUP")
X2("scroll-up", scroll_up,				"\\C-v", "\\PGDN")
X0("search-backward-regexp", search_backward_regexp)
X0("search-forward-regexp", search_forward_regexp)
X0("self-insert-command", self_insert_command)
X1("set-fill-column", set_fill_column,			"\\C-xf")
X2("set-mark-command", set_mark_command,		"\\C-@", "\\F5")
X0("set-variable", set_variable)
X1("shell-command", shell_command,			"\\M-!")
X1("shell-command-on-region", shell_command_on_region,	"\\M-|")
X0("shrink-window", shrink_window)
X1("split-window", split_window,			"\\C-x2")
X1("start-kbd-macro", start_kbd_macro,			"\\C-x(")
X2("suspend-zile", suspend_zile,			"\\C-x\\C-z", "\\C-z")
X1("switch-to-buffer", switch_to_buffer,		"\\C-xb")
X0("tabify", tabify)
X1("tab-to-tab-stop", tab_to_tab_stop,			"\\M-i")
X1("toggle-read-only", toggle_read_only,		"\\C-x\\C-q")
X0("transient-mark-mode", transient_mark_mode)
X1("transpose-chars", transpose_chars,			"\\C-t")
X1("transpose-lines", transpose_lines,			"\\C-x\\C-t")
X1("transpose-sexps", transpose_sexps,			"\\C-\\M-t")
X1("transpose-words", transpose_words,			"\\M-t")
X3("undo", undo,					"\\C-xu", "\\C-_", "\\F4")
X1("universal-argument", universal_argument,		"\\C-u")
X0("untabify", untabify)
X1("upcase-region", upcase_region,			"\\C-x\\C-u")
X1("upcase-word", upcase_word,				"\\M-u")
X1("view-zile-FAQ", view_zile_FAQ,			"\\C-hF")
X1("write-file", write_file,				"\\C-x\\C-w")
X2("yank", yank,					"\\C-y", "\\F8")
X0("zile-version", zile_version)

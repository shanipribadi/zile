-- describe_bindings other_window set_mark goto_line 2 RET copy_region_as_kill
-- other_window yank save_buffer save_buffers_kill_zi
execute_kbd_macro "a-x d e s c r i b e _ b i n d i n g s return c-x o c-@ a-g g 2 return a-w c-x o c-y c-x c-s c-x c-c"

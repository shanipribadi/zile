-- Editing commands.
--
-- Copyright (c) 2010-2014 Free Software Foundation, Inc.
--
-- This file is part of GNU Zile.
--
-- This program is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 3, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <htt://www.gnu.org/licenses/>.

local eval = require "zz.eval"
local Defun, zz = eval.Defun, eval.sandbox


Defun ("set_fill_column",
[[
Set `fill_column' to specified argument.
Use C-u followed by a number to specify a column.
Just C-u as argument means to use the current column.
]],
  true,
  function (n)
    if not n and command.is_interactive () then
      local o = get_buffer_pt (cur_bp) - get_buffer_line_o (cur_bp)
      if lastflag.set_uniarg then
        n = current_prefix_arg
      else
        n = minibuf_read_number (string.format ('Set fill_column to (default %d): ', o))
        if not n then -- cancelled
          return false
        elseif n == '' then
          n = o
        end
      end
    end

    if not n then
      return minibuf_error ('set_fill_column requires an explicit argument')
    end

    minibuf_write (string.format ('Fill column set to %d (was %d)', n,
                                  eval.get_variable ('fill_column')))
    eval.set_variable ('fill_column', tostring (n))
    return true
  end
)


Defun ("quoted_insert",
[[
Read next input character and insert it.
This is useful for inserting control characters.
]],
  true,
  function ()
    minibuf_write ('C-q-')
    insert_char (string.char (bit32.band (getkey_unfiltered (GETKEY_DEFAULT), 0xff)))
    minibuf_clear ()
  end
)


Defun ("fill_paragraph",
[[
Fill paragraph at or after point.
]],
  true,
  function ()
    local m = point_marker ()

    undo_start_sequence ()

    zz.forward_paragraph ()
    if is_empty_line () then
      previous_line ()
    end
    local m_end = point_marker ()

    zz.backward_paragraph ()
    if is_empty_line () then -- Move to next line if between two paragraphs.
      next_line ()
    end

    while buffer_end_of_line (cur_bp, get_buffer_pt (cur_bp)) < m_end.o do
    end_of_line ()
      delete_char ()
      zz.just_one_space ()
    end
    unchain_marker (m_end)

    end_of_line ()
    while get_goalc () > eval.get_variable ('fill_column') + 1 and fill_break_line () do end

    goto_offset (m.o)
    unchain_marker (m)

    undo_end_sequence ()
  end
)


Defun ("shell_command",
[[
Execute string @i{command} in inferior shell; display output, if any.
With prefix argument, insert the command's output at point.

Command is executed synchronously.  The output appears in the buffer
`*Shell Command Output*'.  If the output is short enough to display
in the echo area, it is shown there, but it is nonetheless available
in buffer `*Shell Command Output*' even though that buffer is not
automatically displayed.

The optional second argument @i{output_buffer}, if non-nil,
says to insert the output in some other buffer.
If @i{buffer} is a buffer or buffer name, put the output there,
If @i{buffer} is not a buffer and not nil,
insert output in current buffer.
In either case, the buffer is first erased, and the output is
inserted after point (leaving mark after it).
]],
  true,
  function (cmd, output)
    if not output then
      output = lastflag.set_uniarg
      -- Undo mangled interactive args when called from \C-u\M-!cmd\r.
      if output and cmd == tostring(current_prefix_arg) then cmd = nil end
    end
    if not cmd then
      cmd = minibuf_read_shell_command ()
    end

    if cmd then
      return pipe_command (cmd, output, false)
    end
    return true
  end
)


Defun ("shell_command_on_region",
[[
Execute string @i{command} in inferior shell with region as input.
Normally display output (if any) in temp buffer `*Shell Command Output*'
Prefix arg means replace the region with it.  Return the exit code of
@i{command}.

If the command generates output, the output may be displayed
in the echo area or in a buffer.
If the output is short enough to display in the echo area, it is shown
there.  Otherwise it is displayed in the buffer `*Shell Command Output*'.
The output is available in that buffer in both cases.

Optional fourth arg @i{output-buffer} specifies where to put the
command's output.  If the value is a buffer or buffer name,
put the output there.  If the value is nil, use the buffer
'*Shell Command Output*'.  Any other value, excluding nil,
means to insert the output in the current buffer.  In either case,
the output is inserted after point (leaving mark after it).

Option fifth arg @i{replace}, if non-nil, means to insert the
output in place of text from @i{start} to @i{finish}, putting point and mark
around it.
]],
  true,
  function (start, finish, cmd, output, replace)
    cmd    = cmd or minibuf_read_shell_command ()
    replace = replace or lastflag.set_uniarg
    output  = output or lastflag.set_uniarg

    if cmd then
      local rp = calculate_the_region ()

      if rp then
        return pipe_command (cmd, output, replace, tostring (get_region ()))
      end
      return false
    end
    return true
  end
)


Defun ("delete_region",
[[
Delete the text between point and mark.
]],
  true,
  function ()
    return delete_region (calculate_the_region ())
  end
)


Defun ("delete_blank_lines",
[[
On blank line, delete all surrounding blank lines, leaving just one.
On isolated blank line, delete that one.
On nonblank line, delete any immediately following blank lines.
]],
  true,
  function ()
    local m = point_marker ()
    local r = region_new (get_buffer_line_o (cur_bp), get_buffer_line_o (cur_bp))

    undo_start_sequence ()

    -- Find following blank lines.
    if move_line (1) and is_blank_line () then
      r.start = get_buffer_pt (cur_bp)
      repeat
        r.finish = buffer_next_line (cur_bp, get_buffer_pt (cur_bp))
      until not move_line (1) or not is_blank_line ()
    end
    goto_offset (m.o)

    -- If this line is blank, find any preceding blank lines.
    local singleblank = true
    if is_blank_line () then
      r.finish = math.max (r.finish, buffer_next_line (cur_bp, get_buffer_pt (cur_bp) or math.huge))
      repeat
        r.start = get_buffer_line_o (cur_bp)
      until not move_line (-1) or not is_blank_line ()

      goto_offset (m.o)
      if r.start ~= get_buffer_line_o (cur_bp) or r.finish > buffer_next_line (cur_bp, get_buffer_pt (cur_bp)) then
        singleblank = false
      end
      r.finish = math.min (r.finish, get_buffer_size (cur_bp))
    end

    -- If we are deleting to EOB, need to fudge extra line.
    local at_eob = r.finish == get_buffer_size (cur_bp) and r.start > 0
    if at_eob then
      r.start = r.start - #get_buffer_eol (cur_bp)
    end

    -- Delete any blank lines found.
    if r.start < r.finish then
      delete_region (r)
    end

    -- If we found more than one blank line, leave one.
    if not singleblank then
      if not at_eob then
        intercalate_newline ()
      else
        insert_string "\n"
      end
    end

    undo_end_sequence ()

    unchain_marker (m)
    deactivate_mark ()
  end
)


Defun ("downcase_word",
[[
Convert following word (or @i{arg} words) to lower case, moving over.
]],
  true,
  function (arg)
    return execute_with_uniarg (true, arg, function () return setcase_word ('lower') end)
  end
)


Defun ("upcase_word",
[[
Convert following word (or @i{arg} words) to upper case, moving over.
]],
  true,
  function (arg)
    return execute_with_uniarg (true, arg, function () return setcase_word ('upper') end)
  end
)


Defun ("capitalize_word",
[[
Capitalize the following word (or @i{arg} words), moving over.
This gives the word(s) a first character in upper case
and the rest lower case.
]],
  true,
  function (arg)
    return execute_with_uniarg (true, arg, function () return setcase_word ('capitalized') end)
  end
)


Defun ("upcase_region",
[[
Convert the region to upper case.
]],
  true,
  function ()
    return setcase_region (string.upper)
  end
)


Defun ("downcase_region",
[[
Convert the region to lower case.
]],
  true,
  function ()
    return setcase_region (string.lower)
  end
)


Defun ("transpose_chars",
[[
Interchange characters around point, moving forward one character.
With prefix arg ARG, effect is to take character before point
and drag it forward past ARG other characters (backward if ARG negative).
If no argument and at end of line, the previous two chars are exchanged.
]],
  true,
  function (n)
    return transpose (n or 1, move_char)
  end
)


Defun ("transpose_words",
[[
Interchange words around point, leaving point at end of them.
With prefix arg ARG, effect is to take word before or around point
and drag it forward past ARG other words (backward if ARG negative).
If ARG is zero, the words around or after point and around or after mark
are interchanged.
]],
  true,
  function (n)
    return transpose (n or 1, move_word)
  end
)


Defun ("transpose_sexps",
[[
Like @kbd{M-x transpose_words} but applies to sexps.
]],
  true,
  function (n)
    return transpose (n or 1, move_sexp)
  end
)


Defun ("transpose_lines",
[[
Exchange current line and previous line, leaving point after both.
With argument ARG, takes previous line and moves it past ARG lines.
With argument 0, interchanges line point is in with line mark is in.
]],
  true,
  function (n)
    return transpose (n or 1, move_line)
  end
)

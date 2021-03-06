-- Window handling functions
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
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.


local table = require "std.table"

-- The window list
windows = {}

-- Window table:
-- {
--   next: The next window in window list.
--   bp: The buffer displayed in window.
--   topdelta: The top line delta from point.
--   lastpointn: The last point line number.
--   start_column: The start column of the window (>0 if scrolled sideways).
--   saved_pt: The point line pointer, line number and offset
--             used to hold the point in non-current windows).
--   fwidth, fheight: The formal width and height of the window.
--   ewidth, eheight: The effective width and height of the window.
-- }

-- Set the current window and its buffer as the current buffer.
function set_current_window (wp)
  -- Save buffer's point in a new marker.
  if cur_wp.saved_pt then
    unchain_marker (cur_wp.saved_pt)
  end

  cur_wp.saved_pt = point_marker ()
  cur_wp = wp
  cur_bp = wp.bp

  -- Update the buffer point with the window's saved point marker.
  if cur_wp.saved_pt then
    goto_offset (cur_wp.saved_pt.o)
    unchain_marker (cur_wp.saved_pt)
    cur_wp.saved_pt = nil
  end
end

function find_window (name)
  for _, wp in ipairs (windows) do
    if wp.bp.name == name then
      return wp
    end
  end
end

function split_window ()
  -- Windows smaller than 4 lines cannot be split.
  if cur_wp.fheight < 4 then
    minibuf_error (string.format ("Window height %d too small (after splitting)", cur_wp.fheight))
    return false
  end

  local newwp = table.clone (cur_wp)
  newwp.fheight = math.floor (cur_wp.fheight / 2) + cur_wp.fheight % 2
  newwp.eheight = newwp.fheight - 1
  newwp.saved_pt = point_marker ()
  table.insert (windows, newwp)

  cur_wp.next = newwp
  cur_wp.fheight = math.floor (cur_wp.fheight / 2)
  cur_wp.eheight = cur_wp.fheight - 1
  if cur_wp.topdelta >= cur_wp.eheight then
    recenter (cur_wp)
  end

  return true
end

function window_o (wp)
  -- The current window uses the current buffer point; all other
  -- windows have a saved point, except that if a window has just been
  -- killed, it needs to use its new buffer's current point.

  assert (wp ~= nil)
  if wp == cur_wp then
    assert (wp.bp == cur_bp)
    assert (wp.saved_pt == nil)
    assert (cur_bp ~= nil)
    return get_buffer_pt (cur_bp)
  else
    if wp.saved_pt then
      return wp.saved_pt.o
    else
      return get_buffer_pt (wp.bp)
    end
  end
end

local function window_prev (this_wp)
  for i, wp in ipairs (windows) do
    if wp == this_wp then
      if i < #windows then
        return windows[i + 1]
      elseif i > 1 then
        return windows[i - 1]
      end
      break
    end
  end
  return windows[1]
end

function window_next (this_wp)
  for i, wp in ipairs (windows) do
    if wp == this_wp then
      if i > 1 then
        return windows[i - 1]
      elseif i < #windows then
        return windows[i + 1]
      end
      break
    end
  end
  return windows[1]
end

function delete_window (del_wp)
  for i = 1, #windows do
    local wp = windows[i]
    if wp == del_wp then
      local next_wp = window_prev (wp)
      table.remove (windows, i)
      if next_wp then
        next_wp.fheight = next_wp.fheight + del_wp.fheight
        next_wp.eheight = next_wp.eheight + del_wp.eheight + 1
        set_current_window (next_wp)
      end
      break
    end
  end

  if del_wp.saved_pt then
    unchain_marker (del_wp.saved_pt)
  end
end


function scroll_down ()
  if not window_top_visible (cur_wp) then
    return move_line (-cur_wp.eheight)
  end

  return minibuf_error ("Beginning of buffer")
end

function scroll_up ()
  if not window_bottom_visible (cur_wp) then
    return move_line (cur_wp.eheight)
  end

  return minibuf_error ("End of buffer")
end

-- Scroll completions up.
function completion_scroll_up ()
  local old_wp = cur_wp
  local wp = find_window ("*Completions*")
  assert (wp)
  set_current_window (wp)
  if not scroll_up () then
    goto_offset (1)
  end
  set_current_window (old_wp)

  term_redisplay ()
end

-- Scroll completions down.
function completion_scroll_down ()
  local old_wp = cur_wp

  local wp = find_window ("*Completions*")
  assert (wp)
  set_current_window (wp)
  if not scroll_down () then
    gotoeob ()
    window_resync (cur_wp)
  end
  set_current_window (old_wp)

  term_redisplay ()
end

function window_top_visible (wp)
  return offset_to_line (wp.bp, window_o (wp)) == wp.topdelta
end

function window_bottom_visible (wp)
  return wp.all_displayed
end

function popup_window ()
  if #windows == 1 then
    -- There is only one window on the screen, so split it.
    split_window ()
  end

  return window_next (cur_wp)
end

-- This function creates the scratch buffer and window when there are
-- no other windows (and possibly no other buffers).
function create_scratch_window ()
  local bp = create_scratch_buffer ()
  local w, h = term_width (), term_height ()
  local wp = {topdelta = 0, start_column = 0, lastpointn = 0}
  cur_wp = wp
  table.insert (windows, wp)
  wp.fwidth = w
  wp.ewidth = w
  -- Save space for minibuffer.
  wp.fheight = h - 1
  -- Save space for status line.
  wp.eheight = wp.fheight - 1
  cur_bp = bp
  wp.bp = cur_bp
end

function window_resync (wp)
  local n = offset_to_line (wp.bp, get_buffer_pt (wp.bp))
  local delta = n - wp.lastpointn

  if delta ~= 0 then
    if (delta > 0 and wp.topdelta + delta < wp.eheight) or (delta < 0 and wp.topdelta >= -delta) then
      wp.topdelta = wp.topdelta + delta
    elseif n > wp.eheight / 2 then
      wp.topdelta = math.floor (wp.eheight / 2)
    else
      wp.topdelta = n
    end
  end
  wp.lastpointn = n
end

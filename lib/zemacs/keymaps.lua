-- Zemacs key mappings
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

local posix = require "posix"
local list  = require "std.list"
local Tree  = require "std.tree"

local lisp  = require "zemacs.eval"

local intern_soft = lisp.intern_soft

root_bindings = Tree {}

function init_default_bindings ()
  -- Bind all printing keys to self-insert-command
  for i = 0, 0xff do
    if posix.isprint (string.char (i)) then
      root_bindings[{keycode (string.char (i))}] = intern_soft "self-insert-command"
    end
  end

  -- Bind special key names to self-insert-command
  list.map (function (e)
              root_bindings[{keycode (e)}] = intern_soft "self-insert-command"
            end,
            {"\\SPC", "\\TAB", "\\RET", "\\\\"})

  -- Search package.path for default-bindings.el, and load it.
  local ok, errmsg
  package.path:gsub ("[^;]+",
                    function (path)
                      if ok == nil then
                        path = path:gsub ("%?", "zemacs/default-bindings-el", 1)
                        ok, errmsg = lisp.eval_file (path)
                      end
                    end)
  return ok, errmsg
end

-- Copyright (c) 2012-2014 Free Software Foundation, Inc.
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


--[[--
 Syntax Highlighting.

 This module implements a two stage syntax highlighter:

 1. Search for matches against patterns in the current grammar from
   left to right, adding appropriate color pushes and pops at the
   match begin and end locations;
 2. Step through each character cell pushing any new colors for that
   cell onto a stack according to the instructions from step 1, then
   setting the cell to the color on the top of that stack (if any)
   and then finally popping any colors as instructed by step 1.

 @module zile.syntax
]]


local table = require "std.table"
local clone, empty = table.clone, table.empty

local Array       = require "zile.array"
local compile_rex = require "zile.bundle".compile_rex
local stack       = require "zile.lib".stack


-- Metamethods for syntax parsers state.
local metatable = {
  set = function (self, b, e, attr)
    if not attr then return nil end
    local attrs = self.syntax.attrs
    attrs:set (b, attr, e + 1 - b)
  end,
}


-- Syntax parser state.
local state = {
  -- Return a new parser state for the bp line containing offset o.
  -- @tparam table bp buffer table
  -- @int o offset into buffer bp
  -- @treturn table lexer for buffer line containing offset o
  new = function (bp, o)
    local n       = offset_to_line (bp, o)
    local linelen = buffer_line_len (bp, o)

    bp.syntax[n] = {
      -- Calculate the attributes for each cell of this line using a
      -- stack-machine with color push and pop operations.
      attrs = Array (linelen),

      -- parser state for the current line
      colors    = stack.new (),
      pats      = stack.new {bp.grammar.patterns},
    }

    local bol    = buffer_start_of_line (bp, o)
    local eol    = bol + linelen
    local region = get_buffer_region (bp, {start = bol, finish = eol})
    local lexer  = {
      s       = tostring (region),
      syntax  = bp.syntax[n],
    }

    return setmetatable (lexer, {__index = metatable})
  end,
}


--- Marshal 0-indexed buffer API into and out-of 1-indexed onig API.
-- @tparam userdata rex compiled rex_onig expression
-- @string s string to match against rex
-- @param i search start index into s
-- @treturn int offset of beginning of match
-- @treturn int oppset of end of match
local function rex_exec (rex, s, i)
  local b, e = rex:exec (s, i + 1)
  return b and (b - 1), e and (e - 1)
end


--- Find the leftmost matching expression.
-- @tparam table lexer syntax highlight matcher state
-- @int i search start index
-- @tparam table pats a list of patterns
-- @treturn int offset of beginning of match
-- @treturn int offset of end of match
-- @treturn pattern matching pattern
local function leftmost_match (lexer, i, pats)
  local s = lexer.s
  local b, e, caps, matched

  for _,v in ipairs (pats) do
    local match = v.rex or v.finish
    if match then
      local _b, _e, _caps = match:exec (s, i + 1)
      if _b and (not b or _b < b) then
        b, e, caps, matched = _b, _e, _caps, v
      end
    end
  end

  return b, e, caps, matched
end


--- Parse a string from left-to-right for matches against pats,
-- queueing color push and pop instructions as we go.
-- @tparam table lexer syntax highlight matcher stat
local function highlight (lexer)
  local colors = lexer.syntax.colors
  local pats   = lexer.syntax.pats
  local b, e, caps, matched

  local i = 0
  repeat
    b, e, caps, matched = leftmost_match (lexer, i, pats:top ())
    if b then
      local attrs, n = lexer.syntax.attrs, e + 1 - b

      if matched.colors then
        attrs:set (b, matched.colors, n)
      end
      if caps and matched.captures then
        for k, v in pairs (matched.captures) do
          -- onig marks zero length captures with first > last
          local first, last = caps[(k * 2) -1], caps[k * 2]

          if v and first and first < last then
            attrs:set (first, v, last + 1 - first)
          end
        end
      end

      i = e + 1

      -- If there are subexpressions, push those on the pattern stack.
      if matched.patterns then
        attrs:set (b, colors:push (matched.colors), n)
        pats:push (matched.patterns)
      end

      -- Pop completed subexpressions off the pattern stack
      if matched.finish then
        pats:pop ()
      end
    end
  until b == nil

  return lexer
end


--- Return attributes for the line in bp containing o.
-- @tparam buffer bp a buffer
-- @int o character offset into bp
-- @treturn int attributes
local function attrs (bp, o)
  if not bp.grammar then return nil end

  local lexer = highlight (state.new (bp, o))

  return lexer.syntax.attrs
end


--- @export
return {
  attrs = attrs,
}

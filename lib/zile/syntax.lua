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


-- Syntax parser state.
local state = {}


--- Queue a stack operation.
-- pushing or popping value v to the stack at offset o.
-- @param st
-- @param op
-- @param o
-- @param v
local function push_op (st, op, o, v)
  if not v then return nil end
  st[o] = st[o] or stack.new ()
  st[o]:push { [op] = v }
end


--- Return a new parser state for the buffer containing offset o.
function state.new (bp, o)
  local n = offset_to_line (bp, o)

  bp.syntax[n] = {
    -- Calculate the attributes for each cell of this line using a
    -- stack-machine with color push and pop operations.
    attrs = {},
    ops   = {},
  }

  local bol    = buffer_start_of_line (bp, o)
  local eol    = bol + buffer_line_len (bp, o)
  local region = get_buffer_region (bp, {start = bol, finish = eol})
  local lexer  = {
    grammar = bp.grammar,
    s       = tostring (region),
    syntax  = bp.syntax[n],
  }

  return lexer
end


--- Marshal 0-indexed buffer API into and out-of 1-indexed onig API.
-- @param rex
-- @param s
-- @param i
-- @treturn int offset of beginning of match
-- @treturn int oppset of end of match
local function rex_exec (rex, s, i)
  local b, e = rex:exec (s, i + 1)
  return b and (b - 1), e and (e - 1)
end


--- Find the leftmost matching expression.
-- @param lexer
-- @param i
-- @tparam table pats a list of patterns
-- @treturn int offset of beginning of match
-- @treturn int offset of end of match
-- @treturn pattern matching pattern
local function leftmost_match (lexer, i, pats)
  local s = lexer.s
  local b, e, p

  for _,v in ipairs (pats) do
    if v.match then
      local _b, _e = rex_exec (v.match, s, i)
      if _b and (not b or _b < b) then
        b, e, p = _b, _e, v
      end
    end
  end

  return b, e, p
end


--- Parse a string from left-to-right for matches against pats,
-- queueing color push and pop instructions as we go.
-- @param lexer
local function parse (lexer)
  local ops, pats = lexer.syntax.ops, lexer.grammar.patterns
  local b, e, p

  local i = 0
  repeat
    b, e, p = leftmost_match (lexer, i, pats)
    if b then
      push_op (ops, "push", b, p.attrs)
      push_op (ops, "pop",  e, p.attrs)

      i = e + 1
    end
  until b == nil
end


--- Highlight s according to queued color operations.
-- @param lexer
-- @return lexer
local function highlight (lexer)
  local highlight = stack.new ()

  parse (lexer)

  for i = 0, #lexer.s do
    -- set the color at this position before it can be popped.
    lexer.syntax.attrs[i] = highlight:top ()
    for _,v in ipairs (lexer.syntax.ops[i] or {}) do
      if v.push then
        highlight:push (v.push)
        -- but, override the initial color if a new one is pushed.
        lexer.syntax.attrs[i] = highlight:top ()

      elseif v.pop then
        assert (v.pop == highlight:top ())
        highlight:pop ()
      end
    end
  end

  return lexer
end


--- Return attributes for the line in bp containing o.
-- @tparam buffer bp a buffer
-- @int o character offset into bp
-- @treturn int attributes
local function syntax_attrs (bp, o)
  if not bp.grammar then return nil end

  local lexer = highlight (state.new (bp, o))

  return lexer.syntax.attrs
end


--- @export
return {
  syntax_attrs = syntax_attrs,
}

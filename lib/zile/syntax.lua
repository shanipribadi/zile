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

local compile_rex = require "zile.bundle".compile_rex


-- Metamethods for syntax parsers state.
local metatable = {
  set = function (self, b, e, attr)
    if not attr then return nil end
    local attrs = self.syntax.attrs
    attrs:set (b, attr, e + 1 - b)
  end,

  -- Return the current capture offsets.
  -- @treturn table topmost captures table
  -- @treturn string matching begin string
  top_caps = function (self)
    local top = self.syntax.caps:top ()
    if top then return top.caps, top.begin end
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

    bp.syntax.dirty = n + 1

    bp.syntax[n] = {
      -- Calculate the attributes for each cell of this line using a
      -- stack-machine with color push and pop operations.
      attrs = Array (linelen),

      -- parser state for the current line
      caps      = stack.new {},
      colors    = stack.new (),
      pats      = stack.new {bp.grammar.patterns},
    }

    local bol    = buffer_start_of_line (bp, o)
    local eol    = bol + linelen
    local region = get_buffer_region (bp, {start = bol, finish = eol})
    local lexer  = {
      n       = n,
      repo    = bp.grammar.repository,
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


--- Marshal 0-indexed offsets into 1-indexed string.sub API.
-- @string s subject
-- @int b substring beginning index
-- @int e substring end index
-- @treturn string substring of s from b to e
local function string_sub (s, b, e)
  return s:sub (b + 1, e + 1)
end


--- Expand back-references from pattern begin rule.
-- Used to replace unexpanded backrefs in pattern.end expressions
-- with captures from pattern.begin execution.
-- @tparam table lexer syntax highlight matcher state
-- @string match uncompiled match expression
local function expand (lexer, match)
  local begincaps, begin = lexer:top_caps ()

  if not match or not begincaps then return nil end

  local b, e = 0, 0
  repeat
    b, e = match:find ("\\.", e + 1)
    if e then
      local n = match:sub (e, e):match ("%d")
      if n then
        -- begincaps was adjusted to 0-based indexing by rex_exec.
        local replace = string_sub (begin, begincaps[(n * 2) - 1], begincaps[n * 2])
        match = match:sub (1, b - 1) .. replace .. match:sub (e + 1)
        e = b + #replace -- skip over replace contents
      end
    end
  until b == nil

  return compile_rex (match)
end


--- Find the leftmost matching expression.
-- @tparam table lexer syntax highlight matcher state
-- @int i search start index
-- @tparam table pats a list of patterns
-- @treturn int offset of beginning of match
-- @treturn int offset of end of match
-- @treturn pattern matching pattern
local function leftmost_match (lexer, i, pats)
  local repo, s = lexer.repo, lexer.s
  local b, e, caps, matched

  for _, v in ipairs (pats) do
    local _p  = v.include and repo[v.include] or v
    local rex = expand (lexer, _p.match) or _p.rex or _p.finish

    -- Match next candidate expression.
    local _b, _e, _caps
    if rex then
      _b, _e, _caps = rex:exec (s, i + 1)
    elseif _p.patterns then
      _b, _e, _caps, _p = leftmost_match (lexer, i, _p.patterns)
    end

    -- Save candidate if it matched earlier than previous candidate.
    if _b and (not b or _b < b) then
      b, e, caps, matched = _b, _e, _caps, _p
    end
  end

  return b, e, caps, matched
end


--- Parse a string from left-to-right for matches against pats,
-- queueing color push and pop instructions as we go.
-- @tparam table lexer syntax highlight matcher stat
local function highlight (lexer)
  local begincaps = lexer.syntax.caps
  local colors    = lexer.syntax.colors
  local pats      = lexer.syntax.pats
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
        begincaps:push {caps = caps, begin = lexer.s}
        pats:push (matched.patterns)
      end

      -- Pop completed subexpressions off the pattern stack
      if matched.finish then
        pats:pop ()
        begincaps:pop ()
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
  -- Can't highlight without any grammar!
  if not bp.grammar then return nil end

  local dirty = bp.syntax.dirty or 0
  local n     = offset_to_line (bp, o)

  -- If last calculations are still clean, return them.
  if n < dirty then return bp.syntax[n].attrs end

  -- otherwise, backtrack to the first dirty line...
  local ostart, lstart = o, n
  while lstart >= 0 and lstart > dirty do
    ostart = buffer_prev_line (bp, ostart)
    lstart = lstart - 1
  end

  -- ...and recalculate highlights right up to this line.
  local lexer
  repeat
    lexer  = highlight (state.new (bp, ostart))
    ostart = buffer_next_line (bp, ostart)
  until lexer.n >= n

  return lexer.syntax.attrs
end


--- @export
return {
  attrs = attrs,
}

-- Lua Grammar Bundle.
--
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

{
  name = 'Lua',
  fileTypes = { 'lua' },
  firstLineMatch =[[\A#!.*?\blua\b]],
  decreaseIndentPattern = [[^\s*(elseif|else|end|\})\s*$]],
  increaseIndentPattern = [[\b(else|elseif|(local\s+)?function|then|do|repeat)\b((?!end).)*$|\{\s*$]],
  foldingStartMarker = [[\b(function|local\s+function|then|do|repeat)\b|{[ \t]*$|\[\[]],
  foldingStopMarker = [=[\bend\b|^\s*}|\]\]]=],
  patterns = {
    {
      name = 'meta.function.lua',
      match = [[\b([a-zA-Z_.:]+[.:])?([a-zA-Z_]\w*)\s*(=)\s*(function)\s*(\()(self\b)?([^)]*)(\))]],
      captures = {
        [1] = { name = 'entity.name.function.scope.lua' },
        [2] = { name = 'entity.name.function.lua' },
        [3] = { name = 'keyword.operator.lua' },
        [4] = { name = 'keyword.control.lua' },
        [5] = { name = 'punctuation.definition.parameters.begin.lua' },
        [6] = { name = 'variable.language.self.lua' },
        [7] = { name = 'variable.parameter.function.lua' },
        [8] = { name = 'punctuation.definition.parameters.end.lua' },
      },
    },
    {
      name = 'meta.function.lua',
      match = [[\b(function)(?:\s+([a-zA-Z_.:]+[.:])?([a-zA-Z_]\w*)\s*)?(\()(self\b)?([^)]*)(\))]],
      captures = {
        [1] = { name = 'keyword.control.lua' },
        [2] = { name = 'entity.name.function.scope.lua' },
        [3] = { name = 'entity.name.function.lua' },
        [4] = { name = 'punctuation.definition.parameters.begin.lua' },
        [5] = { name = 'variable.language.self.lua' },
        [6] = { name = 'variable.parameter.function.lua' },
        [7] = { name = 'punctuation.definition.parameters.end.lua' },
      },
    },
    {
      name = 'constant.numeric.lua',
      match = [[(?<![\d.])\s0x[a-fA-F\d]+|\b\d+(\.\d+)?([eE]-?\d+)?|\.\d+([eE]-?\d+)?]],
    },
    {
      name = 'string.quoted.single.lua',
      begin = [[']],
      beginCaptures = {
        [0] = { name = 'punctuation.definition.string.begin.lua' },
      },
      ["end"] = [[']],
      endCaptures = {
        [0] = { name = 'punctuation.definition.string.end.lua' },
      },
      patterns = {
        { include = '#string_escaped_char' },
        { include = '#string_placeholder' },
      },
    },
    {
      name = 'string.quoted.double.lua',
      begin = [["]],
      beginCaptures = {
        [0] = { name = 'punctuation.definition.string.begin.lua' },
      },
      ["end"] = [["]],
      endCaptures = {
        [0] = { name = 'punctuation.definition.string.end.lua' },
      },
      patterns = {
        { include = '#string_escaped_char' },
        { include = '#string_placeholder' },
      },
    },
    {
      name = 'string.quoted.other.multiline.lua',
      begin = [[(?<!--)\[(=*)\[]],
      beginCaptures = {
        [0] = { name = 'punctuation.definition.string.begin.lua' },
      },
      ["end"] = [=[\]\1\]]=],
      endCaptures = {
        [0] = { name = 'punctuation.definition.string.end.lua' },
      },
    },
    {
      name = 'comment.block.lua',
      begin = [[--\[(=*)\[]],
      ["end"] = [=[\]\1\]]=],
      captures = {
        [0] = { name = 'punctuation.definition.comment.lua' },
      },
    },
    {
      begin = [[(^[ \t]+)?(?=--(?!\[\[))]],
      beginCaptures = {
        [1] = { name = 'punctuation.whitespace.comment.leading.lua' },
      },
      ["end"] = [[(?<=$)]],
      patterns = {
        {
          name = 'comment.line.double-dash.lua',
          begin = [[--]],
          beginCaptures = {
            [0] = { name = 'punctuation.definition.comment.lua' },
          },
          ["end"] = [[$]],
        },
      },
    },
    {
      name = 'storage.modifier.lua',
      match = [[\blocal\b]],
    },
    {
      name = 'keyword.control.lua',
      match = [[\b(break|do|else|for|if|elseif|return|then|repeat|while|until|end|function|in)\b]],
    },
    {
      name = 'constant.language.lua',
      match = [[(?<![^.]\.|:)\b(false|nil|true|_G|_VERSION|math\.(pi|huge))\b|(?<![.])\.{3}(?!\.)]],
    },
    {
      name = 'variable.language.self.lua',
      match = [[(?<![^.]\.|:)\b(self)\b]],
    },
    {
      name = 'support.function.lua',
      match = [[(?<![^.]\.|:)\b(assert|collectgarbage|dofile|error|getfenv|getmetatable|ipairs|loadfile|loadstring|module|next|pairs|pcall|print|rawequal|rawget|rawset|require|select|setfenv|setmetatable|tonumber|tostring|type|unpack|xpcall)\b(?=\s*(?:[({"']|\[\[))]],
    },
    {
      name = 'support.function.library.lua',
      match = [[(?<![^.]\.|:)\b(coroutine\.(create|resume|running|status|wrap|yield)|string\.(byte|char|dump|find|format|gmatch|gsub|len|lower|match|rep|reverse|sub|upper)|table\.(concat|insert|maxn|remove|sort)|math\.(abs|acos|asin|atan2?|ceil|cosh?|deg|exp|floor|fmod|frexp|ldexp|log|log10|max|min|modf|pow|rad|random|randomseed|sinh?|sqrt|tanh?)|io\.(close|flush|input|lines|open|output|popen|read|tmpfile|type|write)|os\.(clock|date|difftime|execute|exit|getenv|remove|rename|setlocale|time|tmpname)|package\.(cpath|loaded|loadlib|path|preload|seeall)|debug\.(debug|[gs]etfenv|[gs]ethook|getinfo|[gs]etlocal|[gs]etmetatable|getregistry|[gs]etupvalue|traceback))\b(?=\s*(?:[({"']|\[\[))]],
    },
    {
      name = 'keyword.operator.lua',
      match = [[\b(not|and|or)\b]],
    },
    {
      name = 'support.function.any-method.lua',
      match = [[\b([A-Za-z_]\w*)\b(?=\s*(?:[({"'']|\[\[))]],
    },
    {
      name = 'variable.other.lua',
      match = [[(?<=[^.]\.|:)\b([A-Za-z_]\w*)]],
    },
    {
      name = 'keyword.operator.lua',
      match = [[\+|-|%|#|\*|\/|\^|==?|~=|<=?|>=?|(?<!\.)\.{2}(?!\.)]],
    },
  },
  repository = {
    string_escaped_char = {
      name = 'constant.character.escape.lua',
      match = [[\\.]],
    },
    string_placeholder = {
      patterns = {
        {
          name = 'constant.other.placeholder.lua',
          match = [[(?x)%
                    (\d+\$)?                        # field (argument #)
                    [#0\- +']*                      # flags
                    ((-?\d+)|\*(-?\d+\$)?)?         # minimum field width
                    (\.((-?\d+)|\*(-?\d+\$)?)?)?    # precision
                    (h[hlv]?|ll?|v[hl]?|[jtzqL])?   # length modifier
                    [aAcdeEfgGioOqsuUxX%]           # conversion type
                  ]],
        },
        {
          name = 'constant.other.escape.lua',
          match = [=[%[acdglpsuwx\^\$\(\)\.\[\]\*\+\-\?]]=],
        },
        {
          name = 'invalid.illegal.placeholder.lua',
          match = [[%]],
        },
      },
    }
  },
  scopeName = 'source.lua',
}

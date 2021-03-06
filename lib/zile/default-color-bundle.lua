-- Copyright (c) 2011 Free Software Foundation, Inc.
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
 Zile monochrome terminal theme

 The theme file format is based on the format of [TextMate] themes,
 themselves based on the old Apple ASCII Property List file format,
 with some minimal changes to be valid Lua code.

 A TextMate theme file is not a valid Zile theme file, but can easily
 be converted with minimal editing to take into account the different
 conventions used by Lua. In addition to what is allowed in [TextMate]
 themes, any legal Lua constructs (such as comments!) can be used.

  [TextMate]: http://macromates.com

 ## Strings

 String data can be single or double quoted, and contain any valid Lua
 string.

     "This is a string."
     'A string with "quote" marks.'

 ## Arrays

 Arrays are ordered lists of elements, separated by commas and
 surrounded by curly braces.  Arrays can be nested, and trailing commas
 after the last element are harmless. Implemented as Lua lists.

     { "first", '2nd', "third", }
     { "outer", { "inner" }, "outer" }

 ## Dictionaries

 Unordered lists of named elements, where each element associates a key
 with a value. They look just like Arrays, only value is preceded by its
 name and an equals sign. Implemented as Lua tables.

     { foreground = "black", background = "white", }
     { colors = { "red", "green", "blue" } }


 # THEME FILE FORMAT

 The contents of the file must evaluate to a single Array, of Dictionaries
 as described above, optionally with an additional named Dictionary at the
 top level called 'settings'.

     {
       settings = {
         ...
       },
       {
         ...
       },
       ...
     }

 ## Top Level Settings

 The following keys are currently supported, others can be specified but
 will have no effect.

  1. background: the default background color for display buffer text.
  2. foreground: the default foreground color for display buffer text.
  3. selection:  the background color of the currently selected text.

 Each of these keys may be set to either:

  1. A string naming one of the preset colors, black, red, green,
     yellow, blue, magenta, cyan or white.

       selection = 'blue',

  2. A string beginning with a '#' followed by a six-hexadecimal-digit
     RGB color specification.

       foreground = '#FF33CC',

 Many terminal emulators cannot display hexadecimal colors, and are
 limited to just the 8 preset color names, possibly showing a variation
 on each of those when the bold attribute is also present.  For your
 theme to work on these terminals, you should avoid using hexadecimal
 colors altogether.

 ## All Other Elements

 The remaining un-named elements in the top level list must contain a
 Dictionary with the keys 'scope' and 'settings'.  [TextMate] themes
 also have a 'name' element, which does nothing but is harmless to Zile
 if left in a ported theme file.

 The value for 'scope' is a string of dot delimited words which are
 matched against the syntax scope names matched by the language grammars,
 which you can find examples of by looking in existing themes or into
 the grammar definition files.

     scope = 'keyword',

 The value for 'settings' can be specified as a simple color string in
 the same way as the top level settings values, in which case that color
 is applied as the foreground color for text that matches the associated
 named scope. Otherwise it is another Dictionary containing values for
 one or more of the keys 'background', 'foreground' and 'fontStyle'.

     settings = { background = 'black', foreground = 'white' },

 The values for 'background' and 'foreground' are color strings as for
 the top level settings values. If 'fontStyle' is also given, then its
 value is a space delimited string of words from the following list:

     normal, standout, underline, reverse, blink, dim, bold, protect,
     and invisible.

 Not all of these values can be displayed on all terminals, though at
 least 'normal', 'underline', 'reverse' and 'bold' are almost universal.

 @module zile.default-color
]]

{
  settings = {
    selection = { fontStyle = 'reverse' },
  },
  {
    scope = 'modeline',
    settings = { fontStyle = 'reverse' },
  },
}

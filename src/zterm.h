/* Term interface
   Copyright (c) 2004 Reuben Thomas.
   All rights reserved.

   This file is part of Zile.

   Zile is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2, or (at your option) any later
   version.

   Zile is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with Zile; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

/*      $Id: zterm.h,v 1.2 2004/07/08 23:39:25 rrt Exp $        */

#ifndef ZTERM_H
#define ZTERM_H

/* This header file is just a shell that loads the configured
   implementation. */

#include "config.h"

#ifdef CURSES
#include "term_ncurses.h"
#else
#include "term_termcap.h"
#endif

#endif /* !ZTERM_H */

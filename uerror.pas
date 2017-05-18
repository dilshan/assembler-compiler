{ Experimental CPU (ECPU) - Error definitions.

  Copyright (c) 2017 Dilshan R Jayakody [jayakody2000lk@gmail.com]

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit uError;

{$mode objfpc}{$H+}

interface

// Compiler error messages.
const
  ERR_DUPLICATE_LABEL_NAME = 1;         // Duplicate lablel name "NAME".
  ERR_UNKNOWN_OPERAND = 2;              // Unsupported operand.
  ERR_TOO_MANY_ARGUMENTS = 3;           // Unexpected argument "ARG".
  ERR_OPERAND_MISSING = 4;              // Missing operands.
  ERR_UNSUPPORTD_OPERAND = 5;           // Unsupported operand(s).
  ERR_OFFSET_OUTOFBOUND = 6;            // Offset out of bounds.
  ERR_OUTOF_RANGE = 7;                  // Value out of range.
  ERR_TOO_MANY_OPERANDS = 8;            // Too many operand(s).
  ERR_UNKNOWN_LABEL = 9;                // Unknown label "NAME".
  ERR_LABEL_RANGE_OVERFLOW = 10;        // Label address range exceeds 8 bit limit.
  ERR_OUTPUT_LIMIT = 11;                // Output code limit reached.
  ERR_SYNTAX = 12;                      // Syntax error.
  ERR_INVALID_LABEL = 13;               // Invalid label name "NAME".

  // Runtime error messages.
const
  RUNERR_ACCESS_VIOLATION = 101;        // Access violation at "ADDRESS".
  RUNERR_INVALID_REG = 102;             // Invalid register "REG".

implementation

end.


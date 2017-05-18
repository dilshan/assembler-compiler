{ Experimental CPU (ECPU) - Compiler op-code table.

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

unit uOpcode;

{$mode objfpc}{$H+}

interface

const
  // NONE
  NONE = 0;

  // MOV
  MOV_REG_TO_REG = 1;
  MOV_ADDRESS_TO_REG = 2;
  MOV_REGADDRESS_TO_REG = 3;
  MOV_REG_TO_ADDRESS = 4;
  MOV_REG_TO_REGADDRESS = 5;
  MOV_NUMBER_TO_REG = 6;
  MOV_NUMBER_TO_ADDRESS = 7;
  MOV_NUMBER_TO_REGADDRESS = 8;

  // ADD
  ADD_REG_TO_REG = 10;
  ADD_REGADDRESS_TO_REG = 11;
  ADD_ADDRESS_TO_REG = 12;
  ADD_NUMBER_TO_REG = 13;

  // SUB
  SUB_REG_FROM_REG = 14;
  SUB_REGADDRESS_FROM_REG = 15;
  SUB_ADDRESS_FROM_REG = 16;
  SUB_NUMBER_FROM_REG = 17;

  // INC
  INC_REG = 18;

  // DEC
  DEC_REG = 19;

  // CMP
  CMP_REG_WITH_REG = 20;
  CMP_REGADDRESS_WITH_REG = 21;
  CMP_ADDRESS_WITH_REG = 22;
  CMP_NUMBER_WITH_REG = 23;

  // JMP
  JMP_REGADDRESS = 30;
  JMP_ADDRESS = 31;

  // JC
  JC_REGADDRESS = 32;
  JC_ADDRESS = 33;

  // JNC
  JNC_REGADDRESS = 34;
  JNC_ADDRESS = 35;

  // JZ
  JZ_REGADDRESS = 36;
  JZ_ADDRESS = 37;

  // JNZ
  JNZ_REGADDRESS = 38;
  JNZ_ADDRESS = 39;

  // JA
  JA_REGADDRESS = 40;
  JA_ADDRESS =  41;

  // JNA
  JNA_REGADDRESS = 42;
  JNA_ADDRESS = 43;

  // PUSH
  PUSH_REG = 50;
  PUSH_REGADDRESS = 51;
  PUSH_ADDRESS = 52;
  PUSH_NUMBER = 53;

  // POP
  POP_REG = 54;

  // CALL
  CALL_REGADDRESS = 55;
  CALL_ADDRESS = 56;

  // RET
  RET =  57;

  // MUL
  MUL_REG = 60;
  MUL_REGADDRESS = 61;
  MUL_ADDRESS = 62;
  MUL_NUMBER = 63;

  // DIV
  DIV_REG = 64;
  DIV_REGADDRESS = 65;
  DIV_ADDRESS = 66;
  DIV_NUMBER = 67;

  // AND
  AND_REG_WITH_REG = 70;
  AND_REGADDRESS_WITH_REG = 71;
  AND_ADDRESS_WITH_REG = 72;
  AND_NUMBER_WITH_REG = 73;

  // OR
  OR_REG_WITH_REG = 74;
  OR_REGADDRESS_WITH_REG = 75;
  OR_ADDRESS_WITH_REG = 76;
  OR_NUMBER_WITH_REG = 77;

  // XOR
  XOR_REG_WITH_REG = 78;
  XOR_REGADDRESS_WITH_REG = 79;
  XOR_ADDRESS_WITH_REG = 80;
  XOR_NUMBER_WITH_REG = 81;

  // NOT
  NOT_REG = 82;

  // SHL
  SHL_REG_WITH_REG = 90;
  SHL_REGADDRESS_WITH_REG = 91;
  SHL_ADDRESS_WITH_REG = 92;
  SHL_NUMBER_WITH_REG = 93;

  // SHR
  SHR_REG_WITH_REG = 94;
  SHR_REGADDRESS_WITH_REG = 95;
  SHR_ADDRESS_WITH_REG = 96;
  SHR_NUMBER_WITH_REG = 97;

implementation

end.


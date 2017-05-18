{ Experimental CPU (ECPU) - Base compiler unit.

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

unit uCompiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uError, uOpCode;

type
  TTokenType = (tkUnknown, tkReg, tkRegAddr, tkAddr, tkNumber);

  TErrorEventParam = procedure(errorNumber: Integer; errorStr: String) of object;
  TCompileFinishParam = procedure(outputSize: Integer) of object;

  PLabelEntry = ^TLabelEntry;
  TLabelEntry = record
    LabelName: String;
    LabelAddr: Integer;
  end;

  PLabelMapEntry = ^TLabelMapEntry;
  TLabelMapEntry = record
    LabelName: String;
    CodeAddr: Integer;
    SourceLine: Integer;
  end;

  TRegData = record
    RegName: string;
    RegAddr: Integer;
  end;

  TDataBuffer = array of byte;

  TCompiler = class(TObject)
  private
    compileLine: Integer;
    sourceScript: TStringList;
    labelTable: TList;
    labelMap: TList;
    outputStream: TMemoryStream;
    fOutputPath: String;
    fOutputBuffer: TDataBuffer;
    procedure Compile();
    procedure TokenParser(inTokenData: TStrings; lineNumber: Integer);
    procedure RaiseError(ErrorCode: Integer; lineNumber: Integer; errParam: String = '');
    function AddLabel(inLabel: String; lblAddr: Integer; lineNumber: Integer): Boolean;
    function GetCurrentAddress(): Word;
    procedure PushInstruction(inData: array of byte);
    procedure WriteOutputFile();
    procedure AddLabelMapEntry(inLabel: String; codeAddr: Integer; srcLineNumber: Integer);
    procedure AssignLabelAddress();
    function IsRegister(inStr: String): Boolean;
  public
    OnError: TErrorEventParam;
    OnCompileFinish: TCompileFinishParam;
    property OutputFileName: string read fOutputPath write fOutputPath;
    property OutputBuffer: TDataBuffer read fOutputBuffer write fOutputBuffer;
    procedure ParseScript(inScript: TStrings);
  end;

const
  BYTE_LIMITER = 256;

implementation

// Raise source code compiliation errors.
procedure TCompiler.RaiseError(ErrorCode: Integer; lineNumber: Integer; errParam: String = '');
var
  errStr: String;
begin
  if(Assigned(OnError)) then
  begin
    case(ErrorCode)of
      ERR_DUPLICATE_LABEL_NAME: errStr := 'Duplicate lablel name ' + errParam + '.';
      ERR_UNKNOWN_OPERAND: errStr := 'Unsupported operand.';
      ERR_TOO_MANY_ARGUMENTS: errStr := 'Unexpected argument "' + errParam + '".';
      ERR_OPERAND_MISSING: errStr := 'Missing operands.';
      ERR_UNSUPPORTD_OPERAND: errStr := 'Unsupported operand(s).';
      ERR_OFFSET_OUTOFBOUND: errStr := 'Offset out of bounds.';
      ERR_OUTOF_RANGE: errStr := 'Value out of range.';
      ERR_TOO_MANY_OPERANDS: errStr := 'Too many operand(s).';
      ERR_UNKNOWN_LABEL: errStr := 'Unknown label ' + errParam + '.';
      ERR_LABEL_RANGE_OVERFLOW: errStr := 'Label address range exceeds 8 bit limit.';
      ERR_OUTPUT_LIMIT: errStr := 'Output code limit reached.';
      ERR_SYNTAX: errStr := 'Syntax error.';
      ERR_INVALID_LABEL: errStr := 'Invalid label name "' + errParam + '".';
    end;

    if(lineNumber >= 0) then
      OnError(ErrorCode, '[' + IntToStr(lineNumber + 1) + '] ' + errStr)
    else
      OnError(ErrorCode, errStr);
  end;
end;

// Push specified instruction set to output buffer.
procedure TCompiler.PushInstruction(inData: array of byte);
begin
  if(Assigned(outputStream)) then
    outputStream.WriteBuffer(inData, Length(inData));
end;

// Return current address of instruction buffer.
function TCompiler.GetCurrentAddress(): Word;
begin
  if(Assigned(outputStream)) then
    result := outputStream.Size
  else
    result := 0;
end;

// Parse specified source code and generate output.
procedure TCompiler.ParseScript(inScript: TStrings);
var
  tempPos: Integer;
  tempRec: PLabelEntry;
begin
  if (Assigned(inScript)) then
  begin
    // Release previous source file references.
    if (Assigned(sourceScript)) then
      FreeAndNil(sourceScript);

    // Create new source file container and start compilation.
    sourceScript := TStringList.Create;
    sourceScript.AddStrings(inScript);

    // Clean existing label table and create new table.
    if(Assigned(labelTable)) then
    begin
      for tempPos := 0 to (labelTable.Count - 1) do
      begin
        tempRec := labelTable.Items[tempPos];
        Dispose(tempRec);
      end;
      FreeAndNil(labelTable);
    end;

    // Clean existing label map and create new map.
    if(Assigned(labelMap)) then
    begin
      for tempPos := 0 to (labelMap.Count - 1) do
      begin
        tempRec := labelMap.Items[tempPos];
        Dispose(tempRec);
      end;
      FreeAndNil(labelMap);
    end;

    if(Assigned(outputStream)) then
      FreeAndNil(outputStream);

    labelTable := TList.Create;
    labelMap := TList.Create;

    outputStream := TMemoryStream.Create;

    // Start code compiliation.
    compileLine := 0;
    if(sourceScript.Count > 0)then
    begin
      Compile();
    end;

    // Raise compilation finish event.
    if(Assigned(OnCompileFinish)) then
      OnCompileFinish(outputStream.Size);

    SetLength(fOutputBuffer, outputStream.Size);
    outputStream.Position := 0;
    outputStream.ReadBuffer(fOutputBuffer[0], outputStream.Size);

    FreeAndNil(outputStream);
  end;
end;

// Add new label to label buffer.
function TCompiler.AddLabel(inLabel: String; lblAddr: Integer; lineNumber: Integer): Boolean;
var
  labelRec: PLabelEntry;
  labelPos: Integer;
begin
  // Check for duplicate label names.
  for labelPos := 0 to (labelTable.Count - 1) do
  begin
    if(PLabelEntry(labelTable.Items[labelPos])^.LabelName = inLabel) then
    begin
      RaiseError(ERR_DUPLICATE_LABEL_NAME, lineNumber, inLabel);
      Result := false;
      exit;
    end;
  end;

  // Check for label address range is within 8bit limit.
  if(lblAddr > (BYTE_LIMITER - 1)) then
  begin
    RaiseError(ERR_LABEL_RANGE_OVERFLOW, lineNumber);
    exit;
  end;

  // Check label name is valid. System register names are not allowd as lable.
  if(IsRegister(inLabel)) then
  begin
    RaiseError(ERR_INVALID_LABEL, lineNumber, inLabel);
    exit;
  end;

  // Add new entry to label table.
  New(labelRec);
  labelRec^.LabelName := inLabel;
  labelRec^.LabelAddr := lblAddr;
  labelTable.Add(labelRec);
  Result:= true;
end;

// Add new entry into label map.
procedure TCompiler.AddLabelMapEntry(inLabel: String; codeAddr: Integer; srcLineNumber: Integer);
var
  mapRec: PLabelMapEntry;
  memAddr: Integer;
begin
  New(mapRec);
  mapRec^.LabelName := inLabel;
  mapRec^.SourceLine := srcLineNumber;
  // binary stream location to modify later stages of compilation.
  memAddr := GetCurrentAddress + 1 + codeAddr;
  mapRec^.CodeAddr := memAddr;
  labelMap.Add(mapRec);
end;

// Check specified string is a register.
function TCompiler.IsRegister(inStr: String): Boolean;
begin
  inStr := UpperCase(inStr);
  result := ((inStr = 'A') or (inStr = 'B') or (inStr = 'C') or (inStr = 'D') or (inStr = 'SP'));
end;

// Function to perform code tokenization.
procedure TCompiler.TokenParser(inTokenData: TStrings; lineNumber: Integer);
var
  tokenPointer: Integer;
  tempStrPos, tempStrChr: Integer;
  tempStr: String;
  param1, param2: String;
  param1Type, param2Type: TTokenType;
  param1Val, param2Val: integer;
  tempRegData: TRegData;

  // Check specified token is match with given test signature.
  function IsTokenIsInstruction(testToken: String; testInst: String): Boolean;
  begin
    Result := (UpperCase(testToken) = UpperCase(testInst));
  end;

  // Check next available instruction is match with given test signature.
  function TestNextToken(offset: Integer; testToken: string): Boolean;
  begin
    result := false;

    if((tokenPointer + offset) <  inTokenData.Count) then
    begin
      result := (UpperCase(inTokenData[tokenPointer + offset]) = UpperCase(testToken));
    end;
  end;

  // Check next available token is string.
  function IsNextTokenIsString(offset: Integer): Boolean;
  var
    extStr: String;
  begin
    result := false;
    if((tokenPointer + offset) <  inTokenData.Count) then
    begin
      extStr := inTokenData[tokenPointer + offset];
      if(Length(extStr) > 2) then
      begin
        result := ((extStr[1] = '"') and (extStr[Length(extStr)] = '"'));
      end;
    end;
  end;

  // Check specified string is a number.
  function IsNumber(inVal: String): Boolean;
  var
    testPos: Integer;
    testChar: Integer;
  begin
    result := true;

    for testPos := 1 to Length(inVal) do
    begin
      testChar := Ord(inVal[testPos]);
      if (not((testChar >= 48) and (testChar <= 57))) then
      begin
        result := false;
        break;
      end;
    end;
  end;

  // Check next available token is number.
  function IsNextTokenIsNumber(offset: Integer): Boolean;
  var
    testStr: String;
  begin
    result := false;

    if((tokenPointer + offset) <  inTokenData.Count) then
    begin
      testStr := Trim(inTokenData[tokenPointer + offset]);
      if(testStr <> '') then
      begin
        result := IsNumber(testStr);
      end;
    end;
  end;

  // Check for any extra tokens are available after current token pointer.
  function NoExtraTokens(): Boolean;
  begin
    result := ((tokenPointer + 1) >= inTokenData.Count);
  end;

  // Get base register values.
  function GetBaseRegAddress(inReg: string): SmallInt;
  begin
    inReg := UpperCase(inReg);
    result := -1;

    case inReg of
      'A' : result := 0;
      'B' : result := 1;
      'C' : result := 2;
      'D' : result := 3;
      'SP' : result := 4;
    end;
  end;

   // Check specified string is a register with offset.
  function IsRegsiterWithOffset(inStr: String): Boolean;
  var
    symbPlus, symbMinus: Boolean;
    splitChar: string;
    splitPos: Integer;
    regName, regOffset: string;

  begin
    result := false;
    inStr := UpperCase(inStr);
    symbPlus := (AnsiPos('+', inStr) > 0);
    symbMinus := (AnsiPos('-', inStr) > 0);

    if(symbPlus or symbMinus) then
    begin
      if(symbPlus) then
        splitChar := '+'
      else
        splitChar := '-';

      splitPos := AnsiPos(splitChar, inStr);
      regName := Trim(Copy(inStr, 1, splitPos - 1));
      regOffset := Trim(Copy(inStr, splitPos + 1, (Length(inStr) - splitPos)));

      result := IsRegister(regName) and IsNumber(regOffset);
    end;
  end;

  // Check specified token and return token type.
  function GetTokenType(inToken: String): TTokenType;
  var
    tokenData: string;
  begin
    result := tkUnknown;

    if(trim(inToken) <> '') then
    begin
      if(inToken[1] = '[') then
      begin
        // processed for [number] or [register]
        tokenData := Copy(inToken, 2, Length(inToken) - 2);
        if(IsNumber(tokenData)) then
          result := tkAddr
        else if(IsRegister(tokenData) or IsRegsiterWithOffset(tokenData)) then
          result := tkRegAddr;
      end
      else if(IsNumber(inToken)) then
        result := tkNumber    // processed as number.
      else if(IsRegister(inToken)) then
        result := tkReg      // processed as register.
      else
        result := tkNumber;   // unknown token, is going to be a label?
    end;
  end;

  // Extract registry information from supplied string.
  function GetRegistry(lineNumber: Integer; inStr: String): TRegData;
  var
    symbPlus, symbMinus: Boolean;
    splitChar, stripInStr: string;
    splitPos: Integer;
    regName, regOffset: string;
    offset: Integer;

  begin
    result.RegName := '';
    result.RegAddr := 0;

    if((Length(inStr) >= 3) and (inStr[1] = '[')) then
    begin
      if (IsRegsiterWithOffset(Copy(inStr, 2, Length(inStr) - 2))) then
      begin
        stripInStr := Uppercase(Copy(inStr, 2, Length(inStr) - 2));
        symbPlus := (AnsiPos('+', stripInStr) > 0);
        symbMinus := (AnsiPos('-', stripInStr) > 0);

        if(symbPlus or symbMinus) then
        begin
          if(symbPlus) then
            splitChar := '+'
          else
            splitChar := '-';

          splitPos := AnsiPos(splitChar, stripInStr);
          regName := Trim(Copy(stripInStr, 1, splitPos - 1));
          regOffset := Trim(Copy(stripInStr, splitPos + 1, (Length(stripInStr) - splitPos)));

          if(symbPlus) then
            offset := 1
          else
            offset := -1;

          // Check for offset out of bonuds.
          offset := offset * StrToIntDef(regOffset, 0);
          if((offset < -16) or ( offset > 15)) then
          begin
            RaiseError(ERR_OFFSET_OUTOFBOUND, lineNumber);
            exit;
          end;

          // two's complement with 5bits.
          if(offset < 0) then
          begin
            offset := offset + 32;
          end;

          result.RegName := regName;
          result.RegAddr := offset * 8 + GetBaseRegAddress(regName);
        end;
      end
      else
      begin
        stripInStr := Uppercase(Copy(inStr, 2, Length(inStr) - 2));
        if(stripInStr <> '') then
        begin
          result.RegName := stripInStr;
          result.RegAddr := GetBaseRegAddress(stripInStr);
        end
        else
        begin
          RaiseError(ERR_UNKNOWN_OPERAND, lineNumber);
          exit;
        end;
      end;
    end
    else
    begin
      result.RegName := inStr;
      result.RegAddr := GetBaseRegAddress(inStr);
    end;
  end;

  // Convert specified string to number (byte).
  function ParseNumber(inStr: String): Integer;
  var
    tempNumber: Integer;
  begin
    result := BYTE_LIMITER;
    tempNumber := StrToIntDef(inStr, BYTE_LIMITER);
    if((tempNumber >= BYTE_LIMITER) or (tempNumber < 0)) then
    begin
      RaiseError(ERR_OUTOF_RANGE, lineNumber);
      exit;
    end;
    result := tempNumber;
  end;

  // Process instruction operands and generate instruction parameters.
  function GenerateParamCode(paramType: TTokenType; param: String; var paramVal: integer; byteOffset: integer): Boolean;
  begin
    result := false;

    // Generator for registry and/or registry base addresses.
    if((paramType = tkRegAddr) or (paramType = tkReg)) then
    begin
      tempRegData := GetRegistry(lineNumber, param);
      paramVal := tempRegData.RegAddr;

      if(tempRegData.RegName = '') then
        exit;
    end;

    // Generator for constants (numbers).
    if(paramType = tkNumber) then
    begin
      if(IsNumber(param)) then
      begin
        paramVal := ParseNumber(param);
        if(paramVal >= BYTE_LIMITER) then
          exit;
      end
      else
      begin
        // This is going to be a label, add it to label updater list to update at last phase of compilation.
        AddLabelMapEntry(param, byteOffset, lineNumber)
      end;
    end;

    // Generator for memory address.
    if(paramType = tkAddr) then
    begin
      tempStr := Trim(Copy(param, 2, Length(param) - 2));
      if(tempStr <> '') then
      begin
        paramVal := ParseNumber(tempStr);
        if(paramVal >= BYTE_LIMITER) then
          exit;
      end;
    end;

    result := true;
  end;

  function ExtractParamCode(var param: String; var paramType: TTokenType): Boolean;
  begin
    result := false;
    inc(tokenPointer);

    // Process operand.
    if(tokenPointer <  inTokenData.Count) then
    begin
      param := inTokenData[tokenPointer];
      paramType := GetTokenType(param);
    end
    else
    begin
      RaiseError(ERR_OPERAND_MISSING, lineNumber);
      exit;
    end;

    result := true;
  end;

begin
  tokenPointer := 0;

  param1Val := 0;
  param2Val := 0;

  param1Type := tkUnknown;
  param2Type := tkUnknown;

  if(inTokenData.Count > 0)then
  begin

    // Check for line comments and ignore next available tokens.
    if(inTokenData[tokenPointer] = ';') then
    begin
      exit;
    end;

    // Check for line label. Syntax: LABEL-NAME:
    if((inTokenData.Count > 1) and ((tokenPointer + 1) <= inTokenData.Count) and (inTokenData[tokenPointer] <> '') and (inTokenData[tokenPointer + 1] = ':')) then
    begin
      if(not AddLabel(inTokenData[tokenPointer], GetCurrentAddress, lineNumber)) then begin
        exit;
      end;
      inc(tokenPointer, 2);

      // Check for any other tokens, if not stop the scanning and return (to next line).
      if(NoExtraTokens()) then
        exit;
    end;

    if(IsTokenIsInstruction(inTokenData[tokenPointer], 'DB')) then
    begin
      // Instruction: DB
      if(IsNextTokenIsNumber(1)) then
      begin
        // DB with single character operand.
        PushInstruction([StrToInt(inTokenData[tokenPointer + 1])]);
        inc(tokenPointer, 2);
      end else if(IsNextTokenIsString(1)) then begin
        // DB with string operand.
        tempStr := Copy(inTokenData[tokenPointer + 1], 2, Length(inTokenData[tokenPointer + 1]) - 2);
        for tempStrPos := 1 to Length(tempStr) do
        begin
          tempStrChr := Ord(tempStr[tempStrPos]);
          PushInstruction([tempStrChr]);
        end;
        inc(tokenPointer, 2);
      end else
      begin
        RaiseError(ERR_UNKNOWN_OPERAND, lineNumber);
        exit;
      end;
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'HLT')) then
    begin
      // Instruction: HLT
      if(NoExtraTokens()) then
      begin
        PushInstruction([NONE]);
        inc(tokenPointer, 1);
      end else
      begin
        RaiseError(ERR_TOO_MANY_ARGUMENTS, lineNumber, inTokenData[tokenPointer + 1]);
        exit;
      end;
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'MOV')) then
    begin
      // Instruction: MOV
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      if(not ExtractParamCode(param2, param2Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(not GenerateParamCode(param2Type, param2, param2Val, 2)) then
        exit;

      if((param1Type = tkReg) and (param2Type = tkReg)) then
        PushInstruction([MOV_REG_TO_REG])
      else if((param1Type = tkReg) and (param2Type = tkAddr)) then
        PushInstruction([MOV_ADDRESS_TO_REG])
      else if((param1Type = tkReg) and (param2Type = tkRegAddr)) then
        PushInstruction([MOV_REGADDRESS_TO_REG])
      else if((param1Type = tkAddr) and (param2Type = tkReg)) then
        PushInstruction([MOV_REG_TO_ADDRESS])
      else if((param1Type = tkRegAddr) and (param2Type = tkReg)) then
        PushInstruction([MOV_REG_TO_REGADDRESS])
      else if((param1Type = tkReg) and (param2Type = tkNumber)) then
        PushInstruction([MOV_NUMBER_TO_REG])
      else if((param1Type = tkAddr) and (param2Type = tkNumber)) then
        PushInstruction([MOV_NUMBER_TO_ADDRESS])
      else if((param1Type = tkRegAddr) and (param2Type = tkNumber)) then
        PushInstruction([MOV_NUMBER_TO_REGADDRESS])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val, param2Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'ADD')) then
    begin
      // Instruction: ADD
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      if(not ExtractParamCode(param2, param2Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(not GenerateParamCode(param2Type, param2, param2Val, 2)) then
        exit;

      if((param1Type = tkReg) and (param2Type = tkReg)) then
        PushInstruction([ADD_REG_TO_REG])
      else if((param1Type = tkReg) and (param2Type = tkRegAddr)) then
        PushInstruction([ADD_REGADDRESS_TO_REG])
      else if((param1Type = tkReg) and (param2Type = tkAddr)) then
        PushInstruction([ADD_ADDRESS_TO_REG])
      else if((param1Type = tkReg) and (param2Type = tkNumber)) then
        PushInstruction([ADD_NUMBER_TO_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val, param2Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'SUB')) then
    begin
      // Instruction: SUB
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      if(not ExtractParamCode(param2, param2Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(not GenerateParamCode(param2Type, param2, param2Val, 2)) then
        exit;

      if((param1Type = tkReg) and (param2Type = tkReg)) then
        PushInstruction([SUB_REG_FROM_REG])
      else if((param1Type = tkReg) and (param2Type = tkRegAddr)) then
        PushInstruction([SUB_REGADDRESS_FROM_REG])
      else if((param1Type = tkReg) and (param2Type = tkAddr)) then
        PushInstruction([SUB_ADDRESS_FROM_REG])
      else if((param1Type = tkReg) and (param2Type = tkNumber)) then
        PushInstruction([SUB_NUMBER_FROM_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val, param2Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'INC')) then
    begin
      // Instruction: INC
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([INC_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'DEC')) then
    begin
      // Instruction: DEC
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([DEC_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'CMP')) then
    begin
      // Instruction: CMP
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      if(not ExtractParamCode(param2, param2Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(not GenerateParamCode(param2Type, param2, param2Val, 2)) then
        exit;

      if((param1Type = tkReg) and (param2Type = tkReg)) then
        PushInstruction([CMP_REG_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkRegAddr)) then
        PushInstruction([CMP_REGADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkAddr)) then
        PushInstruction([CMP_ADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkNumber)) then
        PushInstruction([CMP_NUMBER_WITH_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val, param2Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'JMP')) then
    begin
      // Instruction: JMP
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([JMP_REGADDRESS])
      else if(param1Type = tkNumber) then
        PushInstruction([JMP_ADDRESS])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'JC') or IsTokenIsInstruction(inTokenData[tokenPointer], 'JB') or IsTokenIsInstruction(inTokenData[tokenPointer], 'JNAE')) then
    begin
      // Instruction: JC, JB, JNAE
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([JC_REGADDRESS])
      else if(param1Type = tkNumber) then
        PushInstruction([JC_ADDRESS])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'JNC') or IsTokenIsInstruction(inTokenData[tokenPointer], 'JNB') or IsTokenIsInstruction(inTokenData[tokenPointer], 'JAE')) then
    begin
      // Instruction: JNC, JNB, JAE
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([JNC_REGADDRESS])
      else if(param1Type = tkNumber) then
        PushInstruction([JNC_ADDRESS])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'JZ') or IsTokenIsInstruction(inTokenData[tokenPointer], 'JE')) then
    begin
      // Instruction: JZ, JE
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([JZ_REGADDRESS])
      else if(param1Type = tkNumber) then
        PushInstruction([JZ_ADDRESS])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'JNZ') or IsTokenIsInstruction(inTokenData[tokenPointer], 'JNE')) then
    begin
      // Instruction: JNZ, JNE
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([JNZ_REGADDRESS])
      else if(param1Type = tkNumber) then
        PushInstruction([JNZ_ADDRESS])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'JA') or IsTokenIsInstruction(inTokenData[tokenPointer], 'JNBE')) then
    begin
      // Instruction: JA, JNBE
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([JA_REGADDRESS])
      else if(param1Type = tkNumber) then
        PushInstruction([JA_ADDRESS])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'JNA') or IsTokenIsInstruction(inTokenData[tokenPointer], 'JBE')) then
    begin
      // Instruction: JNA, JBE
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([JNA_REGADDRESS])
      else if(param1Type = tkNumber) then
        PushInstruction([JNA_ADDRESS])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'PUSH')) then
    begin
      // Instruction: PUSH
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([PUSH_REG])
      else if(param1Type = tkRegAddr) then
        PushInstruction([PUSH_REGADDRESS])
      else if(param1Type = tkAddr) then
        PushInstruction([PUSH_ADDRESS])
      else if(param1Type = tkNumber) then
        PushInstruction([PUSH_NUMBER])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'POP')) then
    begin
      // Instruction: POP
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([POP_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'CALL')) then
    begin
      // Instruction: CALL
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkRegAddr) then
        PushInstruction([CALL_REGADDRESS])
      else if(param1Type = tkNumber) then
        PushInstruction([CALL_ADDRESS])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'RET')) then
    begin
      // Instruction: RET
      if(NoExtraTokens()) then
      begin
        PushInstruction([RET]);
        inc(tokenPointer, 1);
      end else
      begin
        RaiseError(ERR_TOO_MANY_ARGUMENTS, lineNumber, inTokenData[tokenPointer + 1]);
        exit;
      end;
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'MUL')) then
    begin
      // Instruction: MUL
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([MUL_REG])
      else if(param1Type = tkRegAddr) then
        PushInstruction([MUL_REGADDRESS])
      else if(param1Type = tkAddr) then
        PushInstruction([MUL_ADDRESS])
      else if(param1Type = tkNumber) then
        PushInstruction([MUL_NUMBER])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'DIV')) then
    begin
      // Instruction: DIV
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([DIV_REG])
      else if(param1Type = tkRegAddr) then
        PushInstruction([DIV_REGADDRESS])
      else if(param1Type = tkAddr) then
        PushInstruction([DIV_ADDRESS])
      else if(param1Type = tkNumber) then
        PushInstruction([DIV_NUMBER])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'AND')) then
    begin
      // Instruction: AND
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      if(not ExtractParamCode(param2, param2Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(not GenerateParamCode(param2Type, param2, param2Val, 2)) then
        exit;

      if((param1Type = tkReg) and (param2Type = tkReg)) then
        PushInstruction([AND_REG_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkRegAddr)) then
        PushInstruction([AND_REGADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkAddr)) then
        PushInstruction([AND_ADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkNumber)) then
        PushInstruction([AND_NUMBER_WITH_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val, param2Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'OR')) then
    begin
      // Instruction: OR
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      if(not ExtractParamCode(param2, param2Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(not GenerateParamCode(param2Type, param2, param2Val, 2)) then
        exit;

      if((param1Type = tkReg) and (param2Type = tkReg)) then
        PushInstruction([OR_REG_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkRegAddr)) then
        PushInstruction([OR_REGADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkAddr)) then
        PushInstruction([OR_ADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkNumber)) then
        PushInstruction([OR_NUMBER_WITH_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val, param2Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'XOR')) then
    begin
      // Instruction: XOR
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      if(not ExtractParamCode(param2, param2Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(not GenerateParamCode(param2Type, param2, param2Val, 2)) then
        exit;

      if((param1Type = tkReg) and (param2Type = tkReg)) then
        PushInstruction([XOR_REG_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkRegAddr)) then
        PushInstruction([XOR_REGADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkAddr)) then
        PushInstruction([XOR_ADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkNumber)) then
        PushInstruction([XOR_NUMBER_WITH_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val, param2Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'NOT')) then
    begin
      // Instruction: NOT
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(param1Type = tkReg) then
        PushInstruction([NOT_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'SHL') OR IsTokenIsInstruction(inTokenData[tokenPointer], 'SAL')) then
    begin
      // Instruction: SHL, SAL
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      if(not ExtractParamCode(param2, param2Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(not GenerateParamCode(param2Type, param2, param2Val, 2)) then
        exit;

      if((param1Type = tkReg) and (param2Type = tkReg)) then
        PushInstruction([SHL_REG_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkRegAddr)) then
        PushInstruction([SHL_REGADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkAddr)) then
        PushInstruction([SHL_ADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkNumber)) then
        PushInstruction([SHL_NUMBER_WITH_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val, param2Val]);
    end
    else if(IsTokenIsInstruction(inTokenData[tokenPointer], 'SHR') OR IsTokenIsInstruction(inTokenData[tokenPointer], 'SAR')) then
    begin
      // Instruction: SHR, SAR
      if(not ExtractParamCode(param1, param1Type)) then
        exit;

      if(not ExtractParamCode(param2, param2Type)) then
        exit;

      // Check for any extra operands.
      if(not NoExtraTokens()) then
      begin
        RaiseError(ERR_TOO_MANY_OPERANDS, lineNumber);
        exit;
      end;

      // Generate output values for operands.
      if(not GenerateParamCode(param1Type, param1, param1Val, 1)) then
        exit;

      if(not GenerateParamCode(param2Type, param2, param2Val, 2)) then
        exit;

      if((param1Type = tkReg) and (param2Type = tkReg)) then
        PushInstruction([SHR_REG_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkRegAddr)) then
        PushInstruction([SHR_REGADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkAddr)) then
        PushInstruction([SHR_ADDRESS_WITH_REG])
      else if((param1Type = tkReg) and (param2Type = tkNumber)) then
        PushInstruction([SHR_NUMBER_WITH_REG])
      else
      begin
        RaiseError(ERR_UNSUPPORTD_OPERAND, lineNumber);
        exit;
      end;

      PushInstruction([param1Val, param2Val]);
    end
    else
    begin
      // Unknown instruction. May be a syntax error in code.
      RaiseError(ERR_SYNTAX, lineNumber);
      exit;
    end;
  end;
end;

// Process tokens and generate output.
procedure TCompiler.Compile();
var
  tempLine: String;
  extractionTable: TStringList;
  linePos, startPos: Integer;
  strStart: Integer;
  verb: String;
  isStringField, ignoreToken: Boolean;

  procedure AddToken(inToken: String);
  begin
    if(inToken <> '')then
    begin
      extractionTable.Add(inToken);
    end;
  end;

begin
  extractionTable := TStringList.Create;

  repeat
    tempLine := Trim(sourceScript.Strings[compileLine]);
    isStringField := false;

    // Ignore empty source lines.
    if(Length(tempLine) > 0) then
    begin

      // Check for source line comments.
      if((tempLine[1] = ';') or ( (Length(tempLine) >= 2) and (tempLine[2] = tempLine[1]) and (tempLine[1] = '/'))) then
      begin
        // Source line comment, nothing to do.
      end
      else
      begin
        extractionTable.Clear;
        startPos := 1;
        strStart := 0;
        ignoreToken := false;

        // Tokenizer.
        for linePos := 1 to Length(tempLine) do
        begin
          // Check for string field.
          if((tempLine[linePos] = '"') and (isStringField)) then
          begin
            // Check for end of string.
            AddToken(Trim(Copy(tempLine, strStart, (linePos - strStart + 1))));
            startPos := linePos + 1;
            isStringField := false;
          end
          else if(isStringField) then
          begin
            // Ignore string content and move to next character.
            continue;
          end
          else if((tempLine[linePos] = '"') and (not isStringField)) then
          begin
            // Check for string start.
            isStringField := true;
            strStart := linePos;
            continue;
          end
          // Check for more inline comments and ignore them.
          else if((tempLine[linePos] = ';') or ((Length(tempLine) >= (linePos + 1)) and (tempLine[linePos] = '/') and (tempLine[linePos] = tempLine[linePos + 1]))) then
          begin
            ignoreToken := true;
            break;
          end
          // Split based on symbols.
          else if((tempLine[linePos] = ':') or (tempLine[linePos] = ','))then
          begin
            AddToken(Trim(Copy(tempLine, startPos, (linePos - startPos))));
            AddToken(Trim(tempLine[linePos]));
            startPos := linePos + 1;
          end
          // Split based on white spaces and extract token.
          else if (tempLine[linePos] = ' ') then
          begin
            if(startPos = linePos)then
            begin
              continue;
            end
            else
            begin
              verb := Trim(Copy(tempLine, startPos, (linePos - startPos)));
              if(verb <> '') then
              begin
                startPos := linePos + 1;
                AddToken(verb);
              end;
            end;
          end;
        end;
        // Add last word in current line to token table.
        if(not ignoreToken) then
          AddToken(Trim(Copy(tempLine, startPos, (linePos - startPos + 1))));

        TokenParser(extractionTable, compileLine);
      end;
    end;

    // Move to next source line.
    inc(compileLine);
  until (compileLine >= sourceScript.Count);

  // Update label location in output binary stream.
  AssignLabelAddress();

  // Check output size fits into target system RAM.
  if(outputStream.Size > BYTE_LIMITER) then
  begin
    RaiseError(ERR_OUTPUT_LIMIT, -1);
    exit;
  end;

  WriteOutputFile();

  freeAndNil(extractionTable);
end;

// Check available labels in map and assign them to output binary stream.
procedure TCompiler.AssignLabelAddress();
var
  mapPos: integer;
  addrVal: integer;
  searchLabel: string;

  function GetLabelAddress(lblName: string): Integer;
  var
    tempEntry: PLabelEntry;
  begin
    result := -1;
    for tempEntry in labelTable do
    begin
      if(UpperCase(tempEntry^.LabelName) = UpperCase(lblName)) then
      begin
        result := tempEntry^.LabelAddr;
        break;
      end;
    end;
  end;

begin
  if((Assigned(labelMap)) and (Assigned(labelTable)) and (labelMap.Count > 0)) then
  begin
    for mapPos := 0 to (labelMap.Count - 1) do
    begin
      // Looking for availability of label address.
      searchLabel := PLabelMapEntry(labelMap[mapPos])^.LabelName;
      addrVal := GetLabelAddress(searchLabel);
      if((addrVal >= 0) and (Assigned(outputStream))) then
      begin
        // Update binary stream with label address.
        outputStream.Position := (PLabelMapEntry(labelMap[mapPos])^.CodeAddr) - 1;
        outputStream.WriteByte(addrVal);
      end
      else
      begin
        RaiseError(ERR_UNKNOWN_LABEL, PLabelMapEntry(labelMap[mapPos])^.SourceLine, searchLabel);
        exit;
      end;
    end;
  end;
end;

// Write content of output buffer to file.
procedure TCompiler.WriteOutputFile();
begin
  outputStream.Position := 0;
  outputStream.SaveToFile(fOutputPath);
end;

end.


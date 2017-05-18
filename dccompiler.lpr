{ Experimental CPU - Assembler compiler.

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

program dccompiler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, uCompiler;

// Binary output file extention.
const
  OUTPUT_EXT = '.bin';

type
  TDemoCpuCompiler = class(TCustomApplication)
  private
    errorCount: Integer;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure OnError(errorNumber: Integer; errorStr: String);
    procedure OnCompileFinish(outputSize: Integer);
  end;

// Application executable loop.
procedure TDemoCpuCompiler.DoRun;
var
  srcFilename: String;
  asmCompiler: TCompiler;
  srcData: TStringList;
begin
  if(ParamCount > 0) then
  begin
    srcFilename := ParamStr(1);

    // Check availability of specified source file.
    if(FileExists(srcFilename)) then
    begin
      // Create compiler object to build the specified source file.
      errorCount := 0;
      asmCompiler := TCompiler.Create();
      asmCompiler.OnError := @OnError;
      asmCompiler.OnCompileFinish := @OnCompileFinish;
      asmCompiler.OutputFileName := ChangeFileExt(srcFilename, OUTPUT_EXT);

      // Load assembler script and start compilation.
      srcData := TStringList.Create;
      srcData.LoadFromFile(srcFilename);
      asmCompiler.ParseScript(srcData);
    end
    else
    begin
      writeln('Fatal: Specified assembler source file does not exists or invalid.');
    end;
  end
  else
  begin
    writeln('Demo CPU compiler - Version 1.0.0');
    writeln('Usage: '+ ExtractFileName(ParamStr(0)) +' <asm file>');
  end;

  Terminate;
end;

// Compiler error handling callback.
procedure TDemoCpuCompiler.OnError(errorNumber: Integer; errorStr: String);
begin
  writeln('Error(' + IntToStr(errorNumber) + '): ' + errorStr);
  inc(errorCount);
end;

// Demo CPU compiler application class constructor.
constructor TDemoCpuCompiler.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

// Demo CPU compiler application class destructor.
destructor TDemoCpuCompiler.Destroy;
begin
  inherited Destroy;
end;

// Compilation finish callback handler.
procedure TDemoCpuCompiler.OnCompileFinish(outputSize: Integer);
var
  codeSize: string;
  tempSize: Single;
begin
  if(errorCount = 0) then
  begin
    // Format the code output size.
    if(outputSize > 1000) then
    begin
      tempSize := outputSize / 1000;
      if(tempSize > 1000) then
        codeSize := FloatToStr(tempSize) + ' MB'
      else
        codeSize := FloatToStr(tempSize) + ' KB'
    end
    else
      codeSize := IntToStr(outputSize) + ' Bytes';

    writeln('Compilation finish successfully with output size: ' + codeSize);
  end;

  Terminate;
end;

// Main application entry point.
var
  Application: TDemoCpuCompiler;
begin
  Application:=TDemoCpuCompiler.Create(nil);
  Application.Title:='Demo CPU Compiler';
  Application.Run;
  Application.Free;
end.


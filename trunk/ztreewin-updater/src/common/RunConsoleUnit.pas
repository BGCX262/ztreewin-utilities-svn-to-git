unit RunConsoleUnit;

interface
  function RunConsole(ACommandLine: string;
                      out AExitCode: cardinal;
                      const ATimeout: cardinal = 1500;
                      const ASkipOemConversion: boolean = false): string;

implementation
  uses
    Windows, SysUtils;

function RunConsole(ACommandLine: string;
                    out AExitCode: cardinal;
                    const ATimeout: cardinal = 1500;
                    const ASkipOemConversion: boolean = false): string;
const
   BufferSize = 2400;
var
  Security: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  StartupInfo: TStartUpInfo;
  ProcessInfo: TProcessInformation;
  Buffer: PChar;
  BytesRead: DWord;
  AppRunning: DWord;
  BytesAvailable: DWord;
begin
  Result := '';
  With Security do begin
    nLength := SizeOf(TSecurityAttributes);
    bInheritHandle := true;
    lpSecurityDescriptor := nil;
  end;
  if CreatePipe(ReadPipe, WritePipe, @Security, 0) then begin
    Buffer := AllocMem(BufferSize + 1);
    try
      FillChar(StartupInfo, Sizeof(StartupInfo), #0);
      With StartupInfo do begin
        cb := SizeOf(StartupInfo);
        hStdOutput := WritePipe;
        hStdInput := ReadPipe;
//        hStdError := ReadPipe; // TODO: see if this works
        dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
        wShowWindow := SW_HIDE;
      end;
      if CreateProcess(nil,
             PChar(ACommandLine),
             @Security,
             @Security,
             true,
             NORMAL_PRIORITY_CLASS,
             nil,
             nil,
             StartupInfo,
             ProcessInfo)
      then begin
//        repeat
          AppRunning := WaitForSingleObject(ProcessInfo.hProcess, ATimeout);
//          SleepEx(100, True);
//        until (AppRunning <> WAIT_TIMEOUT);

        PeekNamedPipe(ReadPipe, nil, 0, nil, @BytesAvailable, nil);
        repeat
          BytesRead := 0;
          ReadFile(ReadPipe, Buffer[0], BufferSize, BytesRead, nil);
          Buffer[BytesRead]:= #0;
          if ASkipOemConversion = false then begin
            OemToAnsi(Buffer, Buffer);
          end;
          Result := Result + String(Buffer);
        until (BytesRead < BufferSize);
      end;
    finally
      FreeMem(Buffer);
    end;
    GetExitCodeProcess(ProcessInfo.hProcess, AExitCode);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ReadPipe);
    CloseHandle(WritePipe);

    Result := StringReplace(Result, #13#10, #10, [rfReplaceAll]);
    Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
    Result := StringReplace(Result, #10, #13#10, [rfReplaceAll]);
  end;
end;

end.


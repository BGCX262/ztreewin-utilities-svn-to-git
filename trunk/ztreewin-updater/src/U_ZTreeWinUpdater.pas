unit U_ZTreeWinUpdater;

interface
uses
  Classes{,
  ZipMstr};
const
  cExecutableName = 'ztw.exe';
type
  TUpdateAction = (uaUpdate = 0, uaRollback = 1);

  TZTreeWinUpdater = class
    private
      FVersion: string;
      FHomePath: string;
      FBackupRootPath: string;
      FProxy: string;
      FUpdateFile: string;
      FBackupName: string;

      FAction: TUpdateAction;
      FOverwrite: boolean;

      FReleaseKey: string;
      FTempPath: string;
      FSkippedFiles: TStringList;

      procedure Init;
      procedure ReadParams;
      function  DetermineHome: boolean;
      function  DetermineVersion: boolean;

      function  CopyFiles(SourceDir, TargetDir: string): integer; 
      function  DownloadFile(URL: string; TargetPath: string): string;
      function  ExtractFilesFromZip(Zipfile, TargetPath: string): integer;

//      procedure ZipExtractSkipped(Sender: TObject; ForFile: string; SkipType: UnZipSkipTypes; ExtError: integer);
//      procedure ZipMessage(Sender: TObject; ErrCode: integer; Message: string);
    public
      constructor Create;
      destructor  Destroy; override;

      function  GetOnlineVersions: string;

      function  DownloadLatest: boolean;
      procedure BackupCurrent;
      function  InstallLatest: boolean;
      function  Rollback(Version: string): boolean;

      property Action: TUpdateAction      read FAction;
      property Overwrite: boolean         read FOverwrite write FOverwrite;

      property CurrentVersion: string     read FVersion;
      property UpdateFile: string         read FUpdateFile;
      property TargetVersion: string      read FBackupName;
  end;

implementation

uses
  SysUtils, Windows, Registry, IniFiles,
  URLMon, ComObj, ShellAPI,
  RunConsoleUnit;

////////////////////////////////////////////////////////////////////////////////////////////////////
{ TZTreeWinUpdater }

constructor TZTreeWinUpdater.Create;
begin
  FAction := uaUpdate;
  Init;
end;
{ ------------------------------------------------------------------------------------------------ }
destructor TZTreeWinUpdater.Destroy;
begin
  // Empty and remove FTempPath
  SysUtils.DeleteFile(FTempPath + '*.*');
  SysUtils.RemoveDir(FTempPath);

  inherited;
end;

{ ------------------------------------------------------------------------------------------------ }

procedure TZTreeWinUpdater.Init;
begin
  FTempPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
  ForceDirectories(FTempPath);
  ReadParams;
  if FHomePath = '' then begin
    DetermineHome;
    if FHomePath = '' then begin
      raise Exception.Create('Unable to determine a path to ZTreeWin. Please use the -home switch.');
    end;
  end;
  if FVersion = '' then begin
    DetermineVersion;
    if FVersion = '' then begin
      raise Exception.Create('Unable to determine the current version of ZTreeWin. Please use the -version switch.');
    end;
  end;
  if FBackupRootPath = '' then begin
    FBackupRootPath := FHomePath + 'Updates\Backup\';
  end;
end;

{ ------------------------------------------------------------------------------------------------ }

procedure TZTreeWinUpdater.ReadParams;
var
  i: integer;
  SkipNext: boolean;
begin
  FReleaseKey := 'FullVersion';

  // Read parameters: zipfile, homedir, proxy, release
  SkipNext := False;
  for i := 1 to ParamCount do begin
    if not SkipNext then begin
      if SameText(ParamStr(i), '-rollback') then begin
        FAction := uaRollback;
        if i < ParamCount then begin
          FBackupName := ParamStr(i + 1);
          SkipNext := Copy(FBackupName, 1) <> '-';
          if not SkipNext then begin
            FBackupName := '';
          end;
        end else begin
          FBackupName := '';
        end;
        SkipNext := True;
      end else if SameText(ParamStr(i), '-home') then begin
        FHomePath := IncludeTrailingPathDelimiter(ParamStr(i + 1));
        SkipNext := DirectoryExists(FHomePath);
        if not SkipNext then begin
          FHomePath := '';
        end;
      end else if SameText(ParamStr(i), '-backup') then begin
        FBackupRootPath := IncludeTrailingPathDelimiter(ParamStr(i + 1));
        SkipNext := True;
      end else if SameText(ParamStr(i), '-proxy') then begin
        FProxy := ParamStr(i + 1);
        SkipNext := True;
      end else if SameText(ParamStr(i), '-version') then begin
        FVersion := ParamStr(i + 1);
        SkipNext := True;
      end else if SameText(ParamStr(i), '-beta') then begin
        FReleaseKey := 'BetaZip';
      end else if SameText(ParamStr(i), '-zeta') then begin
        FReleaseKey := 'ZetaZip';
      end else begin
        FUpdateFile := ParamStr(i);
      end;
    end else begin
      SkipNext := False;
    end;
  end;
  if (FUpdateFile <> '') and not FileExists(FUpdateFile) then begin
    raise Exception.CreateFmt('Could not find zip file "%s"', [FUpdateFile]);
  end;
end;

{ ------------------------------------------------------------------------------------------------ }
function TZTreeWinUpdater.DetermineHome;
var
  Reg: TRegistry;
begin
  Result := True;

  // Wasn't it already set by the user?
  if DirectoryExists(FHomePath) then begin
    Exit;
  end;

  // First, try to get the environment variable set by ZTreeWin itself
  FHomePath := GetEnvironmentVariable('#ZTHome');
  if DirectoryExists(FHomePath) then begin
    FHomePath := IncludeTrailingPathDelimiter(FHomePath);
    Exit;
  end;

  // Next, try to read the registry setting written by the installer
  Reg := TRegistry.Create(KEY_QUERY_VALUE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Software\ZTreeWin', False) then begin
      FHomePath := Reg.ReadString('Install_Dir');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  if DirectoryExists(FHomePath) then begin
    FHomePath := IncludeTrailingPathDelimiter(FHomePath);
    Exit;
  end;

  // Otherwise, ask the user...
  Write('Please enter the full home path of ZTreeWin:');
  Readln(Input, FHomePath);
  if DirectoryExists(FHomePath) then begin
    FHomePath := IncludeTrailingPathDelimiter(FHomePath);
    Exit;
  end;

  Result := False;
  FHomePath := '';
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeWinUpdater.DetermineVersion: boolean;
var
  Text: string;
  ExitCode: cardinal;
  StartIndex, EndIndex: integer;
begin
  Result := True;
  // Wasn't it specified on our command line?
  if FVersion <> '' then begin
    Exit;
  end;

  // Run a copy of ZTW.exe in the FHomeDir with /?,
  //  and extract the version number from its first line of standard output
  Text := RunConsole('"' + FHomePath + cExecutableName + '" /?', ExitCode);
//WriteLn(Text);
  StartIndex := Pos('ZTreeWin v', Text);
  if StartIndex > 0 then begin
    EndIndex := Pos('-', Copy(Text, StartIndex));
    if EndIndex > 0 then begin
      FVersion := Trim(Copy(Text, StartIndex + 10, EndIndex - StartIndex - 10));
    end;
  end;
  if FVersion <> '' then begin
    Exit;
  end;

  FVersion := '';
  Result := False;
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeWinUpdater.Rollback(Version: string): boolean;
begin
  Result := False;
  if FBackupName = '' then begin
    // TODO: determine the latest update before the current version

  end;

  // Copy the files from the given backup dir to FHomeDir
  CopyFiles(FBackupRootPath + FBackupName, FHomePath);
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeWinUpdater.CopyFiles(SourceDir, TargetDir: string): integer;
var
  Files: TStringList;
  Failure: integer;
  Found: TSearchRec;
  i, ErrorCode: Integer;
  NewName: TFilename;
begin
  Result := 0;
  ForceDirectories(TargetDir);

  // Read the list of files in FHomeDir
  Files := TStringList.Create;
  try
    SourceDir := IncludeTrailingPathDelimiter(SourceDir);
    Failure := FindFirst(SourceDir + '*.*', 0, Found);
    try
      while Failure = 0 do begin
        Files.AddObject(Found.Name, TObject(False));
        Failure := FindNext(Found);
      end;
      if (Failure <> ERROR_NO_MORE_FILES) then begin
        RaiseLastOSError(Failure);
      end;
    finally
      SysUtils.FindClose(Found);
    end;

    // Copy all files from SourceDir to FBackupDir's subdir for this version
    TargetDir := IncludeTrailingPathDelimiter(TargetDir);
    for i := 0 to Files.Count - 1 do begin
      try
        if CopyFile(PChar(SourceDir + Files[i]), PChar(TargetDir + Files[i]), not FOverwrite) then begin
          Inc(Result);
        end else begin
          ErrorCode := GetLastError;
          if (ErrorCode = ERROR_ACCESS_DENIED) or (ErrorCode = ERROR_SHARING_VIOLATION) then begin
            case FAction of
              uaUpdate: begin
                NewName := ChangeFileExt(Files[i], '_v' + FVersion + '.' + ExtractFileExt(Files[i]));
              end;
              uaRollback: begin
                NewName := ChangeFileExt(Files[i], '_' + ExtractFileName(SourceDir) + '.' + ExtractFileExt(Files[i]));
              end;
            end;
            if SysUtils.RenameFile(TargetDir + Files[i], TargetDir + NewName) then begin
              // Try again
              if CopyFile(PChar(SourceDir + Files[i]), PChar(TargetDir + Files[i]), not FOverwrite) then begin
                Inc(Result);
                continue;
              end else begin
                ErrorCode := GetLastError;
              end;
            end;
          end;
          RaiseLastOSError(ErrorCode);
        end;
      except
        on E: Exception do begin
          raise Exception.CreateFmt('Failed to copy "%s" from "%s" to "%s": %s [%s]', [Files[i], SourceDir, TargetDir, E.Message, E.ClassName]);
        end;
      end;
    end;
  finally
    Files.Free;
  end;
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeWinUpdater.DownloadLatest: boolean;
var
  UpdateIni, FileToDownload: string;
  IniFile: TIniFile;
  CharPos, MajorVersion: integer;
  URL: string;
  BaseURL: array[0..1] of string;
  i: Integer;
begin
  // Determine URL of latest, from http://www.zedtek.com/download/update.ini
  UpdateIni := DownloadFile('http://www.zedtek.com/download/update.ini', FTempPath);
  IniFile := TIniFile.Create(UpdateIni);
  try
    if FReleaseKey = 'ZetaZip' then begin
      MajorVersion := 0;
      CharPos := Pos('.', FVersion);
      if CharPos > 0 then begin
        MajorVersion := StrToInt(Copy(FVersion, 1, CharPos - 1));
      end;
      if MajorVersion > 1 then begin
        FReleaseKey := FReleaseKey + IntToStr(MajorVersion);
      end;
    end;
    FileToDownload := IniFile.ReadString('Latest', FReleaseKey, '');

    // TODO: check whether this file is different than the one we've most recently installed

    BaseURL[0] := IniFile.ReadString('Sites', 'Primary', 'http://www.zedtek.com/download');
    BaseURL[1] := IniFile.ReadString('Sites', 'Secondary', 'http://www.ztree.com/download');
    for i := Low(BaseURL) to High(BaseURL) do begin
      URL := BaseURL[i] + '/' + FileToDownload;
      FUpdateFile := DownloadFile(URL, FTempPath);
      Result := FileExists(FUpdateFile);
      if Result then begin
        Exit;
      end;
    end;
  finally
    IniFile.Free;
    SysUtils.DeleteFile(UpdateIni);
  end;
end;

{ ------------------------------------------------------------------------------------------------ }

procedure TZTreeWinUpdater.BackupCurrent;
var
  TargetDir: string;
begin
  // Create a subdir under FBackupDir
  TargetDir := FBackupRootPath + 'v' + FVersion;
  CopyFiles(FHomePath, TargetDir)

  // TODO: Write backup version and time in the log file, so that we can roll back later
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeWinUpdater.InstallLatest: boolean;
begin
  if SameText(ExtractFileExt(FUpdateFile), '.zip') then begin
    Result := ExtractFilesFromZip(FUpdateFile, FHomePath) > 0;
  end else begin
    Result := 32 < ShellExecute(GetDesktopWindow(), nil, PChar(FUpdateFile), nil, PChar(ExtractFilePath(FUpdateFile)), SW_SHOWNORMAL);
  end;
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeWinUpdater.ExtractFilesFromZip(Zipfile, TargetPath: string): integer;
//var
//  ZM: TZipMaster;
//  i, ErrorCode: Integer;
//  NewName: string;
//  FilesToRetry: TStringList;
begin
  Result := 0; // TODO
//  ZM := TZipMaster.Create(nil);
//  try
//    ZM.OnExtractSkipped := ZipExtractSkipped;
//    ZM.OnMessage := ZipMessage;
//
//    ZM.ZipFileName := Zipfile;
//    ZM.ExtrBaseDir := TargetPath;
//    ZM.ExtrOptions := [ExtrDirNames, ExtrForceDirs, ExtrOverWrite];
//    ZM.FSpecArgs.Add('*.*');
//    FSkippedFiles := TStringList.Create;
//    try
//      ZM.Extract;
//      Result := ZM.SuccessCnt;
//
//        // Retry to extract any files which were locked
//      if FSkippedFiles.Count > 0 then begin
//        FilesToRetry := TStringList.Create;
//        try
//          for i := FSkippedFiles.Count - 1 downto 0 do begin
//            ErrorCode := integer(FSkippedFiles.Objects[i]);
//            // For all files skipped for cause of sharing violations, rename them, and try again
//            if (ErrorCode = ERROR_ACCESS_DENIED) or (ErrorCode = ERROR_SHARING_VIOLATION) then begin
//              NewName := ChangeFileExt(FSkippedFiles[i], '_v' + FVersion + ExtractFileExt(FSkippedFiles[i]));
//              if not SysUtils.RenameFile(TargetPath + FSkippedFiles[i], TargetPath + NewName) then begin
//                FilesToRetry.Add(FSkippedFiles[i]);
//                FSkippedFiles.Delete(i);
//              end;
//            end;
//          end;
//          ZM.FSpecArgs.Clear;
//          ZM.FSpecArgs.AddStrings(FilesToRetry);
//        finally
//          FilesToRetry.Free;
//        end;
//        FSkippedFiles.Clear;
//        // Retry the previously skipped files
//        ZM.Extract;
//        if FSkippedFiles.Count > 0 then begin
//          raise Exception.CreateFmt('Unable to extract the following file(s):'#13#10'%s', [FSkippedFiles.Text]);
//        end;
//        Inc(Result, ZM.SuccessCnt);
//      end;
//    finally
//      FSkippedFiles.Free;
//    end;
//  finally
//    ZM.Free;
//  end;
end;
//{ ------------------------------------------------------------------------------------------------ }
//procedure TZTreeWinUpdater.ZipExtractSkipped(Sender: TObject; ForFile: String;
//  SkipType: UnZipSkipTypes; ExtError: Integer);
//begin
//  FSkippedFiles.AddObject(StringReplace(ForFile, '/', '\', [rfReplaceAll]), TObject(ExtError));
//end;
//
//{ ------------------------------------------------------------------------------------------------ }
//procedure TZTreeWinUpdater.ZipMessage(Sender: TObject; ErrCode: integer; Message: string);
//begin
//  if ErrCode <> 0 then begin
//    WriteLn(Format('Unzip error %d: "%s"', [ErrCode, Message]));
//  end else begin
//    WriteLn(Format('Unzip message: "%s"', [Message]));
//  end;
//end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeWinUpdater.DownloadFile(URL: string; TargetPath: string): string;
var
  Filename: string;
begin
  ForceDirectories(TargetPath);
  Filename := ExtractFilename(StringReplace(URL, '/', '\', [rfReplaceAll]));

  // Try to download the file
  Result := IncludeTrailingPathDelimiter(TargetPath) + Filename;
  OleCheck(UrlDownloadToFile(nil, PChar(URL), PChar(Result), 0, nil));
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeWinUpdater.GetOnlineVersions: string;
begin

end;

{ ------------------------------------------------------------------------------------------------ }

end.

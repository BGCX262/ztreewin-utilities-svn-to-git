unit U_ZTreeUpdater;

interface
uses
  Classes, IniFiles, ShellAPI;

const
  cExecutableName = 'ztw.exe';

type
  TFilePath = string;
  TReleaseLevel = (rlFull, rlBeta, rlZeta);
  TPackageName = string;
  TVersionNumber = string;

  TZTreeUpdater = class
  private
    FHomePath: TFilePath;
    FBackupRootPath: TFilePath;
    FCurrentVersion: TVersionNumber;

    FOverwrite: boolean;

    FTempPath: TFilePath;
    FUpdateIniPath: TFilePath;
    FUpdateIni: TIniFile;
    FFilesToMove: TStringList;

    function  DetermineHome: boolean;
    function  DetermineVersion: boolean;
    procedure RefreshUpdateIni;

    function  CopyFiles(SourceDir, TargetDir: TFilePath; Overwrite: boolean = False; OverwrittenVersion: TVersionNumber = ''): integer;
    function  DownloadFile(URL: string; TargetPath: TFilePath): TFilePath;
    function  ExtractFilesFromZip(Zipfile, TargetPath: TFilePath): integer;

    function  VersionOfPackage(Package: TPackageName): TVersionNumber;
    function  PackageOfVersion(Version: TVersionNumber): TPackageName;

    function  DetermineLatestPackage(ReleaseLevel: TReleaseLevel = rlFull): TPackageName;
    function  BackupCurrentVersion: boolean;
    function  GetPackageLocalPath(Package: TPackageName): TFilePath;

    procedure SetHomePath(const Value: TFilePath);
    procedure SetBackupPath(const Value: TFilePath);
  public
    constructor Create(HomePath: TFilePath = ''; Version: TVersionNumber = '');
    destructor  Destroy; override;

//    function  CheckLatest(Level: TReleaseLevel): TVersionNumber;
    function  DownloadLatest(Level: TReleaseLevel = rlFull): TFilePath;
    function  InstallLatest(Level: TReleaseLevel = rlFull): TVersionNumber;

    procedure InstallPackage(Package: TPackageName);
    function  RollbackInstall: TVersionNumber;

    property  HomePath: TFilePath             read FHomePath        write SetHomePath;
    property  BackupPath: TFilePath           read FBackupRootPath  write SetBackupPath;
    property  CurrentVersion: TVersionNumber  read FCurrentVersion;

  end;

////////////////////////////////////////////////////////////////////////////////////////////////////
implementation
uses
  SysUtils, Registry, Windows,
  URLMon, ComObj,
  RunConsoleUnit, Unzip;

{ ================================================================================================ }
{ Unzip callback functions }
procedure UnzipMessage(ucsize, csiz, cfactor, mo, dy, yr, hh, mm: Longword; c: Byte; fname, meth: PChar; crc: Longword; fCrypt: Byte); stdcall;
begin
  // TODO: list the file's info?
  OutputDebugString(fname);
end;

{ ------------------------------------------------------------------------------------------------ }

function UnzipPassword(pwbuf: PChar; size: Longint; m, efn: PChar): EDllPassword; stdcall;
begin
  // ZTreeWin's updates do no use a password
  Result := IZ_PW_NONE;
end;

{ ------------------------------------------------------------------------------------------------ }

function UnzipPrint(buffer: PChar; size: Longword): EDllPrint; stdcall;
begin
  // TODO: what exactly does this do?
  OutputDebugString(PChar(Copy(buffer, 1, size)));
  Result := PK_OK;
end;

{ ------------------------------------------------------------------------------------------------ }

function UnzipReplace(filename: PChar): EDllReplace; stdcall;
begin
  // All existing files should be replaced
  OutputDebugString(filename);
  Result := IDM_REPLACE_ALL;
end;

{ ------------------------------------------------------------------------------------------------ }

function UnzipService(efn: PChar; details: Longword): EDllService; stdcall;
begin
  OutputDebugString(PChar(Format('File: %s; details: %d', [efn, details])));

  // TODO: look for flag to stop?
  Result := UZ_ST_CONTINUE;
end;

{ ------------------------------------------------------------------------------------------------ }

procedure UnzipSound; stdcall;
begin
  MessageBeep(MB_ICONASTERISK);
end;

{ ================================================================================================ }
{ TZTreeUpdater }

constructor TZTreeUpdater.Create(HomePath: TFilePath; Version: TVersionNumber);
begin
  FTempPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
  ForceDirectories(FTempPath);

  DetermineHome;
  DetermineVersion;
end;
{ ------------------------------------------------------------------------------------------------ }
destructor TZTreeUpdater.Destroy;
begin
  if Assigned(FUpdateIni) then begin
    FUpdateIni.Free;
    SysUtils.DeleteFile(FUpdateIniPath);
  end;

  // Empty and remove FTempPath
  SysUtils.DeleteFile(FTempPath + '*.*');
  SysUtils.RemoveDir(FTempPath);

  inherited;
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeUpdater.DetermineHome: boolean;
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

  // Otherwise, ask the user... TODO: raise an event for this!
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

function TZTreeUpdater.DetermineVersion: boolean;
var
  Text: string;
  ExitCode: cardinal;
  StartIndex, EndIndex: integer;
begin
  Result := True;
  // Wasn't it specified on our command line?
  if FCurrentVersion <> '' then begin
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
      FCurrentVersion := Trim(Copy(Text, StartIndex + 10, EndIndex - StartIndex - 10));
    end;
  end;
  if FCurrentVersion <> '' then begin
    Exit;
  end;

  FCurrentVersion := '';
  Result := False;
end;

{ ------------------------------------------------------------------------------------------------ }

//function TZTreeUpdater.CheckLatest(Level: TReleaseLevel): TVersionNumber;
//var
//  Package: TPackageName;
//begin
//  Package := DetermineLatestPackage(Level);
//  Result := VersionOfPackage(Package);
//end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeUpdater.DownloadLatest(Level: TReleaseLevel): TFilePath;
var
  Package: TPackageName;
  BaseURL: array[0..1] of string;
  i: Integer;
  URL: AnsiString;
  Errors: string;
begin
  // Determine URL of latest package, from http://www.zedtek.com/download/update.ini
  Package := DetermineLatestPackage(Level);
  Result := GetPackageLocalPath(Package);
  if Length(Result) = 0 then begin
    Errors := '';
    BaseURL[0] := FUpdateIni.ReadString('Sites', 'Primary', 'http://www.zedtek.com/download');
    BaseURL[1] := FUpdateIni.ReadString('Sites', 'Secondary', 'http://www.ztree.com/download');
    for i := Low(BaseURL) to High(BaseURL) do begin
      URL := BaseURL[i] + '/' + Package;
      try
        Result := DownloadFile(URL, FBackupRootPath);
      except
        on E: Exception do begin
          Result := '';
          Errors := Errors + #13#10'- ' + E.Message;
        end;
      end;
      if FileExists(Result) then begin
        Exit;
      end;
    end;
    raise Exception.CreateFmt('Failed to download package "%s":%s', [Package, Errors]);
  end;
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeUpdater.DetermineLatestPackage(ReleaseLevel: TReleaseLevel): TPackageName;
const
  LevelKeys: array[0..3] of string = ('FullVersion', 'BetaExe', 'BetaZip', 'ZetaZip');
var
  StartIndex, i: integer;
  CharPos, MajorVersion: integer;
  Key: string;
begin
  RefreshUpdateIni;

  MajorVersion := 0;
  CharPos := Pos('.', FCurrentVersion);
  if CharPos > 0 then begin
    MajorVersion := StrToInt(Copy(FCurrentVersion, 1, CharPos - 1));
  end;

  StartIndex := High(LevelKeys);
  case ReleaseLevel of
    rlZeta: StartIndex := 3;
    rlBeta: StartIndex := 2;
    rlFull: StartIndex := 1;
  end;

  Result := '';
  for i := StartIndex downto Low(LevelKeys) do begin
    Key := LevelKeys[i];
    Result := FUpdateIni.ReadString('Latest', Key + IntToStr(MajorVersion), '');
    if Length(Result) > 0 then
      Exit;
    Result := FUpdateIni.ReadString('Latest', Key, '');
    if Length(Result) > 0 then
      Exit;
  end;

  if Result = '' then begin
    raise Exception.Create('Couldn''t figure out what the latest package is!');
  end;
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeUpdater.BackupCurrentVersion: boolean;
var
  BackupPath: TFilePath;
begin
  BackupPath := FBackupRootPath + FCurrentVersion;
  Result := CopyFiles(FHomePath, BackupPath, True) > 0;
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeUpdater.CopyFiles(SourceDir, TargetDir: TFilePath; Overwrite: boolean = False; OverwrittenVersion: TVersionNumber = ''): integer;
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
        if CopyFile(PChar(SourceDir + Files[i]), PChar(TargetDir + Files[i]), not Overwrite) then begin
          Inc(Result);
        end else begin
          ErrorCode := GetLastError;
          if (ErrorCode = ERROR_ACCESS_DENIED) or (ErrorCode = ERROR_SHARING_VIOLATION) then begin
            NewName := ChangeFileExt(Files[i], '_' + OverwrittenVersion + ExtractFileExt(Files[i]));
//			if Rollback then begin
//              NewName := ChangeFileExt(Files[i], '_' + ExtractFileName(SourceDir) + ExtractFileExt(Files[i]));
//            end else begin
//              NewName := ChangeFileExt(Files[i], '_v' + FCurrentVersion + ExtractFileExt(Files[i]));
//            end;
            if SysUtils.RenameFile(TargetDir + Files[i], TargetDir + NewName) then begin
              // Try again
              if CopyFile(PChar(SourceDir + Files[i]), PChar(TargetDir + Files[i]), not Overwrite) then begin
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

function TZTreeUpdater.DownloadFile(URL: string; TargetPath: TFilePath): TFilePath;
var
  Filename: TFilePath;
begin
  ForceDirectories(TargetPath);
  Filename := ExtractFilename(StringReplace(URL, '/', '\', [rfReplaceAll]));

  // Try to download the file
  Result := IncludeTrailingPathDelimiter(TargetPath) + Filename;
  OleCheck(UrlDownloadToFile(nil, PChar(URL), PChar(Result), 0, nil));
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeUpdater.ExtractFilesFromZip(Zipfile, TargetPath: TFilePath): integer;
var
  Options: DCL;
  Callbacks: USERFUNCTIONS;
  RetVal: integer;
  EmptyFileNames: PAnsiChar;
begin
  // Set all options for Unzip
  Options.ExtractOnlyNewer := 0;
  Options.SpaceToUnderscore := 0;
  Options.PromptToOverwrite := 0;
  Options.fQuiet := 0; // 1 = few messages, 2 = no messages, 0 = all messages -- TODO: change this to 1
  Options.ncflag := 0; // write to STDOUT if true; perhaps set to true?
  Options.ntflag := 1; // test zip file
  Options.nvflag := 1; // verbose listing -- TODO: set to 0
  Options.nfflag := 0; // "freshen" (replace existing files by newer versions)
  Options.nzflag := 0; // display zip file comment
  Options.ndflag := 1; // retain (create) subdirectories when extracting; 1 = "safe" usage of paths in filenames (skip "../")
  Options.noflag := 1; // true if you are to always over-write files, false if not
  Options.naflag := 0; // do end-of-line translation
  Options.nZIflag := 0; // get zip info if true
  Options.C_flag := 1; // be case insensitive if TRUE
  Options.fPrivilege := 1; // 1 => restore Acl's, 2 => Use privileges
  Options.lpszZipFN := PAnsiChar(Zipfile);
  Options.lpszExtractDir := PAnsiChar(TargetPath);

  // Specify the callbacks
  Callbacks.print := UnzipPrint;
  Callbacks.sound := UnzipSound;
  Callbacks.replace := UnzipReplace;
  Callbacks.password := UnzipPassword;
  Callbacks.SendApplicationMessage := UnzipMessage;
  Callbacks.ServCallBk := UnzipService;

  // Perform the extraction
  EmptyFileNames := nil;
  RetVal := Unzip.Wiz_SingleEntryUnzip(0, EmptyFileNames, 0, EmptyFileNames, Options, Callbacks);
  if RetVal <> 0 then begin
    raise Exception.CreateFmt('Error during extraction of "%s" to "%s":'#13#10'%s', [Zipfile, TargetPath, string(Unzip.Wiz_ErrorToStr(RetVal))]);
  end;

  // Return the number of files extracted
  Result := Callbacks.NumMembers;
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeUpdater.InstallLatest(Level: TReleaseLevel): TVersionNumber;
var
  Package: TFilePath;
begin
  Package := DownloadLatest(Level);
  InstallPackage(Package);
end;

{ ------------------------------------------------------------------------------------------------ }

procedure TZTreeUpdater.InstallPackage(Package: TPackageName);
var
  ExtractionPath: TFilePath;
  SEI: ShellExecuteInfo;
begin
  if Pos('\', Package) = 0 then begin
    Package := FBackupRootPath + Package;
  end;
  if SameText(ExtractFileExt(Package), '.zip') then begin
    ExtractionPath := FTempPath + ChangeFileExt(ExtractFileName(Package), '');
    ForceDirectories(ExtractionPath);
    ExtractFilesFromZip(Package, ExtractionPath);
    if BackupCurrentVersion then begin
      // TODO: if some files cannot be overwritten, then rename the target file(s) by inserting
      //  _vN.NN (current version) just before the extension and try again; if it's now successful,
      //  then append the (renamed) file to
      //    HKLM\System\CurrentControlSet\Control\Session Manager\PendingFileRenameOperations
      CopyFiles(ExtractionPath, FHomePath, True, FCurrentVersion);
    end;
    // TODO: if all files were successfully copied, then we can delete the temporary extraction dir
    // TODO: IF NOT, THEN NOT!
  end else begin
    SEI.cbSize := sizeof(SEI);
    SEI.fMask := SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
    SEI.Wnd := GetDesktopWindow;
    SEI.lpVerb := nil;
    SEI.lpFile := PAnsiChar(Package);
    SEI.lpParameters := nil;
    SEI.lpDirectory := nil;
    SEI.nShow := SW_SHOWNORMAL;
    if not ShellExecuteEx(@SEI) then begin
      RaiseLastOSError;
    end;
    if SEI.hProcess <> 0 then begin
      // Now wait for it to finish
      WaitForSingleObject(SEI.hProcess, INFINITE);
    end;
  end;
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeUpdater.GetPackageLocalPath(Package: TPackageName): TFilePath;
begin
  Result := FBackupRootPath + Package;
  if not FileExists(Result) then begin
    Result := '';
  end;
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeUpdater.PackageOfVersion(Version: TVersionNumber): TPackageName;
begin

end;

{ ------------------------------------------------------------------------------------------------ }

procedure TZTreeUpdater.RefreshUpdateIni;
begin
  if not Assigned(FUpdateIni) then begin
    FUpdateIniPath := DownloadFile('http://www.zedtek.com/download/update.ini', FTempPath);
    if not FileExists(FUpdateIniPath) then begin
      raise Exception.Create('Could not download list of latest versions!');
    end;
    FUpdateIni := TIniFile.Create(FUpdateIniPath);
  end;
end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeUpdater.RollbackInstall: TVersionNumber;
begin

end;

{ ------------------------------------------------------------------------------------------------ }

function TZTreeUpdater.VersionOfPackage(Package: TPackageName): TVersionNumber;
begin

end;

{ ------------------------------------------------------------------------------------------------ }

procedure TZTreeUpdater.SetHomePath(const Value: TFilePath);
begin
  if ForceDirectories(Value) = false then begin
    RaiseLastOSError(ERROR_PATH_NOT_FOUND);
  end;
  if FBackupRootPath = IncludeTrailingPathDelimiter(FHomePath + 'Versions') then begin
    FBackupRootPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(Value) + 'Versions');
  end;
  FHomePath := IncludeTrailingPathDelimiter(Value);
  FCurrentVersion := '';
  DetermineVersion;
end;

{ ------------------------------------------------------------------------------------------------ }

procedure TZTreeUpdater.SetBackupPath(const Value: TFilePath);
begin
  if ForceDirectories(Value) = false then begin
    RaiseLastOSError(ERROR_PATH_NOT_FOUND); 
  end;
  FBackupRootPath := IncludeTrailingPathDelimiter(Value);
end;

end.


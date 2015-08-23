program ZTreeWinUpdater;

{$APPTYPE CONSOLE}

{%File '..\doc\ZTreeUpdater.txt'}
{%File '..\doc\windll.txt'}

uses
  SysUtils,
  U_ZTreeUpdater in 'U_ZTreeUpdater.pas',
  Unzip in 'common\Unzip.pas',
  RunConsoleUnit in 'common\RunConsoleUnit.pas',
  U_ZTreeWinUpdater in 'U_ZTreeWinUpdater.pas';

var
  Activity: string;
  ZUpdater: TZTreeWinUpdater;

  procedure SetActivity(NewActivity: string);
  begin
    if Length(Activity) > 0 then begin
      WriteLn(' Done.');
    end;
    Activity := NewActivity;
    Write(Format('%s: %s... ', [FormatDateTime('hh:nn:ss', Now), Activity]));
  end;
begin
  if ParamCount = 0 then begin
    WriteLn('ZTreeWinUpdater <zipfile> [-home <homedir>] [-backup <backuppath>]');
    WriteLn('ZTreeWinUpdater [-home <homedir>] [-proxy <server>] [-beta|-zeta] [-version <current version>]');
    WriteLn('ZTreeWinUpdater -rollback [<backup name>] [-home <homedir>] [-backup <backuppath>] [-version <current version>]');
    Exit;
  end;

  Activity := '';
  try
    SetActivity('initializing');
    ZUpdater := TZTreeWinUpdater.Create;
    try
      ZUpdater.Overwrite := True;
      case ZUpdater.Action of
        uaUpdate: begin // ---
          if ZUpdater.UpdateFile = '' then begin
            SetActivity('downloading the latest version');
            if ZUpdater.DownloadLatest = false then begin
              Writeln(Format('Unable to download the latest version.', []));
              Activity := '';
            end;
          end;

          SetActivity('backing up the current version');
          ZUpdater.BackupCurrent;

          SetActivity('installing the latest version');
          ZUpdater.InstallLatest;
        end;
        uaRollback: begin // ---
          if ZUpdater.TargetVersion = '' then begin
            SetActivity(Format('rolling back from version %s', [ZUpdater.CurrentVersion]));
          end else begin
            SetActivity(Format('rolling back from version %s to version %s', [ZUpdater.CurrentVersion, ZUpdater.TargetVersion]));
          end;
          ZUpdater.Rollback(ZUpdater.TargetVersion);
        end;
      end;
      WriteLn('Done.');
    finally
      ZUpdater.Free;
    end;
  except
    on E: Exception do begin
      WriteLn(Format('Ouch!  Something went wrong while %s:'#13#10'[%s] %s', [Activity, E.ClassName, StringReplace(E.Message, #10, #10#9, [rfReplaceAll])])); 
    end;
  end;

end.

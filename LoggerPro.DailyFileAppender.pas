unit LoggerPro.DailyFileAppender;
{ <@abstract(The unit to include if you want to use @link(TLoggerProDailyFileAppender))
  @author(Claudio Villaschi) }

{$IF Defined(Android) or Defined(iOS)}
{$DEFINE MOBILE}
{$ENDIF}

interface

uses
  LoggerPro,
  LoggerPro.FileAppender,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes;

type
  TDailyFileKey = record
    Tag: string;
    Timestamp: TDateTime;
    function Hash: Integer;
    function TimestampKey: LongWord;
    class operator Equal(a: TDailyFileKey; b: TDailyFileKey): Boolean;
  end;

type
  {
    @abstract(Logs to daily file using one different file for each different TAG used.)
    @author(Claudio Villaschi - cvillaschi@gmail.com)
    Implements a daily based log file.
    It is an adapted copy of the @link(TLoggerProFileAppender) class written by Daniele Teti.
  }

  { @abstract(The daily file appender)
    To learn how to use this appender, check the sample @code(file_appender.dproj)
  }
  TLoggerProDailyFileAppender = class(TLoggerProAppenderBase)
  private
    fFormatSettings: TFormatSettings;
    fWritersDictionaryComparer: IEqualityComparer<TDailyFileKey>;
    fWritersDictionary: TObjectDictionary<TDailyFileKey, TStreamWriter>;
    fLogFormat: string;
    fLogFileNameFormat: string;
    fDailyFileAppenderOptions: TFileAppenderOptions;
    fLogsFolder: string;
    fEncoding: TEncoding;
    fOnLogRow: TFileAppenderLogRow;
    function CreateWriter(const aFileName: string): TStreamWriter;
    procedure AddWriter(const aLogItem: TLogItem; var lWriter: TStreamWriter; var lLogFileName: string);
    procedure CloseUnusedWriters(const aLogItem: TLogItem);
  protected
    function GetLogFileName(const aKey: TDailyFileKey): string;
    procedure InternalWriteLog(const aStreamWriter: TStreamWriter; const aValue: string); inline;
  public const
    { @abstract(Defines the default format string used by the @link(TLoggerProDailyFileAppender).)
      The positional parameters are the followings:
      @orderedList(
      @itemSetNumber 0
      @item TimeStamp
      @item ThreadID
      @item LogType
      @item LogMessage
      @item LogTag
      )
    }
    DEFAULT_LOG_FORMAT = '%0:s [TID %1:10u][%2:-8s] %3:s [%4:s]';
    { @abstract(Defines the default format string used by the @link(TLoggerProDailyFileAppender).)
      The positional parameters are the followings:
      @orderedList(
      @item SetNumber 0
      @item ModuleName
      @item YearMonthDay
      @item LogTag
      )
    }
    DEFAULT_FILENAME_FORMAT = '%s.%s.%s.log';
    { @abstract(Milliseconds to wait between the RETRY_COUNT times. }
    RETRY_DELAY = 200;
    { @abstract(How much times we have to retry if the file is locked?. }
    RETRY_COUNT = 5;
    constructor Create(aLogsFolder: string = ''; aDailyFileAppenderOptions: TFileAppenderOptions = [];
      aLogFileNameFormat: string = DEFAULT_FILENAME_FORMAT; aLogFormat: string = DEFAULT_LOG_FORMAT; aEncoding: TEncoding = nil);
      reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); overload; override;
    property OnLogRow: TFileAppenderLogRow read fOnLogRow write fOnLogRow;
  end;

implementation

uses
  System.IOUtils,
  System.Hash,
  System.DateUtils,
{$IF Defined(Android)}
  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
{$ENDIF}
  idGlobal;

{ TLoggerProDailyFileAppender }

function TLoggerProDailyFileAppender.GetLogFileName(const aKey: TDailyFileKey): string;
var
  lExt: string;
  lModuleName: string;
  lPath: string;
  lFormat: string;
begin
{$IF Defined(Android)}
  lModuleName := TAndroidHelper.ApplicationTitle.Replace(' ', '_', [rfReplaceAll]);
{$ENDIF}
{$IF not Defined(Mobile)}
  lModuleName := TPath.GetFileNameWithoutExtension(GetModuleName(HInstance));
{$ENDIF}
{$IF Defined(IOS)}
  raise Exception.Create('Platform not supported');
{$ENDIF}
  lFormat := fLogFileNameFormat;

  if TFileAppenderOption.IncludePID in fDailyFileAppenderOptions then
    lModuleName := lModuleName + '_pid_' + IntToStr(CurrentProcessId).PadLeft(6, '0');

  lPath := fLogsFolder;
  lExt := Format(lFormat, [lModuleName, akey.TimestampKey.ToString, aKey.Tag]).Replace('..', '.', [rfReplaceAll]);
  Result := TPath.Combine(lPath, lExt);
end;

procedure TLoggerProDailyFileAppender.Setup;
begin
  if fLogsFolder = '' then
  begin
{$IF (Defined(MSWINDOWS) or Defined(POSIX)) and (not Defined(MOBILE))}
    fLogsFolder := TPath.GetDirectoryName(GetModuleName(HInstance));
{$ENDIF}
{$IF Defined(Android) or Defined(IOS)}
    fLogsFolder := TPath.GetSharedDocumentsPath();
{$ENDIF}
  end;
  if not TDirectory.Exists(fLogsFolder) then
    TDirectory.CreateDirectory(fLogsFolder);

  fFormatSettings := LoggerPro.GetDefaultFormatSettings;
  fWritersDictionaryComparer := TDelegatedEqualityComparer<TDailyFileKey>.Create(
    function(const Left, Right: TDailyFileKey): Boolean { TEqualityComparison<TDailyFileKey> }
    begin
      Result := Left = Right;
    end,
    function(const Value: TDailyFileKey): Integer { THasher<TDailyFileKey> }
    begin
      Result := Value.Hash;
    end);
  fWritersDictionary := TObjectDictionary<TDailyFileKey, TStreamWriter>.Create([doOwnsValues], fWritersDictionaryComparer);
end;

procedure TLoggerProDailyFileAppender.TearDown;
begin
  fWritersDictionary.Free;
end;

procedure TLoggerProDailyFileAppender.InternalWriteLog(const aStreamWriter: TStreamWriter; const aValue: string);
begin
  aStreamWriter.WriteLine(aValue);
  aStreamWriter.Flush;
end;

procedure TLoggerProDailyFileAppender.WriteLog(const aLogItem: TLogItem);
var
  lKey: TDailyFileKey;
  lWriter: TStreamWriter;
  lLogFileName: string;
  lLogRow: string;
begin
  lKey.Timestamp := aLogItem.TimeStamp;
  lKey.Tag := aLogItem.LogTag;

  if not fWritersDictionary.TryGetValue(lKey, lWriter) then
  begin
    CloseUnusedWriters(aLogItem);

    AddWriter(aLogItem, lWriter, lLogFileName);
  end;

  if not Assigned(fOnLogRow) then
  begin
    InternalWriteLog(lWriter, Format(fLogFormat, [datetimetostr(aLogItem.TimeStamp, fFormatSettings), aLogItem.ThreadID,
      aLogItem.LogTypeAsString, aLogItem.LogMessage, aLogItem.LogTag]));
  end
  else
  begin
    fOnLogRow(aLogItem, lLogRow);
    InternalWriteLog(lWriter, lLogRow);
  end;
end;

procedure TLoggerProDailyFileAppender.AddWriter(const aLogItem: TLogItem; var lWriter: TStreamWriter; var lLogFileName: string);
var
  lKey: TDailyFileKey;
begin
  lKey.Timestamp := aLogItem.TimeStamp;
  lKey.Tag := aLogItem.LogTag;

  lLogFileName := GetLogFileName(lKey);
  lWriter := CreateWriter(lLogFileName);
  fWritersDictionary.Add(lKey, lWriter);
end;

procedure TLoggerProDailyFileAppender.CloseUnusedWriters(const aLogItem: TLogItem);
var
  lLogItemKey: TDailyFileKey;
  lKeyIndex: Integer;
  lDictionaryKey: TDailyFileKey;
begin
  lLogItemKey.Timestamp := aLogItem.TimeStamp;
  lLogItemKey.Tag := aLogItem.LogTag;

  if fWritersDictionary.Keys.Count > 0 then
    for lKeyIndex := Pred(fWritersDictionary.Keys.Count) downto 0 do
    begin
      lDictionaryKey := fWritersDictionary.Keys.ToArray[lKeyIndex];
      if lDictionaryKey.TimestampKey < lLogItemKey.TimestampKey then
        fWritersDictionary.Remove(lDictionaryKey);
    end;
end;

constructor TLoggerProDailyFileAppender.Create(aLogsFolder: string; aDailyFileAppenderOptions: TFileAppenderOptions;
  aLogFileNameFormat: string; aLogFormat: string; aEncoding: TEncoding);
begin
  inherited Create;
  fLogsFolder := aLogsFolder;
  fLogFormat := aLogFormat;
  fLogFileNameFormat := aLogFileNameFormat;
  fDailyFileAppenderOptions := aDailyFileAppenderOptions;
  if Assigned(aEncoding) then
    fEncoding := aEncoding
  else
    fEncoding := TEncoding.DEFAULT;
end;

function TLoggerProDailyFileAppender.CreateWriter(const aFileName: string): TStreamWriter;
var
  lFileStream: TFileStream;
  lFileAccessMode: Word;
  lRetries: Integer;
begin
  lFileAccessMode := fmOpenWrite or fmShareDenyNone;
  if not TFile.Exists(aFileName) then
    lFileAccessMode := lFileAccessMode or fmCreate;

  // If the file si still blocked by a precedent execution or
  // for some other reasons, we try to access the file for 5 times.
  // If after 5 times (with a bit of delay in between) the file is still
  // locked, then the exception is raised.
  lRetries := 0;
  while true do
  begin
    try
      lFileStream := TFileStream.Create(aFileName, lFileAccessMode);
      try
        lFileStream.Seek(0, TSeekOrigin.soEnd);
        Result := TStreamWriter.Create(lFileStream, fEncoding, 32);
        Result.AutoFlush := true;
        Result.OwnStream;
        Break;
      except
        lFileStream.Free;
        raise;
      end;
    except
      if lRetries = RETRY_COUNT then
      begin
        raise;
      end
      else
      begin
        Inc(lRetries);
        Sleep(RETRY_DELAY); // just wait a little bit
      end;
    end;
  end;
end;

{ TDailyFileKey }

class operator TDailyFileKey.Equal(a, b: TDailyFileKey): Boolean;
begin
  Result := (a.TimestampKey = b.TimestampKey) and (a.Tag = b.Tag);
end;

function TDailyFileKey.Hash: Integer;
var
  lTimestampKey: LongWord;
  lHash: THashBobJenkins;
begin
  lTimestampKey := Self.TimestampKey;

  lHash.Reset;
  lHash.Update(lTimestampKey, SizeOf(LongWord));
  lHash.Update(Self.Tag);

  Result := lHash.HashAsInteger;
end;

function TDailyFileKey.TimestampKey: LongWord;
var
  lYear: Word;
  lMonth: Word;
  lDay: Word;
begin
  DecodeDate(Self.Timestamp, lYear, lMonth, lDay);
  Result := lYear * 10000 +  lMonth * 100 + lDay;
end;

end.


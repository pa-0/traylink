unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Generics.Collections,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Mask;

type

  TRunFile = class
  public
    Path : string;
  end;

  TRunFileList = TObjectList<TRunFile>;


  TForm1 = class(TForm)
    TrayIcon1: TTrayIcon;
    TrayPopup: TPopupMenu;
    Configure1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    PathEdit: TLabeledEdit;
    ChangeTimer: TTimer;
    procedure ChangeTimerTimer(Sender: TObject);
    procedure Configure1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PathEditChange(Sender: TObject);
    procedure PopupItemClick(Sender: TObject);
    procedure TrayPopupPopup(Sender: TObject);
  private
    Files : TRunFileList;
    ShortcutPath : string;

    procedure BuildPopup;
    procedure ClearPopup;
    procedure UpdatePath;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  System.Win.Registry,
  WinApi.ShellAPI;

const
  DefaultItemCount = 3;
  RegRoot = 'Software\Zencode\TrayLink';

procedure TForm1.BuildPopup;

  procedure AddDirectory(Path : string; Parent : TMenuItem; Index : Integer);
  var
    sr : TSearchRec;
    p : string;
    f : TRunFile;
    mi : TMenuItem;

  begin
    if FindFirst(TPath.Combine(Path, '*.*'), faAnyFile, sr) = 0 then begin
      repeat
        if (sr.Name = '.') or (sr.Name = '..') then
          continue;

        p := TPath.Combine(ShortcutPath, sr.Name);
        mi := TMenuItem.Create(TrayPopup);
        if ExtractFileExt(sr.Name) = '.lnk' then begin
          mi.Caption := Copy(sr.Name, 1, Length(sr.Name) - 4);
        end else begin
          mi.Caption := sr.Name;
        end;
        Parent.Insert(Index, mi);

        if sr.Attr = faDirectory then begin
          AddDirectory(p, mi, 0);
        end else begin
          f := TRunFile.Create;
          f.Path := p;
          Files.Add(f);

          mi.Tag := NativeInt(f);
          mi.OnClick := PopupItemClick;
        end;
        Inc(Index);
      until FindNext(sr) <> 0;

      FindClose(sr);
    end;
  end;

begin
  Files.Clear;
  if Length(ShortCutPath) = 0 then
    Exit;

  AddDirectory(ShortcutPath, TrayPopup.Items, 0);
end;

procedure TForm1.ChangeTimerTimer(Sender: TObject);
begin
  ChangeTimer.Enabled := False;
  ClearPopup;
  UpdatePath;
end;

procedure TForm1.ClearPopup;
var
  mi : TMenuItem;

begin
  while TrayPopup.Items.Count > DefaultItemCount do begin
    mi := TrayPopup.Items[0];
    TrayPopup.Items.Delete(0);
    mi.Free;
  end;
  Files.Clear;
end;

procedure TForm1.Configure1Click(Sender: TObject);
begin
  PathEdit.Text := ShortcutPath;
  Show;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Exit1.Tag := 1;
  Close;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Exit1.Tag <> 1 then begin
    Hide;
    CanClose := False;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  reg : TRegistry;

begin
  Files := TRunFileList.Create;

  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly(RegRoot) then begin
      if reg.ValueExists('Path') then
        ShortcutPath := reg.ReadString('Path');
    end;
  finally
    reg.Free;
  end;

  TrayIcon1.Visible := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  TrayIcon1.Visible := False;
  Files.Free;
end;

procedure TForm1.PathEditChange(Sender: TObject);
begin
  ChangeTimer.Enabled := True;
end;

procedure TForm1.PopupItemClick(Sender: TObject);
var
  f : TRunFile;

begin
  f := TRunFile((Sender as TMenuItem).Tag);
  ShellExecute(0, 'open', PChar(f.Path), nil, nil, SW_SHOW);
end;

procedure TForm1.TrayPopupPopup(Sender: TObject);
begin
  if TrayPopup.Items.Count = DefaultItemCount then
    BuildPopup;
end;

procedure TForm1.UpdatePath;
var
  reg : TRegistry;

begin
  ShortcutPath := PathEdit.Text;

  reg := TRegistry.Create(KEY_WRITE);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if not reg.OpenKey(RegRoot, True) then
      RaiseLastOSError;
    reg.WriteString('Path', ShortcutPath);
  finally
    reg.Free;
  end;
end;

end.

unit richeditutils;

interface

uses vcl.comctrls, Graphics;

procedure RichEditColorize(RichEdit1:TRichEdit);


implementation

uses System.SysUtils, Winapi.Windows, Winapi.Messages, Clipbrd,
    vcl.forms, vcl.controls;

procedure RichEditColorize(RichEdit1:TRichEdit);
var
    iPos, iPosWord, iLen, n: integer;

begin
    RichEdit1.Hide;
    iLen := Length(RichEdit1.Lines.Text);

    iPos := 0;
    iPos := RichEdit1.FindText('#', iPos, iLen, []);
    while iPos <> -1 do
    begin
        iPosWord := RichEdit1.FindText(#13, iPos, iLen, []);
        RichEdit1.SelStart := iPos;
        RichEdit1.SelLength := iPosWord - iPos;
        RichEdit1.SelAttributes.Color := clGray;
        iPos := RichEdit1.FindText('#', iPosWord, iLen, []);
    end;

    iPos := 0;
    iPos := RichEdit1.FindText('[', iPos, iLen, []);
    while iPos <> -1 do
    begin
        iPosWord := RichEdit1.FindText(']', iPos, iLen, []);
        RichEdit1.SelStart := iPos;
        RichEdit1.SelLength := iPosWord - iPos + 1;
        RichEdit1.SelAttributes.Color := clNavy;
        iPos := RichEdit1.FindText('[', iPosWord, iLen, []);
    end;

    iPos := 0;
    iPos := RichEdit1.FindText('=', iPos, iLen, []);
    while iPos > 3 do
    begin
        RichEdit1.SelStart := iPos - 2;
        RichEdit1.SelLength := 2;
        while RichEdit1.SelText[1] <> ' ' do
        begin
            n := RichEdit1.SelLength;
            RichEdit1.SelStart := RichEdit1.SelStart - 1;
            RichEdit1.SelLength := n + 1;
        end;
        RichEdit1.SelAttributes.Color := clGreen;

        RichEdit1.SelStart := iPos + 1;
        RichEdit1.SelLength := 2;
        while RichEdit1.SelText[Length(RichEdit1.SelText)] <> #13 do
        begin
            n := RichEdit1.SelLength;
            RichEdit1.SelLength := n + 1;
        end;
        RichEdit1.SelAttributes.Color := clMaroon;

        iPos := RichEdit1.FindText('=', iPos + 1, iLen, []);
    end;
    RichEdit1.Show;
    RichEdit1.SelLength := 0;
end;

end.

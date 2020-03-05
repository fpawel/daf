package report

import (
	"fmt"
	"github.com/fpawel/daf/internal/data"
	"github.com/powerman/structlog"
	"io"
	"os"
	"path/filepath"
)

func WriteProductToFile(dirname string, party data.Party, p data.Product) error {
	file, err := os.Create(filepath.Join(dirname, fmt.Sprintf("%d_id%d.txt", p.Serial, p.ProductID)))
	defer log.ErrIfFail(file.Close)
	if err != nil {
		return err
	}
	if err := writeProductToFile(file, party, p); err != nil {
		return err
	}
	return nil
}

func writeProductToFile(w io.Writer, party data.Party, p data.Product) (err error) {
	Print := func(arg ...interface{}) {
		if err != nil {
			return
		}
		_, err = fmt.Fprint(w, arg...)
	}
	Printf := func(format string, arg ...interface{}) {
		if err != nil {
			return
		}
		_, err = fmt.Fprintf(w, format, arg...)
	}
	writeTable := func(t Table) {
		for _, row := range t {
			for _, cell := range row {
				text := cell.Text
				if cell.Color == "clRed" {
					text = "*" + text
				}
				Print(text, "\t")
			}
			Print("\n")
		}
		Print("\n")
	}

	Printf("ДАФ-М № %d\n\n", p.ProductID)
	Printf("Партия %d %s\n\n", party.PartyID, party.CreatedAt.Format("2006-01-02 15:04"))
	Printf("Шкала %v-%v\n\n", p.ScaleBegin, p.ScaleEnd)
	Print("Проверка погрешности\n\n")

	var t Table
	if err == nil {
		err = IndividualPassport1(p.ProductID, &t)
	}
	writeTable(t)
	Print("Журнал\n\n")
	if err == nil {
		err = IndividualPassport2(p.ProductID, &t)
	}
	writeTable(t)
	return
}

var log = structlog.New()

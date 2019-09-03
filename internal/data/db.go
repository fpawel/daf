package data

import (
	"database/sql"
	"github.com/fpawel/daf/internal"
	"github.com/fpawel/gohelp"
	"github.com/jmoiron/sqlx"
	"path/filepath"
)

//go:generate go run github.com/fpawel/gohelp/cmd/sqlstr/...

var (
	DB = func() *sqlx.DB {
		db := gohelp.OpenSqliteDBx(filepath.Join(internal.DataDir(), "daf.sqlite"))
		db.MustExec(SQLCreate)
		return db
	}()
)

func GetParty(partyID int64) (party Party) {
	if err := DB.Get(&party, `SELECT * FROM party WHERE party_id=?`, partyID); err != nil {
		panic(err)
	}
	return
}

func LastParty() (party Party) {
	err := DB.Get(&party, `SELECT * FROM last_party`)
	if err == sql.ErrNoRows {
		DB.MustExec(`INSERT INTO last_party DEFAULT VALUES`)
		err = DB.Get(&party, `SELECT * FROM last_party`)
	}
	if err != nil {
		panic(err)
	}
	return
}

func Products(partyID int64) (products []Product) {
	if err := DB.Select(&products, `
SELECT product_id, serial  FROM product 
WHERE party_id = ? 
ORDER BY product_id`, partyID); err != nil {
		panic(err)
	}
	return
}

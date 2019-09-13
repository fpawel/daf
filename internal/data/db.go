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
		db := gohelp.MustOpenSqliteDBx(filepath.Join(internal.DataDir(), "daf.sqlite"))
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

func GetPartyByProductID(productID int64) (party Party) {
	if err := DB.Get(&party, `
SELECT party.* 
FROM product
INNER JOIN party USING (party_id)
WHERE product_id=?`, productID); err != nil {
		panic(err)
	}
	return
}

func LastParty() (party Party) {
	err := DB.Get(&party, `SELECT * FROM last_party`)
	if err == sql.ErrNoRows {
		DB.MustExec(`
INSERT INTO party DEFAULT VALUES;
INSERT INTO product(party_id, serial) VALUES ((SELECT party_id FROM last_party), 1);
`)
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

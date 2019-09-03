package last_party

import (
	"fmt"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/data"
	"github.com/pelletier/go-toml"
)

type LastPartySvc struct{}

func (x *LastPartySvc) GetNewPartyTemplate(_ struct{}, r *string) error {
	return tomlStringify(r, addrSerials{
		{
			Addr:   1,
			Serial: 100,
		},
		{
			Addr:   2,
			Serial: 100,
		},
		{
			Addr:   3,
			Serial: 100,
		},
	})
}

func (x *LastPartySvc) CreateNewParty(s [1]string, _ *struct{}) error {
	var p addrSerials
	if err := toml.Unmarshal([]byte(s[0]), &p); err != nil {
		return err
	}
	if _, err := data.DB.Exec(`BEGIN TRANSACTION; INSERT INTO party DEFAULT VALUES;`); err != nil {
		return err
	}
	partyID := data.LastParty().PartyID
	c := cfg.GetConfig()
	for i, x := range p {
		if _, err := data.DB.Exec(`INSERT INTO product(party_id, serial) VALUES ( ?, ?)`, partyID, x.Serial); err != nil {
			data.DB.MustExec(`ROLLBACK;`)
			return fmt.Errorf("serial=%d: %v", x.Serial, err)
		}
		if i < len(c.Network) {
			c.Network[i].Addr = x.Addr
		}
	}
	data.DB.MustExec(`COMMIT;`)
	cfg.SetConfig(c)
	return nil
}

func tomlStringify(r *string, x interface{}) error {
	b, err := toml.Marshal(x)
	if err != nil {
		return err
	}
	*r = string(b)
	return nil
}

type addrSerial struct {
	Addr   modbus.Addr `toml:"addr" comment:"адрес прибора MODBUS"`
	Serial int         `toml:"serial" comment:"серийный номер прибора"`
}
type addrSerials = []addrSerial

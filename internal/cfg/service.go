package cfg

import (
	"fmt"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/data"
	"github.com/pelletier/go-toml"
)

type ConfigSvc struct{}

type guiSettings struct {
	Sets  GuiSettings        `toml:"sets" comment:"параметры связи и технологического процесса"`
	Party data.PartySettings `toml:"party" comment:"параметры текущей загрузки"`
}

func getSets() guiSettings {
	return guiSettings{
		Sets:  GetConfig().GuiSettings,
		Party: data.LastParty().PartySettings,
	}
}

func setSets(x guiSettings) {
	p := x.Party
	data.DB.MustExec(`
UPDATE party 
	SET product_type = ?, 
	    c1 = ?, 
	    c2 = ?, 
	    c3 = ?, 
	    c4 = ?,
	    component = ?,
	    scale = ?,
	    thr1_prod = ?,
	    thr2_prod = ?,
	    thr1_test = ?,
	    thr2_test = ?,
	    abs_err_rng = ?,
	    abs_err_lim = ?,
	    rel_err_lim = ?
WHERE party_id = (SELECT party_id FROM party)`,
		p.ProductType, p.C1, p.C2, p.C3, p.C4, p.Component, p.Scale,
		p.Thr1Prod, p.Thr2Prod,
		p.Thr1Test, p.Thr2Test,
		p.AbsErrRng, p.AbsErrLim, p.RelErrLim)
	c := GetConfig()
	c.GuiSettings = x.Sets
	SetConfig(c)
}

func (x *ConfigSvc) GetConfigToml(_ struct{}, r *string) error {
	return tomlStringify(r, getSets())
}

func (_ *ConfigSvc) SetConfigToml(s [1]string, r *string) error {
	var p guiSettings
	if err := toml.Unmarshal([]byte(s[0]), &p); err != nil {
		return err
	}
	setSets(p)
	return tomlStringify(r, getSets())
}

func (_ *ConfigSvc) SetDefault(_ struct{}, r *string) error {
	SetConfig(defaultConfig)
	return tomlStringify(r, getSets())
}

func (_ *ConfigSvc) SetPlaceChecked(x struct {
	Place   int
	Checked bool
}, _ *struct{}) error {
	c := GetConfig()
	if x.Place < 0 || x.Place >= len(c.Network) {
		return fmt.Errorf("номер места %d: должен быть в диапазоне от 0 до %d", x.Place, len(c.Network)-1)
	}
	if x.Place >= len(c.Network) {
		xs := make([]Place, x.Place+1)
		copy(xs, c.Network)
		c.Network = xs
	}
	c.Network[x.Place].Checked = x.Checked
	SetConfig(c)
	return nil
}

func (_ *ConfigSvc) SetConfig(x struct{ C GuiSettings }, _ *struct{}) error {
	c := GetConfig()
	c.GuiSettings = x.C
	SetConfig(c)
	return nil
}

func (_ *ConfigSvc) GetConfig(_ struct{}, r *GuiSettings) error {
	*r = GetConfig().GuiSettings
	return nil
}

func (_ *ConfigSvc) SetPlaceAddr(x struct {
	Place int
	Addr  modbus.Addr
}, _ *struct{}) error {

	c := GetConfig()
	if x.Place >= len(c.Network) {
		xs := make([]Place, x.Place+1)
		copy(xs, c.Network)
		c.Network = xs
	}
	if x.Addr < 1 || x.Addr > 127 {
		return fmt.Errorf("адрес MODBUS %d: должен быть от 1 до 127", x.Addr)
	}
	for place, p := range c.Network {
		if p.Addr == x.Addr && place != x.Place {
			return fmt.Errorf("адрес MODBUS %d: дублирует адрес места %d", x.Addr, place)
		}
	}
	c.Network[x.Place].Addr = x.Addr
	SetConfig(c)
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

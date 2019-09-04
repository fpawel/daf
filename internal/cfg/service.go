package cfg

import (
	"errors"
	"github.com/fpawel/daf/internal/data"
	"github.com/pelletier/go-toml"
)

type ConfigSvc struct{}

type appSettings struct {
	Main  Config             `toml:"main" comment:"параметры связи и технологического процесса"`
	Party data.PartySettings `toml:"party" comment:"параметры текущей загрузки"`
}

func getAppSets() appSettings {
	return appSettings{
		Main:  GetConfig(),
		Party: data.LastParty().PartySettings,
	}
}

func setAppSets(x appSettings) {
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
	SetConfig(x.Main)
}

func (x *ConfigSvc) GetConfigToml(_ struct{}, r *string) error {
	return tomlStringify(r, getAppSets())
}

func (_ *ConfigSvc) SetConfigToml(s [1]string, r *string) error {
	var p appSettings
	if err := toml.Unmarshal([]byte(s[0]), &p); err != nil {
		return err
	}
	setAppSets(p)
	return tomlStringify(r, getAppSets())
}

func (_ *ConfigSvc) SetDefault(_ struct{}, r *string) error {
	SetConfig(defaultConfig)
	return tomlStringify(r, getAppSets())
}

func (_ *ConfigSvc) SetPlaceChecked(x struct {
	Place   int
	Checked bool
}, _ *struct{}) error {
	c := GetConfig()
	if x.Place < 0 {
		return errors.New("place must be positive number")
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

func (_ *ConfigSvc) SetConfig(x struct{ C Config }, _ *struct{}) error {
	SetConfig(x.C)
	return nil
}

func (_ *ConfigSvc) GetConfig(_ struct{}, r *Config) error {
	*r = GetConfig()
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
package cfg

import (
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
WHERE party_id = (SELECT party_id FROM last_party)`,
		p.ProductType, p.C1, p.C2, p.C3, p.C4, p.Component, p.Scale,
		p.Thr1Prod, p.Thr2Prod,
		p.Thr1Test, p.Thr2Test,
		p.AbsErrRng, p.AbsErrLim, p.RelErrLim)
	SetConfig(x.Main)
}

func (x *ConfigSvc) Get(_ struct{}, r *string) error {
	return tomlStringify(r, getAppSets())
}

func (_ *ConfigSvc) Set(s [1]string, r *string) error {
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

func tomlStringify(r *string, x interface{}) error {
	b, err := toml.Marshal(x)
	if err != nil {
		return err
	}
	*r = string(b)
	return nil
}

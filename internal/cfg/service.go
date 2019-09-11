package cfg

import (
	"github.com/fpawel/daf/internal/data"
	"github.com/pelletier/go-toml"
)

type ConfigSvc struct{}

type tomlConfig struct {
	App   AppConfig        `toml:"app" comment:"параметры связи и технологического процесса"`
	Party data.PartyConfig `toml:"party" comment:"параметры текущей загрузки"`
}

func getTomlConfig() tomlConfig {
	return tomlConfig{
		App:   GetConfig().AppConfig,
		Party: data.LastParty().PartyConfig,
	}
}

func (x *ConfigSvc) GetConfigToml(_ struct{}, r *string) error {
	return tomlStringify(r, getTomlConfig())
}

func (_ *ConfigSvc) SetConfigToml(s [1]string, r *string) error {
	var p tomlConfig
	if err := toml.Unmarshal([]byte(s[0]), &p); err != nil {
		return err
	}
	if _, err := data.DB.NamedExec(`
UPDATE party 
	SET product_type = :product_type, 
	    c1 = :c1, 
	    c2 = :c2, 
	    c3 = :c3, 
	    c4 = :c4,
	    abs_error_limit1 = :abs_error_limit1, 
	    abs_error_limit2 = :abs_error_limit2, 
	    abs_error_limit3 = :abs_error_limit3, 
	    abs_error_limit4 = :abs_error_limit4,
	    component = :component,
	    scale_begin = :scale_begin,
	    scale_end = :scale_end,	    
	    variation_limit3 = :variation_limit3
WHERE party_id = (SELECT party_id FROM party)`, p.Party); err != nil {
		panic(err)
	}
	c := GetConfig()
	c.AppConfig = p.App
	ApplyConfig(c)
	return tomlStringify(r, getTomlConfig())
}

func (_ *ConfigSvc) SetDefault(_ struct{}, r *string) error {
	ApplyConfig(defaultConfig)
	return tomlStringify(r, getTomlConfig())
}

func (_ *ConfigSvc) SetPlaceChecked(x struct {
	Place   int
	Checked bool
}, _ *struct{}) error {
	c := GetConfig()
	c.EnsurePlace(x.Place)
	c.Network[x.Place].Checked = x.Checked
	ApplyConfig(c)
	return nil
}

func (_ *ConfigSvc) SetConfig(x struct{ C AppConfig }, _ *struct{}) error {
	c := GetConfig()
	c.AppConfig = x.C
	ApplyConfig(c)
	return nil
}

func (_ *ConfigSvc) GetConfig(_ struct{}, r *AppConfig) error {
	*r = GetConfig().AppConfig
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

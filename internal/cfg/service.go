package cfg

import (
	"github.com/fpawel/daf/internal/data"
	"gopkg.in/yaml.v3"
)

type ConfigSvc struct{}

type configYaml struct {
	App   AppConfig        `yaml:"app"`
	Party data.PartyConfig `yaml:"party"`
}

func getConfigYaml() configYaml {
	return configYaml{
		App:   GetConfig().AppConfig,
		Party: data.LastParty().PartyConfig,
	}
}

func (x *ConfigSvc) GetConfigYaml(_ struct{}, r *string) error {
	return yamlStringify(r, getConfigYaml())
}

func (_ *ConfigSvc) SetConfigYaml(s [1]string, r *string) error {
	var p configYaml
	if err := yaml.Unmarshal([]byte(s[0]), &p); err != nil {
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
	    variation_limit3 = :variation_limit3
WHERE party_id = (SELECT party_id FROM last_party)`, p.Party); err != nil {
		panic(err)
	}
	c := GetConfig()
	c.AppConfig = p.App
	ApplyConfig(c)
	return yamlStringify(r, getConfigYaml())
}

func (_ *ConfigSvc) SetDefault(_ struct{}, r *string) error {
	ApplyConfig(defaultConfig)
	return yamlStringify(r, getConfigYaml())
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

func yamlStringify(r *string, x interface{}) error {
	b, err := yaml.Marshal(x)
	if err != nil {
		return err
	}
	*r = string(b)
	return nil
}

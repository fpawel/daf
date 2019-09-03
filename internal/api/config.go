package api

import (
	"github.com/fpawel/mil82/internal/api/types"
	"github.com/fpawel/mil82/internal/cfg"
)

type ConfigSvc struct{}

func (_ *ConfigSvc) ProductTypeTemperatures(productType [1]string, r *types.TempPlusMinus) error {
	for _, t := range cfg.Get().ProductTypes {
		if t.Name() == productType[0] {
			r.TempMinus = t.TempMinus
			r.TempPlus = t.TempPlus
		}
	}
	return nil
}

func (_ *ConfigSvc) ProductTypesNames(_ struct{}, r *[]string) error {
	for _, t := range cfg.Get().ProductTypes {
		*r = append(*r, t.Name())
	}
	return nil
}

func (_ *ConfigSvc) UserAppSetts(_ struct{}, r *cfg.UserAppSettings) error {
	*r = cfg.Get().UserAppSettings
	return nil
}

func (_ *ConfigSvc) SetUserAppSetts(x struct{ A cfg.UserAppSettings }, _ *struct{}) error {
	c := cfg.Get()
	c.UserAppSettings = x.A
	cfg.Set(c)
	return nil
}

func (_ *ConfigSvc) Vars(_ struct{}, vars *[]cfg.Var) error {
	*vars = cfg.Get().Vars
	return nil
}

func (_ *ConfigSvc) SetPlaceChecked(x struct {
	Place   int
	Checked bool
}, _ *struct{}) error {
	c := cfg.Get()
	c.SetPlaceChecked(x.Place, x.Checked)
	cfg.Set(c)
	return nil
}

package cfg

import (
	"encoding/json"
	"fmt"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/gohelp/must"
	"github.com/fpawel/mil82/internal/data"
	"io/ioutil"
	"os"
	"path/filepath"
	"sync"
	"time"
)

type Config struct {
	UserAppSettings
	PlacesUncheck []int
	Vars          []Var
	ProductTypes  []ProductType
}

type Var struct {
	Code modbus.Var `db:"var"`
	Name string     `db:"name"`
}

type UserAppSettings struct {
	ComportProducts,
	ComportTemperature,
	ComportGas string
	TemperatureMinus,
	TemperaturePlus float64
	BlowGasMinutes,
	BlowAirMinutes,
	HoldTemperatureMinutes int
	InterrogateProductVarIntervalMillis int
}

func (x UserAppSettings) InterrogateProductVarInterval() time.Duration {
	return time.Duration(x.InterrogateProductVarIntervalMillis) * time.Millisecond
}

func Set(v Config) {
	mu.Lock()
	defer mu.Unlock()
	must.UnmarshalJSON(must.MarshalJSON(&v), &config)
	must.WriteFile(fileName(), must.MarshalIndentJSON(&config, "", "    "), 0666)
	return
}

func Get() (result Config) {
	mu.Lock()
	defer mu.Unlock()
	must.UnmarshalJSON(must.MarshalJSON(&config), &result)
	return
}

func (c Config) PlaceChecked(place int) bool {
	for _, x := range c.PlacesUncheck {
		if x == place {
			return false
		}
	}
	return true
}

func (c *Config) SetPlaceChecked(place int, checked bool) {
	if checked {
		b := c.PlacesUncheck[:0]
		for _, x := range c.PlacesUncheck {
			if x != place {
				b = append(b, x)
			}
		}
		c.PlacesUncheck = b
	} else {
		c.PlacesUncheck = append(c.PlacesUncheck, place)
	}
}

func fileName() string {
	return filepath.Join(filepath.Dir(os.Args[0]), "config.json")
}

var (
	mu     sync.Mutex
	config = func() Config{
		c := Config{
			UserAppSettings: UserAppSettings{
				BlowGasMinutes:         5,
				BlowAirMinutes:         1,
				HoldTemperatureMinutes: 120,
				TemperatureMinus:       -60,
				TemperaturePlus:        80,
			},
			ProductTypes: []ProductType{
				{N1: 0, N2: 0, Component: CO2, Scale: Sc4, K4: 5, K14: 0.1, K45: 60, K35: 5, K50: 0, TempMinus: -40, TempPlus: 80},
				{N1: 0, N2: 1, Component: CO2, Scale: Sc10, K4: 5, K14: 0.1, K45: 60, K35: 5, K50: 0, TempMinus: -40, TempPlus: 80},
				{N1: 0, N2: 2, Component: CO2, Scale: Sc20, K4: 5, K14: 0.1, K45: 60, K35: 5, K50: 0, TempMinus: -40, TempPlus: 80},
				{N1: 1, N2: 0, Component: CH4, Scale: Sc100, K4: 7.5, K14: 0.5, K45: 60, K35: 5, K50: 0, TempMinus: -40, TempPlus: 80},
				{N1: 1, N2: 1, Component: CH4, Scale: Sc100, K4: 7.5, K14: 0.5, K45: 60, K35: 5, K50: 0, TempMinus: -60, TempPlus: 60},
				{N1: 2, N2: 0, Component: C3H8, Scale: Sc50, K4: 12.5, K14: 0.5, K45: 30, K35: 5, K50: 0, TempMinus: -40, TempPlus: 60},
				{N1: 2, N2: 1, Component: C3H8, Scale: Sc50, K4: 12.5, K14: 0.5, K45: 30, K35: 5, K50: 0, TempMinus: -60, TempPlus: 60},
				{N1: 3, N2: 0, Component: C3H8, Scale: Sc100, K4: 12.5, K14: 0.5, K45: 30, K35: 5, K50: 0, TempMinus: -40, TempPlus: 60},
				{N1: 3, N2: 1, Component: C3H8, Scale: Sc100, K4: 12.5, K14: 0.5, K45: 30, K35: 5, K50: 0, TempMinus: -60, TempPlus: 60},
				{N1: 4, N2: 0, Component: CH4, Scale: Sc100, K4: 7.5, K14: 0.5, K45: 60, K35: 5, K50: 0, TempMinus: -60, TempPlus: 80},
				{N1: 5, N2: 0, Component: C6H14, Scale: Sc50, K4: 1, K14: 30, K45: 30, K35: 5, K50: 0, TempMinus: 15, TempPlus: 80, ScaleBeg: 5},
				{N1: 10, N2: 0, Component: CO2, Scale: Sc4, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -40, TempPlus: 80},
				{N1: 10, N2: 1, Component: CO2, Scale: Sc10, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -40, TempPlus: 80},
				{N1: 10, N2: 2, Component: CO2, Scale: Sc20, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -40, TempPlus: 80},
				{N1: 10, N2: 3, Component: CO2, Scale: Sc4, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
				{N1: 10, N2: 4, Component: CO2, Scale: Sc10, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
				{N1: 10, N2: 5, Component: CO2, Scale: Sc20, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
				{N1: 11, N2: 0, Component: CH4, Scale: Sc100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -40, TempPlus: 80},
				{N1: 11, N2: 1, Component: CH4, Scale: Sc100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
				{N1: 13, N2: 0, Component: C3H8, Scale: Sc100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -40, TempPlus: 80},
				{N1: 13, N2: 1, Component: C3H8, Scale: Sc100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
				{N1: 14, N2: 0, Component: CH4, Scale: Sc100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
				{N1: 16, N2: 0, Component: C3H8, Scale: Sc100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
			},
		}

		if err := data.DB.Select(&c.Vars, `SELECT var, name FROM var ORDER BY var`); err != nil {
			panic(err)
		}

		b, err := ioutil.ReadFile(fileName())
		if err != nil {
			fmt.Println(err, "файл", fileName())
		}
		if err == nil {
			err = json.Unmarshal(b, &c)
			if err != nil {
				fmt.Println(err, "файл", fileName())
			}
		}
		return c
	}()
)

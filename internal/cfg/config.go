package cfg

import (
	"encoding/json"
	"fmt"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/gohelp/must"
	"io/ioutil"
	"os"
	"path/filepath"
	"sync"
)

type Config struct {
	GuiSettings
	Network []Place
}

type GuiSettings struct {
	ComportProducts        string `toml:"comport_products" comment:"СОМ порт приборов"`
	ComportHart            string `toml:"comport_hart" comment:"СОМ порт HART модема"`
	DurationBlowGasMinutes int    `toml:"duration_blow_gas" comment:"длительность продувки газа в минутах"`
	DurationBlowAirMinutes int    `toml:"duration_blow_air" comment:"длительность продувки воздуха в минутах"`
	PauseReadPlaceMillis   int    `toml:"pause_read_place" comment:"длительность паузы между опросом мест стенда"`
}

type Place struct {
	Addr    modbus.Addr `toml:"addr" comment:"адрес прибора MODBUS"`
	Checked bool        `toml:"check" comment:"true - опрашивать данный адрес, false - не опрашивать"`
}

func (x *Config) SetAddrAtPlace(place int, addr modbus.Addr) {

	if place >= len(x.Network) {
		xs := make([]Place, place+1)
		copy(xs, x.Network)
		x.Network = xs
	}
	x.Network[place].Addr = addr
}

func Save() {
	mu.Lock()
	defer mu.Unlock()
	save()
	return
}

func SetConfig(v Config) {
	mu.Lock()
	defer mu.Unlock()
	must.UnmarshalJSON(must.MarshalJSON(&v), &config)

	if len(config.Network) == 0 {
		config.Network = []Place{
			{
				Addr:    0,
				Checked: false,
			},
		}
	}

	save()
	return
}

func GetConfig() (result Config) {
	mu.Lock()
	defer mu.Unlock()
	must.UnmarshalJSON(must.MarshalJSON(&config), &result)
	if len(result.Network) == 0 {
		result.Network = []Place{
			{
				Addr:    0,
				Checked: false,
			},
		}
	}
	return
}

func fileName() string {
	return filepath.Join(filepath.Dir(os.Args[0]), "config.json")
}

func save() {
	must.WriteFile(fileName(), must.MarshalIndentJSON(&config, "", "    "), 0666)
}

var (
	mu            sync.Mutex
	defaultConfig = Config{
		GuiSettings: GuiSettings{
			ComportProducts:        "COM1",
			ComportHart:            "COM2",
			DurationBlowGasMinutes: 5,
			DurationBlowAirMinutes: 1,
		},
		Network: []Place{
			{
				Addr:    1,
				Checked: true,
			},
		},
	}
	config = func() Config {
		c := defaultConfig
		b, err := ioutil.ReadFile(fileName())
		if err != nil {
			fmt.Println(err, fileName())
			return c
		}
		if err = json.Unmarshal(b, &c); err != nil {
			fmt.Println(err, fileName())
		}
		return c
	}()
)

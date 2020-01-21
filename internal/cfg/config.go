package cfg

import (
	"encoding/json"
	"fmt"
	"github.com/fpawel/comm"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/pkg/must"
	"io/ioutil"
	"os"
	"path/filepath"
	"sync"
	"time"
)

type Config struct {
	AppConfig
	Network []Place
}

type AppConfig struct {
	ComportProducts        string  `toml:"comport_products" comment:"СОМ порт приборов"`
	ComportHart            string  `toml:"comport_hart" comment:"СОМ порт HART модема"`
	DurationBlowGasMinutes []int   `toml:"duration_blow_gas" comment:"длительности продувки газов в минутах"`
	DurationBlowOutMinutes int     `toml:"duration_blow_out" comment:"длительность продувки воздуха в минутах"`
	SoftVersion            byte    `toml:"soft_version" comment:"Контролирумое значение ВПО ДАФ-М"`
	SoftVersionID          uint16  `toml:"soft_version_id" comment:"Контролирумое значение цифрового идентификатора ВПО ДАФ-М"`
	Temperature            float64 `toml:"temperature" comment:"Температура,\"С"`
	Comm                   Comm    `toml:"comm" comment:"параметры приёмо-передачи оборудования"`
}

type DurationBlowGasMinutes []int

type Comm struct {
	Log    bool        `toml:"log" comment:"логгирование посылок COM портов"`
	Daf    comm.Config `toml:"daf" comment:"ДАФ-М"`
	Gas    comm.Config `toml:"gas" comment:"Газовый блок"`
	EN6408 comm.Config `toml:"en6408" comment:"Стенд ЭН8800-6408"`
	Hart   comm.Config `toml:"hart" comment:"HART протокол"`
}

type Place struct {
	Addr    modbus.Addr `toml:"addr" comment:"адрес прибора MODBUS"`
	Checked bool        `toml:"check" comment:"true - опрашивать данный адрес, false - не опрашивать"`
}

func (x *Config) EnsurePlace(place int) {
	if place < 0 {
		panic("place must be positive")
	}
	if place >= len(x.Network) {
		xs := make([]Place, place+1)
		n := copy(xs, x.Network)
		for i := range xs[n:] {
			xs[n:][i].Addr = 1
			xs[n:][i].Checked = true
		}
		x.Network = xs
	}
}

func Save() {
	mu.Lock()
	defer mu.Unlock()
	save()
	return
}

func ApplyConfig(v Config) {
	mu.Lock()
	defer mu.Unlock()
	must.UnmarshalJSON(must.MarshalJSON(&v), &config)
	comm.SetEnableLog(config.Comm.Log)
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
		AppConfig: AppConfig{
			ComportProducts:        "COM1",
			ComportHart:            "COM2",
			DurationBlowGasMinutes: []int{10, 5, 5, 5},
			DurationBlowOutMinutes: 1,
			SoftVersion:            1,
			SoftVersionID:          0x7116,
			Temperature:            20,
			Comm: Comm{
				Log: true,
				Daf: comm.Config{
					TimeoutEndResponse: 50 * time.Millisecond,
					TimeoutGetResponse: 700 * time.Millisecond,
					MaxAttemptsRead:    3,
				},
				Gas: comm.Config{
					TimeoutEndResponse: 50 * time.Millisecond,
					TimeoutGetResponse: time.Second,
					MaxAttemptsRead:    5,
				},
				EN6408: comm.Config{
					TimeoutEndResponse: 50 * time.Millisecond,
					TimeoutGetResponse: 3000 * time.Millisecond,
					MaxAttemptsRead:    5,
					Pause:              100 * time.Millisecond,
				},
				Hart: comm.Config{
					TimeoutEndResponse: 100 * time.Millisecond,
					TimeoutGetResponse: 2 * time.Second,
					MaxAttemptsRead:    5,
				},
			},
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

		comm.SetEnableLog(c.Comm.Log)

		return c
	}()
)

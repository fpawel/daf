package cfg

import (
	"encoding/json"
	"fmt"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/gohelp/must"
	"io/ioutil"
	"os"
	"path/filepath"
	"sync"
)

type Config struct {
	ComportProducts        string  `toml:"comport_products" comment:"СОМ порт приборов"`
	ComportHart            string  `toml:"comport_hart" comment:"СОМ порт HART модема"`
	DurationBlowGasMinutes int     `toml:"duration_blow_gas" comment:"длительность продувки газа в минутах"`
	DurationBlowAirMinutes int     `toml:"duration_blow_air" comment:"длительность продувки воздуха в минутах"`
	Network                []Place `toml:"network" comment:"сеть MODBUS"`
}

type Place struct {
	Place     int         `toml:"place" comment:"номер места"`
	Addr      modbus.Addr `toml:"addr" comment:"адрес прибора MODBUS"`
	Uncheck   bool        `toml:"uncheck" comment:"false - опрашивать данный адрес, true - не опрашивать"`
	Serial    int         `toml:"serial" comment:"серийный номер прибора, подключенного к данному месту. Только для чтения. Устанавливается при создании загрузки."`
	ProductID int64       `toml:"product_id" comment:"номер прибора, подключенного к данному месту. Только для чтения. Устанавливается при создании загрузки."`
}

func SetConfig(v Config) {
	mu.Lock()
	defer mu.Unlock()
	must.UnmarshalJSON(must.MarshalJSON(&v), &config)
	v.Network = v.Places()
	must.WriteFile(fileName(), must.MarshalIndentJSON(&config, "", "    "), 0666)
	return
}

func GetConfig() (result Config) {
	mu.Lock()
	defer mu.Unlock()
	must.UnmarshalJSON(must.MarshalJSON(&config), &result)
	result.Network = result.Places()
	return
}

func (c Config) PlaceAddr(place int) modbus.Addr {
	for i, x := range c.Network {
		if i == place {
			return x.Addr
		}
	}
	return 1
}

func (c Config) PlaceChecked(place int) bool {
	for i, x := range c.Network {
		if i == place {
			return !x.Uncheck
		}
	}
	return true
}

func (c Config) Places() (places []Place) {
	var products []data.Product
	if err := data.DB.Select(&products, `
SELECT product_id, serial  FROM product 
WHERE party_id = (SELECT party_id FROM last_party) 
ORDER BY product_id`); err != nil {
		panic(err)
	}
	places = make([]Place, len(products))
	for i := range products {
		places[i].Place = i
		places[i].Serial = products[i].Serial
		places[i].ProductID = products[i].ProductID
		if i < len(c.Network) {
			places[i].Addr = c.Network[i].Addr
			places[i].Uncheck = c.Network[i].Uncheck
		}
	}
	return
}

func fileName() string {
	return filepath.Join(filepath.Dir(os.Args[0]), "config.json")
}

var (
	mu            sync.Mutex
	defaultConfig = Config{
		ComportProducts:        "COM1",
		ComportHart:            "COM2",
		DurationBlowGasMinutes: 5,
		DurationBlowAirMinutes: 1,
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

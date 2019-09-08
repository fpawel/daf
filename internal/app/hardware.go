package app

import (
	"fmt"
	"github.com/ansel1/merry"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal"
	"github.com/fpawel/daf/internal/api/notify"
	"github.com/fpawel/daf/internal/api/types"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/party"
	"github.com/fpawel/gohelp"
	"github.com/fpawel/gohelp/myfmt"
	"strconv"
)

var (
	//ErrEN6408   = merry.New("стенд 6408")
	ErrGasBlock = merry.New("газовый блок")
)

type DafIndication struct {
	Concentration,
	Threshold1, Threshold2,
	Failure float64
	Mode uint16
}

type EN6408Value struct {
	OutputCurrent float64
	Threshold1,
	Threshold2 bool
}

//func init(){
//	rand.Seed(time.Now().UnixNano())
//}

func (x worker) writeProduct(p party.Product, cmd modbus.DevCmd, arg float64) error {
	//pause(x.ctx.Done(), time.Millisecond * 300)
	//return nil
	log := logPrependSuffixKeys(p.WrapLog(x.log), internal.LogKeyHardwareDevice, "daf")
	err := modbus.Write32(log, x.ctx, x.portProducts, p.Addr, 0x10, cmd, arg)
	if err != nil {
		if isDeviceError(err) {
			notify.PlaceConnection(nil, types.PlaceConnection{
				Place: p.Place,
				Text:  fmt.Sprintf("$%X: %v", cmd, err),
			})
		}
		return err
	}
	notify.PlaceConnection(x.log.Info, types.PlaceConnection{
		Place: p.Place,
		Text:  fmt.Sprintf("$%X<-%v", cmd, arg),
		Ok:    true,
	})
	return nil
}

func (x worker) readProduct(p party.Product) (a DafIndication, b EN6408Value, err error) {
	a, err = x.readDafIndication(p)
	if err != nil {
		return
	}
	b, err = x.read6408(p)
	return
}

func (x worker) read6408(p party.Product) (EN6408Value, error) {
	//pause(x.ctx.Done(), time.Millisecond * 300)
	//return EN6408Value{
	//	OutputCurrent: 4 + rand.Float64() * 16,
	//	Threshold1:    rand.Float64() < 0.5,
	//	Threshold2:    rand.Float64() < 0.5,
	//}, nil
	log := gohelp.LogPrependSuffixKeys(p.WrapLog(x.log), internal.LogKeyHardwareDevice, "ЭН6408")
	var result EN6408Value
	_, err := modbus.Read3(log, x.ctx,
		x.portProducts, 32, modbus.Var(p.Addr-1)*2, 2, func(_, response []byte) (string, error) {
			b := response[3:]
			result.OutputCurrent = (float64(b[0])*256 + float64(b[1])) / 100
			result.Threshold1 = b[3]&1 == 0
			result.Threshold2 = b[3]&2 == 0
			return fmt.Sprintf("%+v", result), nil
		})
	if err = p.WrapError(err); err != nil {
		return result, merry.Append(err, "ЭН6408")
	}
	go notify.PlaceConnection(nil, types.PlaceConnection{
		Place:  p.Place,
		Column: "Ток",
		Text:   myfmt.FormatFloat(result.OutputCurrent, -1),
		Ok:     true,
	})
	go notify.PlaceConnection(nil, types.PlaceConnection{
		Place:  p.Place,
		Column: "Реле 1",
		Text:   formatOnOf(result.Threshold1),
		Ok:     true,
	})
	go notify.PlaceConnection(nil, types.PlaceConnection{
		Place:  p.Place,
		Column: "Реле 2",
		Text:   formatOnOf(result.Threshold2),
		Ok:     true,
	})
	return result, nil
}

func (x worker) readUInt16(p party.Product, Var modbus.Var, column string, formatFunc func(int) string) (uint16, error) {
	//pause(x.ctx.Done(), time.Millisecond * 300)
	//return uint16(rand.Uint32()), nil
	log := logPrependSuffixKeys(p.WrapLog(x.log), internal.LogKeyHardwareDevice, "daf")
	if len(column) > 0 {
		log = gohelp.LogPrependSuffixKeys(log, internal.LogKeyDeviceVar, column)
	}
	c := types.PlaceConnection{
		Place:  p.Place,
		Column: column,
	}
	defer func() {
		go notify.PlaceConnection(nil, c)
	}()

	value, err := modbus.Read3UInt16(log, x.ctx, x.portProducts, p.Addr, Var)
	if err = p.WrapError(err); err == nil {
		if formatFunc == nil {
			formatFunc = strconv.Itoa
		}
		c.Text = formatFunc(int(value))
		c.Ok = true
		return value, err
	}
	if isDeviceError(err) {
		c.Text = err.Error()
	}
	return 0, err
}

func (x worker) readFloat(p party.Product, Var modbus.Var, column string, formatFunc func(float64) string) (float64, error) {
	//pause(x.ctx.Done(), time.Millisecond * 300)
	//return rand.Float64(), nil

	log := logPrependSuffixKeys(p.WrapLog(x.log), internal.LogKeyHardwareDevice, "daf")
	if len(column) > 0 {
		log = gohelp.LogPrependSuffixKeys(log, "column", column)
	}

	c := types.PlaceConnection{
		Place:  p.Place,
		Column: column,
	}
	defer func() {
		go notify.PlaceConnection(nil, c)
	}()
	value, err := modbus.Read3BCD(log, x.ctx, x.portProducts, p.Addr, Var)
	if err = p.WrapError(err); err == nil {
		if formatFunc == nil {
			formatFunc = func(f float64) string {
				return myfmt.FormatFloat(value, -1)
			}
		}
		c.Ok = true
		c.Text = formatFunc(value)
		return value, err
	}
	if isDeviceError(err) {
		c.Text = err.Error()
	}
	return 0, err
}

func (x worker) readDafIndication(p party.Product) (r DafIndication, err error) {
	for _, a := range []struct {
		Var devVar
		p   *float64
	}{
		{varC, &r.Concentration},
		{varThr1, &r.Threshold1},
		{varThr2, &r.Threshold2},
		{varFailureCode, &r.Failure},
	} {
		if *a.p, err = x.readFloat(p, a.Var.Code, a.Var.Name, nil); err != nil {
			return
		}
	}
	if r.Mode, err = x.readUInt16(p, varMode.Code, varMode.Name, nil); err == nil {
		return
	}
	return
}

func (x worker) blowGas(n int) error {
	if err := x.performf("включение клапана %d", n)(func(x worker) error {
		return x.performWithWarn(func() error {
			return x.switchGas(n)
		})
	}); err != nil {
		return err
	}
	return delayf(x, minutes(cfg.GetConfig().DurationBlowAirMinutes), "продувка ПГС%d", n)
}

func (x worker) switchGas(n int) error {
	//pause(x.ctx.Done(), time.Millisecond * 300)
	//return nil
	s := "отключить газ"
	if n != 0 {
		s = fmt.Sprintf("включить ПГС%d", n)
	}
	x.log = gohelp.LogPrependSuffixKeys(x.log,
		internal.LogKeyHardwareDevice, internal.LogValueGasSwitcher,
		internal.LogKeyGasValve, n)
	return x.perform(s, func(x worker) error {
		req := modbus.Request{
			Addr:     33,
			ProtoCmd: 0x10,
			Data:     []byte{0, 32, 0, 1, 2, 0, byte(n)},
		}
		if _, err := req.GetResponse(x.log, ctxApp, x.portProducts, nil); err != nil {
			return merry.WithCause(err, ErrGasBlock)
		}
		return nil
	})
}

type devVar struct {
	Code modbus.Var
	Name string
}

var (
	varC                      = devVar{0x00, "Концентрация"}
	varThr1                   = devVar{0x1C, "Порог 1"}
	varThr2                   = devVar{0x1E, "Порог 2"}
	varMode                   = devVar{0x23, "Режим"}
	varFailureCode            = devVar{0x20, "Отказ"}
	varSoftVer     modbus.Var = 0x36
	varSoftVerID   modbus.Var = 0x3A
	//varGas         modbus.Var = 0x32
)

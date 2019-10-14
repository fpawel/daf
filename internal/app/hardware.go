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
	"github.com/fpawel/daf/internal/pkg"
	"strconv"
	"time"
)

var (
	ErrHardware = merry.New("стендовое оборудование")
	ErrEN6408   = merry.New("стенд 6408").WithCause(ErrHardware)
	ErrGasBlock = merry.New("газовый блок").WithCause(ErrHardware)
)

type EN6408Value struct {
	I          float64
	Thr1, Thr2 bool
}

func (x worker) setNetAddress(addr modbus.Addr) error {
	r := modbus.NewWrite32BCDRequest(0, 0x10, 5, float64(addr))
	if err := x.portProducts.Open(); err != nil {
		return err
	}
	x.log.Info(fmt.Sprintf("% X", r.Data))
	if _, err := x.portProducts.Write(r.Data); err != nil {
		return err
	}
	pause(x.ctx.Done(), time.Second)
	_, err := modbus.Read3(x.log, x.ReaderDaf(), addr, 0, 2, nil)
	return err
}

func (x worker) writeProduct(p party.Product, cmd dafCmd, arg float64) error {
	//pause(x.ctx.Done(), time.Millisecond * 300)
	//return nil
	log := logPrependSuffixKeys(x.log,
		internal.LogKeyHardwareDevice, "daf",
		internal.LogKeyDafCmd, fmt.Sprintf("`%s`", cmd.Name))

	err := modbus.Write32(log, x.ReaderDaf(), p.Addr, 0x10, cmd.Code, arg)
	if err != nil {
		if isCommErrorOrDeadline(err) {
			notify.PlaceConnection(nil, types.PlaceConnection{
				Place: p.Place,
				Text:  fmt.Sprintf("%s %d: %v", cmd.Name, cmd.Code, err),
			})
		}
		return merry.Append(err, cmd.Name)
	}
	notify.PlaceConnection(x.log.Info, types.PlaceConnection{
		Place: p.Place,
		Text:  fmt.Sprintf("%s %d<-%v", cmd.Name, cmd.Code, arg),
		Ok:    true,
	})
	return nil
}

func (x worker) read6408(p party.Product) (EN6408Value, error) {
	//pause(x.ctx.Done(), time.Millisecond * 300)
	//n := float64(time.Now().Nanosecond()) / 1_000_000_000.
	//return EN6408Value{
	//	OutputCurrent: 4 + 16 * n,
	//	Threshold1:    n > 0.5,
	//	Threshold2:    n > 0.5,
	//}, nil

	log := pkg.LogPrependSuffixKeys(x.log, internal.LogKeyHardwareDevice, "ЭН6408")
	var result EN6408Value
	_, err := modbus.Read3(log, x.Reader6408(),
		32, modbus.Var(p.Addr-1)*2, 2, func(_, response []byte) (string, error) {
			b := response[3:]
			result.I = (float64(b[0])*256 + float64(b[1])) / 100
			result.Thr1 = b[3]&1 == 0
			result.Thr2 = b[3]&2 == 0
			return fmt.Sprintf("%+v", result), nil
		})
	if err = p.WrapError(err); err != nil {
		return result, merry.Wrap(err).WithCause(ErrEN6408)
	}
	notify.PlaceConnection(nil, types.PlaceConnection{
		Place:  p.Place,
		Column: "Ток",
		Text:   pkg.FormatFloat(result.I, -1),
		Ok:     true,
	})
	notify.PlaceConnection(nil, types.PlaceConnection{
		Place:  p.Place,
		Column: "Реле 1",
		Text:   formatBool(result.Thr1, "ВКЛ", "выкл"),
		Ok:     true,
	})
	notify.PlaceConnection(nil, types.PlaceConnection{
		Place:  p.Place,
		Column: "Реле 2",
		Text:   formatBool(result.Thr2, "ВКЛ", "выкл"),
		Ok:     true,
	})
	return result, nil
}

func (x worker) readUInt16(p party.Product, Var modbus.Var, column string, formatFunc func(int) string) (uint16, error) {
	//pause(x.ctx.Done(), time.Millisecond * 300)
	//return uint16(rand.Uint32()), nil
	log := logPrependSuffixKeys(x.log, internal.LogKeyHardwareDevice, "daf")
	if len(column) > 0 {
		log = pkg.LogPrependSuffixKeys(log, internal.LogKeyDafVar, column)
	}
	c := types.PlaceConnection{
		Place:  p.Place,
		Column: column,
	}
	defer func() {
		notify.PlaceConnection(nil, c)
	}()

	value, err := modbus.Read3UInt16(log, x.ReaderDaf(), p.Addr, Var)
	if err = p.WrapError(err); err == nil {
		if formatFunc == nil {
			formatFunc = strconv.Itoa
		}
		c.Text = formatFunc(int(value))
		c.Ok = true
		return value, err
	}
	if isCommErrorOrDeadline(err) {
		c.Text = err.Error()
	}
	return 0, err
}

func (x worker) readFloat(p party.Product, Var modbus.Var, column string) (float64, error) {
	//pause(x.ctx.Done(), time.Millisecond * 300)
	//return rand.Float64(), nil

	log := logPrependSuffixKeys(x.log, internal.LogKeyHardwareDevice, "daf")
	if len(column) > 0 {
		log = pkg.LogPrependSuffixKeys(log, "column", column)
	}

	c := types.PlaceConnection{
		Place:  p.Place,
		Column: column,
	}
	defer func() {
		notify.PlaceConnection(nil, c)
	}()
	value, err := modbus.Read3BCD(log, x.ReaderDaf(), p.Addr, Var)
	if err = p.WrapError(err); err == nil {
		c.Ok = true
		c.Text = pkg.FormatFloat(value, -1)
		return value, err
	}
	if isCommErrorOrDeadline(err) {
		c.Text = err.Error()
	}
	return 0, err
}

func (x worker) interrogate(p party.Product) error {
	for _, v := range []dafVar{
		varConcentration,
		varSignalSensor,
		varFailureCode,
		varType, varGas, varMeasureRange, varThr1, varThr2} {
		if _, err := x.readDafFloat(p, v); isFailWork(err) {
			return err
		}
	}
	if _, err := x.readDafUInt16(p, varMode); isFailWork(err) {
		return err
	}
	if _, err := x.read6408(p); err != nil {
		return err
	}
	return nil
}

func (x worker) readDafFloat(p party.Product, dafVar dafVar) (float64, error) {
	return x.readFloat(p, dafVar.Code, dafVar.Name)
}
func (x worker) readDafUInt16(p party.Product, dafVar dafVar) (uint16, error) {
	return x.readUInt16(p, varMode.Code, varMode.Name, nil)
}

func (x worker) blowGas(n int) error {
	if err := x.performf("включение клапана %d", n)(func(x worker) error {
		return x.performWithWarn(func() error {
			return x.switchGas(n)
		})
	}); err != nil {
		return err
	}
	c := cfg.GetConfig().DurationBlowGasMinutes
	dur := 5
	if n >= 0 && n < len(c) {
		dur = c[n]
	}
	return delayf(x, minutes(dur), "продувка ПГС%d", n)
}

func (x worker) switchGas(n int) error {
	//pause(x.ctx.Done(), time.Millisecond * 300)
	//return nil
	s := "отключить газ"
	if n != 0 {
		s = fmt.Sprintf("включить ПГС%d", n)
	}
	x.log = pkg.LogPrependSuffixKeys(x.log,
		internal.LogKeyHardwareDevice, internal.LogValueGasSwitcher,
		internal.LogKeyGasValve, n)
	return x.perform(s, func(x worker) error {
		req := modbus.Request{
			Addr:     33,
			ProtoCmd: 0x10,
			Data:     []byte{0, 32, 0, 1, 2, 0, byte(n)},
		}
		if _, err := req.GetResponse(x.log, x.ReaderGas(), nil); err != nil {
			return merry.WithCause(err, ErrGasBlock)
		}
		*x.gas = n
		return nil
	})
}

type dafVar struct {
	Code modbus.Var
	Name string
}

var (
	varConcentration = dafVar{0x00, "Концентрация"}
	varThr1          = dafVar{0x1C, "Порог 1"}
	varThr2          = dafVar{0x1E, "Порог 2"}
	varMode          = dafVar{0x23, "Режим"}
	varFailureCode   = dafVar{0x20, "Отказ"}
	varSignalSensor  = dafVar{86, "Uдат"}

	varGas          = dafVar{50, "Газ"}
	varType         = dafVar{224 + 25*2, "Исполнение"}
	varMeasureRange = dafVar{0xEC, "Диапазон"}
)

const (
	varSoftVer   = modbus.Var(0x36)
	varSoftVerID = modbus.Var(0x3A)
	//varGas         modbus.Var = 0x32
)

type dafCmd struct {
	Code modbus.DevCmd
	Name string
}

var (
	cmdAdjustBeg = dafCmd{1, "корректировка нулевых показаний ДАФ"}
	cmdAdjustEnd = dafCmd{2, "корректировка чувствительности ДАФ"}
	cmdSetupThr1 = dafCmd{3, "установка порога 1 ДАФ"}
	cmdSetupThr2 = dafCmd{4, "установка порога 2 ДАФ"}
	//cmdSetupAddr = dafCmd{5,"установка адреса"}
	cmdSetupType      = dafCmd{7, "установка кода исполнения ДАФ"}
	cmdSetupComponent = dafCmd{6, "установка кода измеряемого компонента ДАФ"}

	cmdSetupTemp = dafCmd{8, "установка температуры ДАФ"}
	cmdSetup4    = dafCmd{9, "корректировка 4 мА ДАФ"}
	cmdSetup20   = dafCmd{0x0A, "корректировка 20 мА ДАФ"}
	cmdMode4     = dafCmd{0x0B, "установка 4 мА ДАФ"}
	cmdMode20    = dafCmd{0x0C, "установка 20 мА ДАФ"}
)

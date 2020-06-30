package app

import (
	"encoding/binary"
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
	r := modbus.RequestWrite32{
		Addr:      0,
		ProtoCmd:  0x10,
		DeviceCmd: 5,
		Format:    modbus.BCD,
		Value:     float64(addr),
	}
	if err := x.portProducts.Open(); err != nil {
		return err
	}
	x.log.Info(fmt.Sprintf("% X", r.Request().Bytes()))
	if _, err := x.portProducts.Write(r.Request().Bytes()); err != nil {
		return err
	}
	pause(x.ctx.Done(), time.Second)
	_, err := modbus.Read3Value(x.log, x.ctx, x.commDaf(), addr, 0, modbus.BCD)
	return err
}

func (x worker) writeProduct(p party.Product, cmd dafCmd, arg float64) error {
	//pause(x.ctx.Done(), time.Millisecond * 300)
	//return nil
	log := logPrependSuffixKeys(x.log,
		internal.LogKeyHardwareDevice, "daf",
		internal.LogKeyDafCmd, fmt.Sprintf("`%s`", cmd.Name))

	err := modbus.RequestWrite32{
		Addr:      p.Addr,
		ProtoCmd:  0x10,
		DeviceCmd: cmd.Code,
		Format:    "bcd",
		Value:     arg,
	}.GetResponse(log, x.ctx, x.commDaf())

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
	log := pkg.LogPrependSuffixKeys(x.log, internal.LogKeyHardwareDevice, "ЭН6408")
	var result EN6408Value

	response, err := modbus.RequestRead3{
		Addr:           32,
		FirstRegister:  modbus.Var(p.Addr-1) * 2,
		RegistersCount: 2,
	}.GetResponse(log, x.ctx, x.commDaf())
	if err = p.WrapError(err); err != nil {
		return result, merry.Wrap(err).WithCause(ErrEN6408)
	}

	b := response[3:]
	result.I = (float64(b[0])*256 + float64(b[1])) / 100

	c := cfg.GetConfig()
	d, _ := c.CurrentAdd[p.Addr]
	result.I += d

	result.Thr1 = b[3]&1 == 0
	result.Thr2 = b[3]&2 == 0

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

	value, err := modbus.Read3UInt16(log, x.ctx, x.commDaf(), p.Addr, Var, binary.BigEndian)
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
	value, err := modbus.Read3Value(log, x.ctx, x.commDaf(), p.Addr, Var, modbus.BCD)
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
	vars := []dafVar{
		varConcentration,
		varSignalSensor,
		varFailureCode,
		varType, varGas, varMeasureRange, varThr1, varThr2,
	}
	for _, v := range vars {
		if _, err := x.readFloatVar(p, v); isFailWork(err) {
			return err
		}
	}
	if _, err := x.readUInt16Var(p, varMode); isFailWork(err) {
		return err
	}
	if _, err := x.read6408(p); err != nil {
		return err
	}
	return nil
}

func (x worker) readFloatVar(p party.Product, dafVar dafVar) (float64, error) {
	return x.readFloat(p, dafVar.Code, dafVar.String())
}
func (x worker) readUInt16Var(p party.Product, dafVar dafVar) (uint16, error) {
	return x.readUInt16(p, varMode.Code, dafVar.String(), nil)
}

func (x worker) read2(p party.Product, Var modbus.Var, column string) ([]byte, error) {
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

	bs, err := modbus.RequestRead3{
		Addr:           p.Addr,
		FirstRegister:  Var,
		RegistersCount: 2,
	}.GetResponse(log, x.ctx, x.commDaf())
	if err = p.WrapError(err); err == nil {
		c.Ok = true
		c.Text = fmt.Sprintf("% X", bs)
		return bs, nil
	}
	if isCommErrorOrDeadline(err) {
		c.Text = err.Error()
	}
	return nil, err
}

func (x worker) blowGas(n int) error {
	if err := x.performf("включение клапана %d", n)(func(x worker) error {
		return x.performWithWarn(func() error {
			return x.switchGas(n)
		})
	}); err != nil {
		return err
	}
	c := cfg.GetConfig().DurationBlowGas
	duration := 5 * time.Minute
	if n > 0 && n <= len(c) {
		duration = c[n-1]
	}
	return delayf(x, duration, "продувка ПГС%d", n)
}

func (x worker) blowAir() error {
	if err := x.perform("включение клапана 1", func(x worker) error {
		return x.performWithWarn(func() error {
			return x.switchGas(1)
		})
	}); err != nil {
		return err
	}
	return delay(x, cfg.GetConfig().DurationBlowAir, "продувка воздухом")
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
		if _, err := req.GetResponse(x.log, x.ctx, x.commGas()); err != nil {
			return merry.WithCause(err, ErrGasBlock)
		}
		return nil
	})
}

type dafVar struct {
	Code modbus.Var
	Name string
}

var (
	varConcentration = dafVar{0x00, "конц."}
	varThr1          = dafVar{0x1C, "порог1"}
	varThr2          = dafVar{0x1E, "порог2"}
	varMode          = dafVar{0x23, "режим"}
	varFailureCode   = dafVar{0x20, "отказ"}
	varSignalSensor  = dafVar{86, "датчик"}

	varGas          = dafVar{50, "газ"}
	varType         = dafVar{224 + 25*2, "исп."}
	varMeasureRange = dafVar{0xEC, "диап."}
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

func (x dafVar) String() string {
	return fmt.Sprintf("[0x%X] %s", x.Code, x.Name)
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

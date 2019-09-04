package app

import (
	"fmt"
	"github.com/ansel1/merry"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/api/notify"
	"github.com/fpawel/daf/internal/api/types"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/party"
	"github.com/fpawel/gohelp"
)

var (
	ErrEN6408   = merry.New("стенд 6408")
	ErrGasBlock = merry.New("газовый блок")
)

type DafIndication struct {
	Concentration,
	Threshold1, Threshold2,
	Failure float64
	Mode uint16
}

type DafInfo struct {
	Version, VersionID, Gas float64
}

type EN6408Value struct {
	Current float64
	Threshold1,
	Threshold2 bool
}

func (x worker) readPlace(p party.Product) (a DafIndication, b EN6408Value, err error) {
	a, err = x.dafReadIndication(p)
	if err != nil {
		return
	}
	b, err = x.read6408(p)
	return
}

func (x worker) read6408(p party.Product) (EN6408Value, error) {
	x.log = logProduct(x.log, p)
	x.log = gohelp.LogPrependSuffixKeys(x.log, "ЭН6408", "")
	var result EN6408Value
	_, err := modbus.Read3(x.log, x.ctx,
		x.portProducts, 32, modbus.Var(p.Addr-1)*2, 2, func(_, response []byte) (string, error) {
			b := response[3:]
			result.Current = (float64(b[0])*256 + float64(b[1])) / 100
			result.Threshold1 = b[3]&1 == 0
			result.Threshold2 = b[3]&2 == 0
			return fmt.Sprintf("%+v", result), nil
		})
	if err != nil {
		return result, merry.Appendf(err, "ЭН6408: место %d", p.Place)
	}
	go notify.ReadProductValue(log.Debug, types.ProductValue{
		Place:  p.Place,
		Column: "Ток",
		Value:  result.Current,
	})
	go notify.ReadProductValue(log.Debug, types.ProductValue{
		Place:  p.Place,
		Column: "Реле 1",
		Value:  boolToFloat(result.Threshold1),
	})
	go notify.ReadProductValue(log.Debug, types.ProductValue{
		Place:  p.Place,
		Column: "Реле 2",
		Value:  boolToFloat(result.Threshold2),
	})
	return result, nil
}

func (x worker) dafReadUInt16(p party.Product, Var devVar) (uint16, error) {
	log := logProduct(x.log, p)
	log = gohelp.LogPrependSuffixKeys(log, "параметр_ДАФ", Var.Name)
	value, err := modbus.Read3UInt16(log, x.ctx, x.portProducts, p.Addr, Var.Code)
	if err == nil {
		notify.ReadProductValue(log.Debug, types.ProductValue{
			Place:  p.Place,
			Column: Var.Name,
			Value:  float64(value),
		})
		return value, err
	}
	if isDeviceError(err) {
		notify.ProductError(log.PrintErr, types.ProductError{
			Place:   p.Place,
			Message: err.Error(),
		})
	}
	return 0, err
}

func (x worker) dafReadFloat(p party.Product, Var devVar) (float64, error) {
	log := logProduct(x.log, p)
	log = gohelp.LogPrependSuffixKeys(log, "параметр_ДАФ", Var.Name)
	value, err := modbus.Read3BCD(log, x.ctx, x.portProducts, p.Addr, Var.Code)
	if err == nil {
		notify.ReadProductValue(log.Debug, types.ProductValue{
			Place:  p.Place,
			Column: Var.Name,
			Value:  value,
		})
		return value, err
	}
	if isDeviceError(err) {
		notify.ProductError(log.PrintErr, types.ProductError{
			Place:   p.Place,
			Message: err.Error(),
		})
	}
	return 0, err
}

func (x worker) dafReadIndication(p party.Product) (r DafIndication, err error) {
	for _, a := range []struct {
		Var devVar
		p   *float64
	}{
		{varC, &r.Concentration},
		{varThr1, &r.Threshold1},
		{varThr2, &r.Threshold2},
		{varFailureCode, &r.Failure},
	} {
		if *a.p, err = x.dafReadFloat(p, a.Var); err != nil {
			return
		}
	}
	if r.Mode, err = x.dafReadUInt16(p, varMode); err == nil {
		return
	}
	return
}

func (x worker) dafReadInfo(p party.Product) (r DafInfo, err error) {
	for _, a := range []struct {
		Var devVar
		p   *float64
	}{
		{varGas, &r.Gas},
		{varSoftVer, &r.Version},
		{varSoftVerID, &r.VersionID},
	} {
		if *a.p, err = x.dafReadFloat(p, a.Var); err != nil {
			return
		}
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
	s := "отключить"
	if n != 0 {
		s = fmt.Sprintf("ПГС%d", n)
	}
	x.log = gohelp.LogPrependSuffixKeys(x.log, "газовый_блок", s)
	return x.perform(s, func(x worker) error {
		req := modbus.Request{
			Addr:     33,
			ProtoCmd: 0x10,
			Data:     []byte{0, 32, 0, 1, 2, 0, byte(n)},
		}
		if _, err := req.GetResponse(x.log, ctxApp, x.portProducts, nil); err != nil {
			return merry.Append(err, s).WithCause(ErrGasBlock)
		}
		return nil
	})
}

type devVar struct {
	Code modbus.Var
	Name string
}

var (
	varC           = devVar{0x00, "Концентрация"}
	varThr1        = devVar{0x1C, "Порог 1"}
	varThr2        = devVar{0x1E, "Порог 2"}
	varMode        = devVar{0x23, "Режим"}
	varFailureCode = devVar{0x20, "Код отказа"}
	varSoftVer     = devVar{0x36, "Версия ПО"}
	varSoftVerID   = devVar{0x3A, "ID версии ПО"}
	varGas         = devVar{0x32, "Газ"}
)

package api

import "github.com/fpawel/comm/modbus"

type Runner interface {
	Cancel()
	SkipDelay()
	RunReadVars()
	SwitchGas(int)
	RunMainWork([]bool)
	SetNetAddress(addr modbus.Addr)
	Read3(Var modbus.Var, bcd bool)
	Write32(cmd modbus.DevCmd, value float64)
}

type RunnerSvc struct {
	r Runner
}

func NewRunnerSvc(r Runner) *RunnerSvc {
	return &RunnerSvc{r}
}

func (x *RunnerSvc) Write32(n [2]float64, _ *struct{}) error {
	x.r.Write32(modbus.DevCmd(n[0]), n[1])
	return nil
}

func (x *RunnerSvc) Read3(n [2]int, _ *struct{}) error {
	x.r.Read3(modbus.Var(n[0]), n[0] != 0)
	return nil
}

func (x *RunnerSvc) SwitchGas(n [1]int, _ *struct{}) error {
	x.r.SwitchGas(n[0])
	return nil
}

func (x *RunnerSvc) Cancel(_ struct{}, _ *struct{}) error {
	x.r.Cancel()
	return nil
}

func (x *RunnerSvc) SkipDelay(_ struct{}, _ *struct{}) error {
	x.r.SkipDelay()
	return nil
}

func (x *RunnerSvc) RunReadVars(_ struct{}, _ *struct{}) error {
	x.r.RunReadVars()
	return nil
}

func (x *RunnerSvc) RunMainWork(r struct{ Works []bool }, _ *struct{}) error {
	x.r.RunMainWork(r.Works)
	return nil
}

func (x *RunnerSvc) SetNetAddress(r [1]byte, _ *struct{}) error {
	x.r.SetNetAddress(modbus.Addr(r[0]))
	return nil
}

package api

type Runner interface {
	Cancel()
	SkipDelay()
	RunReadVars()
	SwitchGas(int)
	RunMainWork([]bool)
}

type RunnerSvc struct {
	r Runner
}

func NewRunnerSvc(r Runner) *RunnerSvc {
	return &RunnerSvc{r}
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

package api

type Runner interface {
	Cancel()
	SkipDelay()
	RunReadVars()
	RunMainWork()
}

type RunnerSvc struct {
	r Runner
}

func NewRunnerSvc(r Runner) *RunnerSvc {
	return &RunnerSvc{r}
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

func (x *RunnerSvc) RunMainWork(_ struct{}, _ *struct{}) error {
	x.r.RunMainWork()
	return nil
}

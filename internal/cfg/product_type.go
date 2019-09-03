package cfg

import "fmt"

type ProductType struct {
	N1, N2                                                int
	Component                                             Component
	Scale                                                 Scale
	K4, K14, K45, K35, K50, TempMinus, TempPlus, ScaleBeg float64
}

type Component string

type ComponentInfo struct {
	Code, UnitsCode int
}

const (
	CO2   Component = "CO₂"
	CH4   Component = "CH₄"
	C3H8  Component = "C₃H₈"
	C6H14 Component = "C₆H₁₄"
)

type Scale float64

const (
	Sc4   Scale = 4
	Sc10  Scale = 10
	Sc20  Scale = 20
	Sc50  Scale = 50
	Sc100 Scale = 100
)

func (t ProductType) Name() string {
	return fmt.Sprintf("%02d.%02d", t.N1, t.N2)
}

func (s Scale) Code() int {
	switch s {
	case Sc4:
		return 57
	case Sc10:
		return 7
	case Sc20:
		return 9
	case Sc50:
		return 0
	case Sc100:
		return 21
	default:
		panic(s)
	}
}

func (c Component) Info() ComponentInfo {
	type ci = ComponentInfo
	switch c {
	case CO2:
		return ci{4, 7}
	case CH4:
		return ci{5, 14}
	case C3H8:
		return ci{7, 14}
	case C6H14:
		return ci{7, 14}
	default:
		panic(c)
	}
}

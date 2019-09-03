package daf

type ProductType struct {
	Name      string
	Component Component
	Scale,
	K4, K14, K45, K35, K50,
	TempMinus, TempPlus float64
}

type Component string

const (
	CO2   Component = "CO₂"
	CH4   Component = "CH₄"
	C3H8  Component = "C₃H₈"
	C6H14 Component = "C₆H₁₄"
)

func ProductTypeByName(s string) ProductType {
	for _, x := range productTypes {
		if x.Name == s {
			return x
		}
	}
	panic(s)
}

func (x ProductType) maxErr20(Cn float64) float64 {
	if x.Component != CO2 {
		return 2.5 + 0.05*Cn
	}
	switch x.Scale {
	case 4:
		return 0.2 + 0.05*Cn
	case 10:
		return 0.5
	default:
		return 1
	}
}

var productTypes = []ProductType{
	{Name: "00.00", Component: CO2, Scale: 4, K4: 5, K14: 0.1, K45: 60, K35: 5, K50: 0, TempMinus: -40, TempPlus: 80},
	{Name: "00.01", Component: CO2, Scale: 10, K4: 5, K14: 0.1, K45: 60, K35: 5, K50: 0, TempMinus: -40, TempPlus: 80},
	{Name: "00.02", Component: CO2, Scale: 20, K4: 5, K14: 0.1, K45: 60, K35: 5, K50: 0, TempMinus: -40, TempPlus: 80},
	{Name: "01.00", Component: CH4, Scale: 100, K4: 7.5, K14: 0.5, K45: 60, K35: 5, K50: 0, TempMinus: -40, TempPlus: 80},
	{Name: "01.01", Component: CH4, Scale: 100, K4: 7.5, K14: 0.5, K45: 60, K35: 5, K50: 0, TempMinus: -60, TempPlus: 60},
	{Name: "02.00", Component: C3H8, Scale: 50, K4: 12.5, K14: 0.5, K45: 30, K35: 5, K50: 0, TempMinus: -40, TempPlus: 60},
	{Name: "02.01", Component: C3H8, Scale: 50, K4: 12.5, K14: 0.5, K45: 30, K35: 5, K50: 0, TempMinus: -60, TempPlus: 60},
	{Name: "03.00", Component: C3H8, Scale: 100, K4: 12.5, K14: 0.5, K45: 30, K35: 5, K50: 0, TempMinus: -40, TempPlus: 60},
	{Name: "03.01", Component: C3H8, Scale: 100, K4: 12.5, K14: 0.5, K45: 30, K35: 5, K50: 0, TempMinus: -60, TempPlus: 60},
	{Name: "04.00", Component: CH4, Scale: 100, K4: 7.5, K14: 0.5, K45: 60, K35: 5, K50: 0, TempMinus: -60, TempPlus: 80},
	{Name: "05.00", Component: C6H14, Scale: 50, K4: 1, K14: 30, K45: 30, K35: 5, K50: 0, TempMinus: 15, TempPlus: 80},
	{Name: "10.00", Component: CO2, Scale: 4, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -40, TempPlus: 80},
	{Name: "10.01", Component: CO2, Scale: 10, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -40, TempPlus: 80},
	{Name: "10.02", Component: CO2, Scale: 20, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -40, TempPlus: 80},
	{Name: "10.03", Component: CO2, Scale: 4, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
	{Name: "10.04", Component: CO2, Scale: 10, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
	{Name: "10.05", Component: CO2, Scale: 20, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
	{Name: "11.00", Component: CH4, Scale: 100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -40, TempPlus: 80},
	{Name: "11.01", Component: CH4, Scale: 100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
	{Name: "13.00", Component: C3H8, Scale: 100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -40, TempPlus: 80},
	{Name: "13.01", Component: C3H8, Scale: 100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
	{Name: "14.00", Component: CH4, Scale: 100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
	{Name: "16.00", Component: C3H8, Scale: 100, K4: 1, K14: 30, K45: 30, K35: 1, K50: 1, TempMinus: -60, TempPlus: 80},
}

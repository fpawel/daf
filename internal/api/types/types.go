package types

import (
	"fmt"
)

type YearMonth struct {
	Year  int `db:"year"`
	Month int `db:"month"`
}

type TempPlusMinus struct {
	TempPlus, TempMinus float64
}

type PlaceConnection struct {
	Place  int
	Text   string
	Column string
	Ok     bool
}

type WorkResult int

const (
	WrOk WorkResult = iota
	WrCanceled
	WrError
)

func (x WorkResult) String() string {
	switch x {
	case WrOk:
		return "Ok"
	case WrCanceled:
		return "Canceled"
	case WrError:
		return "Error"
	default:
		return fmt.Sprintf("?%d", x)
	}
}

type WorkResultInfo struct {
	Work    string
	Result  WorkResult
	Message string
}

type DelayInfo struct {
	TotalSeconds,
	ElapsedSeconds int
	What string
}

SET APP_DIR=build
SET GOARCH=386
buildmingw32 go build -o %APP_DIR%\daf.exe github.com/fpawel/daf/cmd/daf
buildmingw32 go build -o %APP_DIR%\run.exe github.com/fpawel/daf/cmd/run


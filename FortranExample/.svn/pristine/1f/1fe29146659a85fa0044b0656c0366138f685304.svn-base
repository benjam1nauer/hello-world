call set_vars.bat

if '%1' == 'Release' goto release

sh -c "BUILD_TYPE=Debug make"

if '%1' == 'Debug' goto fertig

:release
sh -c "BUILD_TYPE=Release make"

:fertig
pause

rem Weitere..
rem sh -c "BUILD_TYPE=Debug make test"


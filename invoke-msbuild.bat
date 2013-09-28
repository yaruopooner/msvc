@echo off


rem # current path change to package directory
pushd "%~dp0"


rem # set arguments
set _MS_VS_SHELL=%1
set _MS_VS_SHELL_ARG=%2
set _MSB_RSP_FILE=%3
set _LOG_FILE=%4


call %_MS_VS_SHELL% %_MS_VS_SHELL_ARG%


rem # exec MSBuild
msbuild.exe @%_MSB_RSP_FILE%


rem # display to STDOUT
if exist %_LOG_FILE% (
type %_LOG_FILE%
)


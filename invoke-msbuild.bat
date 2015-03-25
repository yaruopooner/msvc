@echo off


@rem # current path change to package directory
pushd "%~dp0"


@rem # set arguments
set _MSVC_TOOLSET_SHELL=%1
set _MSVC_TOOLSET_TYPE=%2
set _MSB_RSP_FILE=%3
set _LOG_FILE=%4


call %_MSVC_TOOLSET_SHELL% %_MSVC_TOOLSET_TYPE%


@rem # exec MSBuild
msbuild.exe @%_MSB_RSP_FILE%


@rem # display to STDOUT
if exist %_LOG_FILE% (
type %_LOG_FILE%
)


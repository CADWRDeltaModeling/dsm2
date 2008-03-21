setlocal
set dsm2guipath=..\lib\dsm2help.jar;..\lib\jh.jar;..\lib\DatabaseGUI.jar;..\lib\jdnc-0_7-all.jar
set dsm2guipath=%dsm2guipath%;..\lib\dx.jar;..\lib\dbswing.jar;..\lib\beandt.jar;
set dsm2app=dwr.dms.appframework.Application
:: the redirection did not work so well with javaw
start javaw.exe -classpath %dsm2guipath%  -mx100m -ea %dsm2app% dsm2interface.properties 2>err.log
endlocal


cd /d %~dp0
echo "put " java -jar %CD%\dist\rcsjava.jar --subclass Plotter  %%* " in " %1\Desktop\plotter.bat
echo java -jar %CD%\dist\rcsjava.jar --subclass Plotter  %%* > %1\Desktop\plotter.bat
#Stop Geckodriver
try(system("taskkill /F /IM geckodriver.exe"))

#Stop Java-Process
try(system("taskkill /F /IM java.exe"))
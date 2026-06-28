$ErrorActionPreference = 'Continue'
$env:DATASCIENCE_SHINY_CLEAN_CHILD = '1'
$logPath = 'C:/Users/PC/Desktop/R_git/DataScience_Shiny/logs/run_app_clean_child.log'
"Starting clean DataScience_Shiny child process..." | Set-Content -Path $logPath -Encoding UTF8
& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' 'C:/Users/PC/Desktop/R_git/DataScience_Shiny/run_app.R' 2>&1 | Tee-Object -FilePath $logPath -Append
exit $LASTEXITCODE

Clear-Host

$files = @(
	".\atms.exe", 
	".\central.exe", 
	".\trans711.txt", 
	".\trans713.txt", 
	".\transSorted711.txt", 
	".\transSorted713.txt", 
	".\transSorted.txt", 
	".\updatedMaster.txt", 
	".\negReport.txt"
)

foreach ( $file in $files ) { if (Test-Path $file) { Remove-Item $file } }

cobc -x .\atms.cob
cobc -x .\central.cob

Get-Content .\testcase\testcase.txt | .\atms.exe
.\central.exe

if ( Compare-Object (Get-Content .\updatedMaster.txt) (Get-Content .\testcase\reference_updatedMaster.txt) ) {
<<<<<<< Updated upstream:touch_atm_project/test.ps1
	Write-Output "TESTCASE FAILED"
=======
	Write-Error "`n>>>>> TESTCASE FAILED`n"
>>>>>>> Stashed changes:atm_project/test.ps1
} 
else {
	Write-Output "`n>>>>> TESTCASE PASSED`n"
}

foreach ( $file in $files ) { if (Test-Path $file) { Remove-Item $file } }

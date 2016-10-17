mkdir gen
call ..\..\..\tools\gbc.exe c# --output-dir=gen --namespace=bond=Bond ..\..\..\..\idl\bond\core\bond.bond ..\..\..\..\idl\bond\core\bond_const.bond

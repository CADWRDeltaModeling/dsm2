copy .\dbutil\copy_informax_db\template\dsm2input.mdb .\dbutil\copy_informax_db\dist /Y 
.\dbutil\copy_informax_db\dist\i2m_dbtranslator_ado.exe .\dbutil\copy_informax_db\dist\schema.sql
copy .\dbutil\copy_informax_db\dist\dsm2input.mdb /Y
del /F .\dbutil\copy_informax_db\dist\dsm2input.mdb

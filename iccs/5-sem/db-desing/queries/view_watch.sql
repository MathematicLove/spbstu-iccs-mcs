CREATE USER 'viewsmotr'@'localhost' IDENTIFIED BY 'Ayzek123321';
GRANT SELECT ON student_adm.E_Stats1 TO 'viewsmotr'@'localhost';
FLUSH PRIVILEGES;

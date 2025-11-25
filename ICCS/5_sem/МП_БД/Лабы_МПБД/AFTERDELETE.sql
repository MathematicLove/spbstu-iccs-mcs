DELIMITER //
DROP TRIGGER IF EXISTS UECAD;
CREATE TRIGGER UECAD
AFTER DELETE ON Enrollee
FOR EACH ROW
BEGIN
    DECLARE old_department_id INT;

    SELECT s.department_id INTO old_department_id
    FROM Specialty s
    JOIN Choice c ON s.specialty_id = c.specialty_id
    WHERE c.enrollee_id = OLD.enrollee_id
    LIMIT 1;

    IF OLD.enrollment_status_id = 6 THEN
        UPDATE de_count_tr
        SET enrollee_count = enrollee_count - 1
        WHERE department_id = old_department_id;
    END IF;
END //

DELIMITER ;

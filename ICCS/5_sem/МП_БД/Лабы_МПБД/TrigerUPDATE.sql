DELIMITER //

CREATE TRIGGER update_enrollee_count_after_update
AFTER UPDATE ON Enrollee
FOR EACH ROW
BEGIN
    DECLARE old_department_id INT;
    DECLARE new_department_id INT;
    IF OLD.enrollment_status_id = 6 THEN
        SELECT s.department_id INTO old_department_id
        FROM Specialty s
        JOIN Choice c ON s.specialty_id = c.specialty_id
        WHERE c.enrollee_id = OLD.enrollee_id
        LIMIT 1;
        UPDATE de_count_tr
        SET enrollee_count = enrollee_count - 1
        WHERE department_id = old_department_id;
    END IF;
    IF NEW.enrollment_status_id = 6 THEN
        SELECT s.department_id INTO new_department_id
        FROM Specialty s
        JOIN Choice c ON s.specialty_id = c.specialty_id
        WHERE c.enrollee_id = NEW.enrollee_id
        LIMIT 1;

        UPDATE de_count_tr
        SET enrollee_count = enrollee_count + 1
        WHERE department_id = new_department_id;
    END IF;
END //

DELIMITER ;

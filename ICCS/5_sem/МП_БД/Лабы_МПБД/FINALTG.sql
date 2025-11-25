
DELIMITER //
CREATE TRIGGER update_enrollee_count_after_update
AFTER UPDATE ON Enrollee
FOR EACH ROW
BEGIN
    IF OLD.enrollment_status_id = 6 THEN
        UPDATE de_count_tr
        SET enrollee_count = enrollee_count - 1
        WHERE department_id IN (
            SELECT s.department_id
            FROM Specialty s
            JOIN Choice c ON s.specialty_id = c.specialty_id
            WHERE c.enrollee_id = OLD.enrollee_id
        );
    END IF;
    IF NEW.enrollment_status_id = 6 THEN
        UPDATE de_count_tr
        SET enrollee_count = enrollee_count + 1
        WHERE department_id IN (
            SELECT s.department_id
            FROM Specialty s
            JOIN Choice c ON s.specialty_id = c.specialty_id
            WHERE c.enrollee_id = NEW.enrollee_id
        );
    END IF;
END //



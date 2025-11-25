DELIMITER //
CREATE TRIGGER update_department_in_de_count_tr
AFTER UPDATE ON Department
FOR EACH ROW
BEGIN
    UPDATE de_count_tr
    SET department_name = NEW.name
    WHERE department_id = NEW.department_id;
END //
DELIMITER ;

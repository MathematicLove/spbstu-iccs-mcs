DELIMITER //
CREATE TRIGGER add_new_department
AFTER INSERT ON Department
FOR EACH ROW
BEGIN
    INSERT INTO de_count_tr (department_id, department_name, enrollee_count)
    VALUES (NEW.department_id, NEW.name, 0);
END //
DELIMITER ;

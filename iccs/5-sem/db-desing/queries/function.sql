DELIMITER //
DROP FUNCTION IF exists giveIONS;
CREATE FUNCTION giveIONS(first_name VARCHAR(80), last_name VARCHAR(80), middle_name VARCHAR(80))
RETURNS VARCHAR(100)
DETERMINISTIC
BEGIN
    RETURN IF(
        middle_name IS NULL OR middle_name = '',
        CONCAT(LEFT(first_name, 1), '. ', last_name),
        CONCAT(LEFT(first_name, 1), '.', LEFT(middle_name, 1), '. ', last_name)
    );
END //

DELIMITER ;


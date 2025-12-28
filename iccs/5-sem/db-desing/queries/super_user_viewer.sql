CREATE VIEW E_Stats1 AS
SELECT
    e.enrollee_id,
    CONCAT(e.last_name, ' ', e.first_name, ' ', e.middle_name) AS enrollee_name,
    COALESCE(a.exam_count, 0) AS exam_count,  
    COALESCE(d.institute_count, 0) AS institute_count
FROM 
    Enrollee e
LEFT JOIN (
    SELECT enrollee_id, COUNT(achievment_id) AS exam_count
    FROM Achievment
    GROUP BY enrollee_id
) a ON e.enrollee_id = a.enrollee_id
LEFT JOIN (
    SELECT c.enrollee_id, COUNT(s.department_id) AS institute_count
    FROM Choice c
    JOIN Specialty s ON c.specialty_id = s.specialty_id
    GROUP BY c.enrollee_id
) d ON e.enrollee_id = d.enrollee_id;

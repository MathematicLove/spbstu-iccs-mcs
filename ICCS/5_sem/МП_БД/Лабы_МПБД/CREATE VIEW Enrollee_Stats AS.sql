CREATE VIEW E_Stats AS
SELECT
    e.enrollee_id,
    CONCAT(e.last_name, ' ', e.first_name, ' ', e.middle_name) AS enrollee_name,
    COUNT(a.achievment_id) AS exam_count,  
    COUNT(d.department_id) AS institute_count
FROM 
    Enrollee e
LEFT JOIN Achievment a ON e.enrollee_id = a.enrollee_id
LEFT JOIN Choice c ON e.enrollee_id = c.enrollee_id
LEFT JOIN Specialty s ON c.specialty_id = s.specialty_id
LEFT JOIN Department d ON s.department_id = d.department_id
GROUP BY e.enrollee_id, e.first_name, e.last_name, e.middle_name;

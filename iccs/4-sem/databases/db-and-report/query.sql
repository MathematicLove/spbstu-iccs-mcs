# 1) найти дисциплину который сдавал абитуриент А который поступал по направлению В

select S.subject_id, S.name
from Choice C
join Achievment A on C.enrollee_id = A.enrollee_id
join Specialty Sp on C.specialty_id = Sp.specialty_id
join Subject_Requirement SR on Sp.specialty_id = SR.specialty_id and SR.subject_id = A.subject_id
join Subject S on SR.subject_id = S.subject_id
where C.enrollee_id = 457322 and C.specialty_id = 3;


# 2) из института А посчитать число абитуриентов

select S.department_id, count(DISTINCT C.enrollee_id)
from Choice C
join Specialty S on C.specialty_id = S.specialty_id
where S.department_id = 1;

# 2.2) для института А посчитать число абитуриентов по каждому направлению

select C.specialty_id, count(DISTINCT C.enrollee_id)
from Choice C
join Specialty S on C.specialty_id = S.specialty_id
where S.department_id = 1
group by C.specialty_id;

# 3) с каждого института число подавших абитуриентов + к этому гистограмму

select D.name as department_name, count(DISTINCT C.enrollee_id) as enrollee_count
from Department D
left join Specialty S on S.department_id = D.department_id
left join Choice C on C.specialty_id = S.specialty_id
group by D.department_id;

# выведет с нулями


# 4) найти абитуриента с максимальным числом результата поступления
# выполняется 14,09 при 200к абитуров, не уверен, что это можно переписать быстрее
select C.enrollee_id, C.specialty_id, sum(greatest(A.ege_result, A.inner_result)) as result
from Choice C
join Subject_Requirement SR on C.specialty_id = SR.specialty_id
join Achievment A on A.enrollee_id = C.enrollee_id and A.subject_id = SR.subject_id
group by C.specialty_id, C.enrollee_id
having result = (select max(result) from (select sum(greatest(A.ege_result, A.inner_result)) as result
    from Choice C
    join Subject_Requirement SR on C.specialty_id = SR.specialty_id
    join Achievment A on A.enrollee_id = C.enrollee_id and A.subject_id = SR.subject_id
    group by C.enrollee_id, C.specialty_id) results
);

    # чтобы проверить предыдущий запрос: подставить вместо цифр значения столбцов enrollee_id и specialty_id
    # select A.subject_id, greatest(A.ege_result, A.inner_result)
    # from Achievment A
    # join Subject_Requirement SR on SR.subject_id = A.subject_id
    # where A.enrollee_id = 156187 and SR.specialty_id = 9

# 5) число направлений с одинаковым числом абитуриентов

select enrollees_count, count(enrollees_count) as specialty_count
from (select count(DISTINCT C.enrollee_id) enrollees_count
    from Choice C
    join Specialty S on C.specialty_id = S.specialty_id
    where C.priority_index = 2
    group by S.specialty_id) as counts
group by enrollees_count;

# 6) найти дисциплину по которой подано больше результатов чем у дисциплины A

# 4 сек

select A.subject_id, count(A.enrollee_id) as count
from Achievment A
group by A.subject_id
having count > (select count(A.enrollee_id)
from Achievment A
where A.subject_id = 23
group by A.subject_id);

# 7) найти абитуриентов которые не подавались в институт A

select E.last_name, E.first_name
from Enrollee E
where E.enrollee_id not in (select distinct C.enrollee_id
from Enrollee
join Choice C on C.enrollee_id = Enrollee.enrollee_id
left join Specialty S on C.specialty_id = S.specialty_id
where S.department_id = 2);

# 8) для всех институтов и всех дисциплин посчитать число абитуриентов

# 10 сек

with EnrolleeCount as (
    select 
        Sp.specialty_id, 
        count(distinct C.enrollee_id) as enrollee_count
    from 
        Specialty Sp
        left join Choice C on Sp.specialty_id = C.specialty_id
    group by 
        Sp.specialty_id
)
select 
    S.name as subject_name, 
    D.name as department_name, 
    coalesce(sum(EC.enrollee_count), 0) as enrollee_count
from 
    Subject S
    cross join Department D
    left join Specialty Sp on D.department_id = Sp.department_id
    left join Subject_Requirement SR on S.subject_id = SR.subject_id and Sp.specialty_id = SR.specialty_id
    left join EnrolleeCount EC on Sp.specialty_id = EC.specialty_id
group by 
    S.subject_id, 
    D.department_id, 
    S.name, 
    D.name;

select D.name as department_name, count(distinct specialty_id)
from Specialty S
join Department D on S.department_id = D.department_id
group by D.department_id;

select S.specialty_id, SR.subject_id
from Subject_Requirement SR
join Specialty S on SR.specialty_id = S.specialty_id
where department_id = 4
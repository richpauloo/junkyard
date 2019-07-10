# SQL Bolt

[Webpage here](https://sqlbolt.com/lesson/select_queries_introduction).  


## Lesson 1: SELECT queries  

`SELECT` statement = query.  
	* what to find  
	* where to find it  
	* optionally, how to transform it before it's returned  


Return all columns from a table:  

```
SELECT *  
FROM mytable;  
```

Query a few columns from a table:  

```
SELECT column, another_column, ...  
FROM mytable;  
```

***  

## Lesson 2: Queries with constraints (Pt. 1)

`WHERE` lets us apply logical filters to observations in a table.  

```
SELECT column, another_column, ...  
FROM mytable
WHERE condition
	AND/OR another_condition
	AND_OR ...;
```

Operators:  

```
=, !=, <, <=, >, >=
BETWEEN 1 AND 3  
NOT BETWEEN 1 AND 3
IN (2,4,6)
NOT IN (2,4,6)
```

***  

## Lesson 2: Queries with constraints (Pt. 2)

[More operators](https://sqlbolt.com/lesson/select_queries_with_constraints_pt_2):  

```
LIKE
NOT LIKE
% - Used anywhere in a string to match a sequence of zero or more characters (only with LIKE or NOT LIKE)
```

Example of `%`: `col_name LIKE "%AT%"` matches "AT", "ATTIC", "CAT" or even "BATS"  


***  

## Lesson 4: Filtering and sorting Query results

`DISTINCT` discards non-unique rows of a given column value, and keeps only the first distinct occurence:   

```
SELECT DISTINCT column, another_column, …
FROM mytable
WHERE condition(s);
```

`ORDER BY` arranges a column in ascending/descending (`ASC`/`DESC`) order:    

```
SELECT column, another_column, …
FROM mytable
WHERE condition(s)
ORDER BY column ASC/DESC;
```

`LIMIT` reduces the number of rows to return.  
`OFFSET` specifies where to begin counting rows from and follows a `LIMIT` command   


```
SELECT column, another_column, …
FROM mytable
WHERE condition(s)
ORDER BY column ASC/DESC;
LIMIT num_limit OFFSET num_offset;
```

***  

## SQL Lesson 6: Multi-table queries with JOINs

**Database normalization** splits data into mutliple tables to avoid duplicate information.  

**Multi-tabe queries with `JOIN`s** require a unique field in a table common among different tables.  

`INNER JOIN`:  

```
SELECT column_1, column_2, ...  
FROM mytable
INNER JOIN another_table
  ON mytable.id = another_table.id
WHERE condition(s)
ORDER BY column, ... ASC/DESC
LIMIT num_limit OFFSET num_offset;
```

***  

## SQL Lesson 7: OUTER JOINs

Depending on how you want to analyze the data, the `INNER JOIN` we used last lesson might not be sufficient because the resulting table only contains data that belongs in both of the tables.  

If the two tables have asymmetric data, which can easily happen when data is entered in different stages, then we would have to use a `LEFT JOIN`, `RIGHT JOIN` or `FULL JOIN` instead to ensure that the data you need is not left out of the results.  

```
SELECT column, another_column, …
FROM mytable
INNER/LEFT/RIGHT/FULL JOIN another_table 
    ON mytable.id = another_table.matching_id
WHERE condition(s)
ORDER BY column, … ASC/DESC
LIMIT num_limit OFFSET num_offset;
```

Like the `INNER JOIN` these three new joins have to specify which column to join the data on. When joining table A to table B, a `LEFT JOIN` simply includes rows from A regardless of whether a matching row is found in B. `The RIGHT JOIN` is the same, but reversed, keeping rows in B regardless of whether a match is found in A. Finally, a `FULL JOIN` simply means that rows from both tables are kept, regardless of whether a matching row exists in the other table.  


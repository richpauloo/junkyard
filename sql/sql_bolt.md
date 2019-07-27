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
SELECT DISTINCT column, another_column, ...
FROM mytable
WHERE condition(s);
```

`ORDER BY` arranges a column in ascending/descending (`ASC`/`DESC`) order:    

```
SELECT column, another_column, ...
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
SELECT column, another_column, ...
FROM mytable
INNER/LEFT/RIGHT/FULL JOIN another_table 
    ON mytable.id = another_table.matching_id
WHERE condition(s)
ORDER BY column, … ASC/DESC
LIMIT num_limit OFFSET num_offset;
```

Like the `INNER JOIN` these three new joins have to specify which column to join the data on. When joining table A to table B, a `LEFT JOIN` simply includes rows from A regardless of whether a matching row is found in B. `The RIGHT JOIN` is the same, but reversed, keeping rows in B regardless of whether a match is found in A. Finally, a `FULL JOIN` simply means that rows from both tables are kept, regardless of whether a matching row exists in the other table.  

When using any of these new joins, you will likely have to write additional logic to deal with `NULL`s in the result and constraints (more on this in the next lesson).  


***  

## SQL Lesson 8: A short note on NULLs

An alternative to NULL values in your database is to have data-type appropriate default values, like 0 for numerical data, empty strings for text data, etc. But if your database needs to store incomplete data, then NULL values can be appropriate if the default values will skew later analysis (for example, when taking averages of numerical data).  

Sometimes, it's also not possible to avoid NULL values, as we saw in the last lesson when outer-joining two tables with asymmetric data. In these cases, you can test a column for NULL values in a WHERE clause by using either the IS NULL or IS NOT NULL constraint.  

```
SELECT column, another_column, ...
FROM mytable
WHERE column IS/IS NOT NULL
AND/OR another_condition
AND/OR …;
```


***  


## SQL Lesson 9: Queries with expressions

In addition to querying and referencing raw column data with SQL, you can also use expressions to write more complex logic on column values in a query. These expressions can use mathematical and string functions along with basic arithmetic to transform values when the query is executed, as shown in this physics example.  

```
SELECT particle_speed / 2.0 AS half_particle_speed
FROM physics_data
WHERE ABS(particle_position) * 10.0 > 500;
```

Each database has its own supported set of mathematical, string, and date functions that can be used in a query, which you can find in their own respective docs.  

The use of expressions can save time and extra post-processing of the result data, but can also make the query harder to read, so we recommend that when expressions are used in the SELECT part of the query, that they are also given a descriptive alias using the AS keyword.  

``` 
SELECT col_expression AS expr_description, ...
FROM mytable;
```

In addition to expressions, regular columns and even tables can also have aliases to make them easier to reference in the output and as a part of simplifying more complex queries.  

```
SELECT column AS better_column_name, ...
FROM a_long_widgets_table_name AS mywidgets
INNER JOIN widget_sales
  ON mywidgets.id = widget_sales.widget_id;
```


***  


## SQL Lesson 10: Queries with aggregates (Pt. 1)
In addition to the simple expressions that we introduced last lesson, SQL also supports the use of aggregate expressions (or functions) that allow you to summarize information about a group of rows of data. With the Pixar database that you've been using, aggregate functions can be used to answer questions like, "How many movies has Pixar produced?", or "What is the highest grossing Pixar film each year?".  

```
SELECT AGG_FUNC(column_or_expression) AS aggregate_description, ...
FROM mytable
WHERE constraint_expression;
```

Without a specified grouping, each aggregate function is going to run on the whole set of result rows and return a single value. And like normal expressions, giving your aggregate functions an alias ensures that the results will be easier to read and process.  

Common aggregate functions:  

* COUNT() 
* MIN()
* MAX()
* AVG()
* SUM()

Grouped aggregate functions:  

In addition to aggregating across all the rows, you can instead apply the aggregate functions to individual groups of data within that group (ie. box office sales for Comedies vs Action movies).
This would then create as many results as there are unique groups defined as by the GROUP BY clause.  

```
SELECT AGG_FUNC(column_or_expression) AS aggregate_description, ...
FROM mytable
WHERE constraint_expression
GROUP BY column;
```


***  

## SQL Lesson 11: Queries with aggregates (Pt. 2)

Our queries are getting fairly complex, but we have nearly introduced all the important parts of a  SELECT query. One thing that you might have noticed is that if the GROUP BY clause is executed after the WHERE clause (which filters the rows which are to be grouped), then how exactly do we filter the grouped rows?  

Luckily, SQL allows us to do this by adding an additional HAVING clause which is used specifically with the  GROUP BY clause to allow us to filter grouped rows from the result set.  

```
SELECT group_by_column, AGG_FUNC(column_expression) AS aggregate_result_alias, …
FROM mytable
WHERE condition
GROUP BY column
HAVING group_condition;
```

The HAVING clause constraints are written the same way as the WHERE clause constraints, and are applied to the grouped rows. With our examples, this might not seem like a particularly useful construct, but if you imagine data with millions of rows with different properties, being able to apply additional constraints is often necessary to quickly make sense of the data.


***  

## SQL Lesson 12: Order of execution of a Query

Now that we have an idea of all the parts of a query, we can now talk about how they all fit together in the context of a complete query.  

```
SELECT DISTINCT column, AGG_FUNC(column_or_expression), ...
FROM mytable
    JOIN another_table
      ON mytable.column = another_table.column
    WHERE constraint_expression
    GROUP BY column
    HAVING constraint_expression
    ORDER BY column ASC/DESC
    LIMIT count OFFSET COUNT;
```

Each query begins with finding the data that we need in a database, and then filtering that data down into something that can be processed and understood as quickly as possible. Because each part of the query is executed sequentially, it's important to understand the order of execution so that you know what results are accessible where.

Query order of execution:  

1. FROM and JOINs  

The FROM clause, and subsequent JOINs are first executed to determine the total working set of data that is being queried. This includes subqueries in this clause, and can cause temporary tables to be created under the hood containing all the columns and rows of the tables being joined.

2. WHERE  

Once we have the total working set of data, the first-pass WHERE constraints are applied to the individual rows, and rows that do not satisfy the constraint are discarded. Each of the constraints can only access columns directly from the tables requested in the FROM clause. Aliases in the SELECT part of the query are not accessible in most databases since they may include expressions dependent on parts of the query that have not yet executed.

3. GROUP BY  

The remaining rows after the WHERE constraints are applied are then grouped based on common values in the column specified in the GROUP BY clause. As a result of the grouping, there will only be as many rows as there are unique values in that column. Implicitly, this means that you should only need to use this when you have aggregate functions in your query.

4. HAVING  

If the query has a GROUP BY clause, then the constraints in the HAVING clause are then applied to the grouped rows, discard the grouped rows that don't satisfy the constraint. Like the WHERE clause, aliases are also not accessible from this step in most databases.

5. SELECT  

Any expressions in the SELECT part of the query are finally computed.

6. DISTINCT  

Of the remaining rows, rows with duplicate values in the column marked as DISTINCT will be discarded.

7. ORDER BY  

If an order is specified by the ORDER BY clause, the rows are then sorted by the specified data in either ascending or descending order. Since all the expressions in the SELECT part of the query have been computed, you can reference aliases in this clause.

8. LIMIT / OFFSET  

Finally, the rows that fall outside the range specified by the LIMIT and OFFSET are discarded, leaving the final set of rows to be returned from the query.



***  

## SQL Lesson 13: Inserting rows

We've spent quite a few lessons on how to query for data in a database, so it's time to start learning a bit about SQL schemas and how to add new data.
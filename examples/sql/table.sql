SELECT
  table_schema AS 'schema'
, table_name AS 'table'
, table_type AS 'type'
, engine AS 'engine'
, table_collation AS 'collation'
FROM  information_schema.tables
WHERE table_schema NOT IN ('mysql','information_schema', 'performance_schema', 'ndbinfo', 'sys')
ORDER BY
  table_schema
, table_name
;

SELECT
  table_schema AS 'schema'
, table_name AS 'table'
, partition_name AS 'partition'
, nodegroup AS 'nodegroup'
, table_rows AS 'rows'
FROM  information_schema.partitions
WHERE table_schema NOT IN ('mysql','information_schema', 'performance_schema', 'ndbinfo', 'sys')
ORDER BY
  table_schema
, table_name
;

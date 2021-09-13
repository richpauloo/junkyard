# Convert spatial data with GDAL

1. install Homebrew if you don't have it. 

2. Install GDAL 

3. make it discoverable: `export PATH=/Library/Frameworks/GDAL.framework/Programs:$PATH`

4. verify with `gdalinfo --version` 

5. download postrges and make `postgres` user: `/usr/local/opt/postgres/bin/createuser -s postgres`

6. create db for local user: `createdb` makes a database for the local user (in my case, named "richpauloo"). Verify with `createdb <user>` (e.g., `createdb richpauloo`) and you'll get a message saying the database already exists.

7. test on 3 GB of downloaded vector building footprint data: `createdb ca` followed by `ogr2ogr -f "PostgreSQL" -a_srs "EPSG:4326 PG:"dbname=ca user=richpauloo" "California.geojson"`. This takes a while. 

8. query postgresql database with dbeaver


# using psql

Create db:

`createdb <my_db>`

connect to a database

`psql <my_db>`

See a prompt like: `my_db=#`, where `=#` means you're a superuser without restrictions  




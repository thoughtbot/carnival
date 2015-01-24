ALTER TABLE "user" ADD COLUMN name varchar;
UPDATE "user" SET name = first_name || ' ' || last_name;
ALTER TABLE "user" ALTER COLUMN name SET NOT NULL;
ALTER TABLE "user" DROP COLUMN first_name;
ALTER TABLE "user" DROP COLUMN last_name;

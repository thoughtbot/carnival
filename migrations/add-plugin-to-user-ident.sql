ALTER TABLE "user" ADD COLUMN plugin varchar;
UPDATE "user" SET plugin = 'learn';
ALTER TABLE "user" ALTER COLUMN plugin SET NOT NULL;

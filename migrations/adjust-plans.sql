ALTER TABLE PLAN
  RENAME branded to commercial;

UPDATE PLAN
  SET commercial = false,
      comment_quota = 100
  WHERE name = 'personal';

UPDATE PLAN
  SET site_quota = 1,
      commercial = true,
      comment_quota = 1000
  WHERE name = 'business';

ALTER TABLE comment
  ADD COLUMN article_title varchar
  NOT NULL DEFAULT '';

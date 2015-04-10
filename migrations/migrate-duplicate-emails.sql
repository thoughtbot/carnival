--------------------------------------------------------------------------------
-- For each relation, find all records tied to a learn user that also have a
-- github account with the same email, re-associate the records with the github
-- account
--------------------------------------------------------------------------------
UPDATE membership
  SET "user" = guser.id
  FROM "user" luser
  JOIN "user" guser
    ON guser.email = luser.email
  WHERE luser.id = membership.user
    AND guser.plugin = 'github'
    AND luser.plugin = 'learn';

UPDATE comment
  SET "user" = guser.id
  FROM "user" luser
  JOIN "user" guser
    ON guser.email = luser.email
  WHERE luser.id = comment.user
    AND guser.plugin = 'github'
    AND luser.plugin = 'learn';

UPDATE subscription
  SET "user" = guser.id
  FROM "user" luser
  JOIN "user" guser
    ON guser.email = luser.email
  WHERE luser.id = subscription.user
    AND guser.plugin = 'github'
    AND luser.plugin = 'learn';

--------------------------------------------------------------------------------
-- Delete all learn accounts that have no related data. All that should remain
-- are learn accounts with data, but no associated github account.
--------------------------------------------------------------------------------
DELETE
  FROM "user"
  WHERE "user"."id" IN (
    SELECT "user"."id"
    FROM "user"
    LEFT OUTER JOIN membership
      ON membership.user = "user".id
    LEFT OUTER JOIN comment
      ON comment.user = "user".id
    LEFT OUTER JOIN subscription
      ON subscription.user = "user".id
    WHERE membership.id IS NULL
      AND comment.id IS NULL
      AND subscription.id IS NULL
  );

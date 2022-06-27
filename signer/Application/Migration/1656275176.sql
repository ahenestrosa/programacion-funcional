ALTER TABLE keys RENAME TO "key";
ALTER TABLE "key" ADD CONSTRAINT key_date_key UNIQUE(date);

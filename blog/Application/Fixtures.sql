

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.posts DISABLE TRIGGER ALL;

INSERT INTO public.posts (id, title, body) VALUES ('8092809c-6eb3-456d-9c38-61ce15130026', 'Mi primer post', 'uwu');
INSERT INTO public.posts (id, title, body) VALUES ('8f010c0a-1330-4bdc-bcaf-b64b38272107', 'Second post', 'asdf');


ALTER TABLE public.posts ENABLE TRIGGER ALL;


ALTER TABLE public.schema_migrations DISABLE TRIGGER ALL;

INSERT INTO public.schema_migrations (revision) VALUES (1654363437);


ALTER TABLE public.schema_migrations ENABLE TRIGGER ALL;



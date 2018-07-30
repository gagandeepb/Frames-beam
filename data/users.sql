--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.13
-- Dumped by pg_dump version 10.4

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: cart_users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.cart_users (
    email character varying NOT NULL,
    first_name character varying NOT NULL,
    last_name character varying NOT NULL,
    is_member boolean NOT NULL,
    days_in_queue integer NOT NULL
);


--
-- Data for Name: cart_users; Type: TABLE DATA; Schema: public; Owner: -
--

COPY public.cart_users (email, first_name, last_name, is_member, days_in_queue) FROM stdin;
james@example.com	James	Smith	t	1
betty@example.com	Betty	Jones	f	42
james@pallo.com	James	Pallo	t	1
betty@sims.com	Betty	Sims	f	42
james@oreily.com	James	O'Reily	t	1
sam@sophitz.com	Sam	Sophitz	f	42
sam@jely.com	Sam	Jely	t	1
sam@example.com	Sam	Taylor	f	42
\.


--
-- Name: cart_users cart_users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cart_users
    ADD CONSTRAINT cart_users_pkey PRIMARY KEY (email);


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: -
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--


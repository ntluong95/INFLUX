/*
* Note for SQLServer users, the TIMESTAMP field need to be changed to DATETIME type
*/
-- t_datatypes
CREATE TABLE t_datatypes (
	dtcode CHAR(3) PRIMARY KEY,
	libtype VARCHAR(128)
);

-- t_langs
CREATE TABLE t_langs (
  codelang CHAR(2) PRIMARY KEY,
  language VARCHAR(128)
);

-- t_countries
CREATE TABLE t_countries (
  isocountry CHAR(2) PRIMARY KEY,
  country VARCHAR(128)
);

-- t_codes
CREATE TABLE t_codes (
	codeid INTEGER PRIMARY KEY,
	eppocode VARCHAR(10) NULL,
	dtcode CHAR(3), 
	status CHAR(1),
	c_date TIMESTAMP NULL,
	m_date TIMESTAMP NULL
);


-- t_authorities
CREATE TABLE t_authorities (
	idauth INTEGER PRIMARY KEY,
	authdesc VARCHAR(250) NOT NULL
);

-- t_names
CREATE TABLE t_names (
	nameid INTEGER PRIMARY KEY,
	codeid INTEGER NOT NULL,
	fullname VARCHAR(250) NULL,
	codelang CHAR(2),
	isocountry CHAR(2) NULL,
	preferred integer,
	idauth INTEGER NULL,
	status CHAR(1) NOT NULL,
	c_date TIMESTAMP NULL,
	m_date TIMESTAMP NULL
);

-- t_links
CREATE TABLE t_links (
	idlink INTEGER PRIMARY KEY,
	codeid INTEGER NOT NULL,
	codeid_parent INTEGER NOT NULL,
	status CHAR(1) NOT NULL,
	c_date TIMESTAMP NULL,
	m_date TIMESTAMP NULL
);

-- ALTER TABLE t_codes ADD CONSTRAINT t_codes_dtcode_fkey FOREIGN KEY (dtcode) REFERENCES t_datatypes (dtcode) ON UPDATE RESTRICT ON DELETE RESTRICT;
-- ALTER TABLE t_links ADD CONSTRAINT t_links_codeid_fkey FOREIGN KEY (codeid) REFERENCES t_codes (codeid) ON UPDATE RESTRICT ON DELETE RESTRICT;
-- ALTER TABLE t_links ADD CONSTRAINT t_links_codeid_parent_fkey FOREIGN KEY (codeid_parent) REFERENCES t_codes (codeid) ON UPDATE RESTRICT ON DELETE RESTRICT;
-- ALTER TABLE t_names ADD CONSTRAINT t_names_codelang_fkey FOREIGN KEY (codelang)  REFERENCES t_langs (codelang) ON UPDATE RESTRICT ON DELETE RESTRICT;
-- ALTER TABLE t_names ADD CONSTRAINT t_names_idauth_fkey FOREIGN KEY (idauth) REFERENCES t_authorities (idauth) ON UPDATE RESTRICT ON DELETE RESTRICT;
-- ALTER TABLE t_names ADD CONSTRAINT t_names_isocountry_fkey FOREIGN KEY (isocountry) REFERENCES t_countries (isocountry) ON UPDATE RESTRICT ON DELETE RESTRICT;












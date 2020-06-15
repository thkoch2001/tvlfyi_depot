CREATE TABLE keywords (
	id SERIAL PRIMARY KEY,
	name VARCHAR UNIQUE NOT NULL,
	chan VARCHAR NOT NULL,
	UNIQUE(name, chan)
);

CREATE TABLE entries (
	id SERIAL PRIMARY KEY,
	keyword_id INT NOT NULL REFERENCES keywords ON DELETE CASCADE,
	idx INT NOT NULL,
	text VARCHAR NOT NULL,
	creation_ts TIMESTAMP NOT NULL,
	created_by VARCHAR NOT NULL
);

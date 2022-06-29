-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE "key" (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    pem TEXT NOT NULL,
    date DATE NOT NULL UNIQUE
);
CREATE TABLE pub_keys (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    pem TEXT NOT NULL,
    date DATE NOT NULL
);

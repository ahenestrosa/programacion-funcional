CREATE TABLE keys (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    pem TEXT NOT NULL,
    date DATE NOT NULL
);

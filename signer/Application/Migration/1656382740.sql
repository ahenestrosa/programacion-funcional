CREATE TABLE verification_messages (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    text TEXT NOT NULL,
    signature TEXT NOT NULL,
    date DATE NOT NULL
);

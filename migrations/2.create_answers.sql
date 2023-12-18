create table if not exists answers (
    aid text primary key, 
    description text,
    atype text not null,
    qid text
);

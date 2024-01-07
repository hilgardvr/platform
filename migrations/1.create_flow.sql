create table if not exists flow (
    id text primary key not null,
    product text not null,
    qid text not null,
    question_description text not null,
    aid text not null,
    answer text not null,
    status text not null
);

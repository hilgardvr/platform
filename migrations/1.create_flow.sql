create table if not exists flow (
    id integer primary key autoincrement,
    product text not null,
    qid text not null,
    question_description text not null,
    aid text not null,
    answer text not null,
    answer_mapping text,
    status text not null,
    sess text not null
);

create table if not exists question_answers (
    qaid text not null primary key,
    qid text not null,
    aid text not null
)

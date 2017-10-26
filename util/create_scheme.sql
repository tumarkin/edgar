create database edgar;
\c edgar

create table forms (
  id             serial primary key,
  cik            text,
  company_name   text,
  form_type      text,
  date_filed     date,
  filename       text,

  unique (cik, company_name, form_type, date_filed, filename)
);

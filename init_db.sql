create type location as
  ( latlng      circle
  , timepoint   time
  , str_num     integer
  , str_name    text
  , city        text
  , state       text
  , zip         char(5)
  );

create type reply as
  ( poster      varchar(32)
  , posted      timestamp
  , content     text
  );

create table users
  ( id          varchar(32)   primary key
  , email       varchar(320)  unique
  , phone       char(10)      unique
  , birthday    date
  , sex         boolean
  , password    varchar(32)
  , about       text
  , upvotes     integer
  , downvotes   integer
  , locations   location[]
  , replies     reply[]
  );

create index user_id on users (id);

create table wants
  ( id          serial        primary key
  , poster      varchar(32)   references users(id) on delete cascade
  , keywords    text
  , details     text
  , highprice   money
  , lowprice    money
  , replies     reply[]
  , unique (poster, keywords)
  );

create index want_id on wants (id);

create table haves
  ( id          serial        primary key
  , poster      varchar(32)   references users(id) on delete cascade
  , title       text
  , description text
  , price       money
  , condition   text
  , replies     reply[]
  , unique (poster, title)
  );

create index have_id on haves (id);

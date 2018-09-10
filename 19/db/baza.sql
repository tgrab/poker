drop database poker;
create database poker;

use poker;

create table crypto(
	id bigint,
      czas bigint,
      table_name varchar(20),
      kind smallint,
      blind float,
      table_size smallint,
      name2seat varchar(200),
      cards varchar(30),
      table_cards varchar(30),
      cards_log varchar(120),
      history_log varchar(400),
      money_log varchar(200),
      primary key(id) );

create table games_list(
	id bigint auto_increment,
      start_period bigint,
	end_period bigint,
	casino varchar(10),
	table_name varchar(20),
	primary key(id) );











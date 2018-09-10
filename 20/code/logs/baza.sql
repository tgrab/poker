drop database poker;
create database poker;

use poker;

create table crypto(
      id bigint,
      czas bigint,
      table_name varchar(20),
      kind smallint,
      players varchar(350),
      history varchar(700),
      log varchar(250),      
      primary key(id));

create table games_list(
	nr bigint auto_increment,
	casino varchar(10),	
	table_name varchar(20),
      start_period bigint,
	end_period bigint,
	primary key(nr));











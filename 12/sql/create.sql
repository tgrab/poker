CREATE TABLE players ( 
    nr serial PRIMARY KEY ,
    name text);
    
CREATE TABLE comments (
    player_nr integer,
    czas timestamp,
    comment text);
    
CREATE TABLE table_descr (
    nr serial PRIMARY KEY,
    name text,
    table_size smallint,
    small_blind float,
    big_blind float,
    game_kind smallint);   
    
CREATE TABLE games (
    id character(10) PRIMARY KEY,
    czas time,
    data date,
    table_nr integer,
    cards text,
    history text,
    button smallint,
    player_nrs integer[],
    player_desc text)
    
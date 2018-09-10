CREATE TABLE players ( 
    nr serial PRIMARY KEY ,
    name text);
    
CREATE TABLE comments (
    player_nr integer,
    czas timestamp DEFAULT CURRENT_TIMESTAMP,
    comment text);
    
CREATE TABLE table_descr (
    nr serial PRIMARY KEY,
    name text,
    table_size smallint,
    small_blind float,
    big_blind float,
    game_kind smallint);   
    
CREATE TABLE games (
    nr serial PRIMARY KEY,
    id character(10),
    czas timestamp,
    table_nr integer,
    cards text,
    history text,
    button smallint,
    pot float);

CREATE TABLE player_log (
    player_nr integer,
    game_nr integer,
    pos smallint,
    cards text,
    money float,
    balance float);
    
CREATE INDEX player_log_idx ON player_log (player_nr);
CREATE INDEX player_log_game_idx ON player_log (game_nr);
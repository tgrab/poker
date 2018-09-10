CREATE TABLE players ( 
    nr serial PRIMARY KEY ,
    name text);
    
CREATE TABLE comments (
    player_nr integer,
    czas timestamp DEFAULT CURRENT_TIMESTAMP,
    comment text);
    
CREATE TABLE games (
    id integer PRIMARY KEY,
    czas timestamp,
    game_kind smallint,
    table_name text,
    table_cards text,
    blind float,
    players text,
    history text);

CREATE TABLE player_log (
    player_nr integer,
    game_id integer,
    game_kind smallint,
    blind float,
    czas timestamp);
    
CREATE INDEX player_log_idx ON player_log (player_nr);
CREATE INDEX player_log_game_idx ON player_log (czas);
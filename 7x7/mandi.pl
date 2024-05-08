% Define musicians
musician(freddy_mercury).
musician(dave_lombardo).
musician(jeff_beck).
musician(phil_lynott).
musician(gary_moore).
musician(pete_way).
musician(micheal_schenker).
musician(rob_halford).
musician(les_binks).
musician(marty_friedman).
musician(steve_harris).
musician(johnny_marr).
musician(sister_bliss).
musician(keith_emerson).
musician(cozy_powell).
musician(brian_robertson).
musician(nick_cave).
musician(miles_davis).
musician(rat_scabies).
musician(steve_jones).

% Define instruments
instrument(vocals, freddy_mercury).
instrument(drums, dave_lombardo).
instrument(guitar, jeff_beck).
instrument(bass, phil_lynott).
instrument(guitar, gary_moore).
instrument(bass, pete_way).
instrument(guitar, micheal_schenker).
instrument(vocals, rob_halford).
instrument(drums, les_binks).
instrument(guitar, marty_friedman).
instrument(bass, steve_harris).
instrument(guitar, johnny_marr).
instrument(keyboards, sister_bliss).
instrument(keyboards, keith_emerson).
instrument(drums, cozy_powell).
instrument(guitar, brian_robertson).
instrument(vocals, nick_cave).
instrument(trumpet, miles_davis).
instrument(drums, rat_scabies).
instrument(guitar, steve_jones).

% Define genres
genre(rock, freddy_mercury).
genre(thrash_metal, dave_lombardo).
genre(blues_rock, jeff_beck).
genre(rock, phil_lynott).
genre(blues, gary_moore).
genre(hard_rock, pete_way).
genre(hard_rock, micheal_schenker).
genre(heavy_metal, rob_halford).
genre(heavy_metal, les_binks).
genre(heavy_metal, marty_friedman).
genre(heavy_metal, steve_harris).
genre(indie_rock, johnny_marr).
genre(electronic_dance, sister_bliss).
genre(progressive_rock, keith_emerson).
genre(hard_rock, cozy_powell).
genre(blues_rock, brian_robertson).
genre(alternative_rock, nick_cave).
genre(jazz, miles_davis).
genre(punk_rock, rat_scabies).
genre(punk_rock, steve_jones).

% Rule to find all musicians who play a particular instrument
musicians_by_instrument(Instrument, Musician) :-
    instrument(Instrument, Musician).

% Rule to find all musicians of a particular genre
musicians_of_genre(Genre, Musician) :-
    genre(Genre, Musician).

% Rule to find which instrument a musician plays
instrument_played_by(Musician, Instrument) :-
    instrument(Instrument, Musician).

% Rule to find the genre of music a musician is associated with
genre_of_musician(Musician, Genre) :-
    genre(Genre, Musician).

% Rule to list all unique genres
list_genres(Genre) :-
    setof(G, M^genre(G, M), Genres),
    member(Genre, Genres).
